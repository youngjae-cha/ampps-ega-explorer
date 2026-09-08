# Run from the app folder: Rscript tests/test_orientation.R
source("R/model.R")
source("R/export.R")
d <- load_demo_data("data")
items <- d$metadata$item
raw <- fit_landscape(d$data,items,"MobilityLag",own_lag=TRUE)
out <- orient_landscape(raw,d$orientation)
flipped <- c("Happy","Hapmar","Health","Life","Satjob","News")
idx <- out$item %in% flipped
stopifnot(identical(out$item[out$orientation_reversed], items[items %in% flipped]),
  identical(out$b,raw$b), identical(out$t,raw$t), identical(out$p,raw$p), identical(out$se,raw$se),
  identical(abs(out$oriented_t),abs(raw$t)),
  isTRUE(all.equal(out$oriented_b[idx],-raw$b[idx])),
  isTRUE(all.equal(out$oriented_ci_lo[idx],-raw$ci_hi[idx])),
  isTRUE(all.equal(out$oriented_ci_hi[idx],-raw$ci_lo[idx])),
  all(out$oriented_ci_lo<=out$oriented_ci_hi),
  all(out$orientation_multiplier[match(c("Trust","Fair","Helpful","Finalter"),out$item)]==1),
  all(startsWith(out$orientation_status[match(c("Trust","Fair","Helpful","Finalter"),out$item)],"raw_")))
reports <- make_report_tables(raw,c("Happy","Trust","Fair"),items,d$metadata,TRUE,d$orientation)
base <- make_report_tables(raw,c("Happy","Trust","Fair"),items,d$metadata,TRUE)
stopifnot(identical(reports$all$observed_abs_t_rank_universe,base$all$observed_abs_t_rank_universe),
  identical(reports$all$p_bonferroni_universe,base$all$p_bonferroni_universe))
shown <- display_oriented_estimates(reports$all)
stopifnot(shown$b[shown$item=="Happy"]>0, shown$raw_b[shown$item=="Happy"]<0)
fails <- function(expr) inherits(tryCatch(force(expr),error=function(e)e),"error")
stopifnot(fails(normalize_orientation_key(c(Happy=0),items)),
  fails(normalize_orientation_key(c(Happy=-1,Happy=1),items)))
bad <- d$orientation; bad$status[1] <- "raw_unverified"; bad$multiplier[1] <- -1
stopifnot(fails(normalize_orientation_key(bad,items)))
meta <- data.frame(item="Happy",orientation_multiplier=-1,high_value_means="Declared increase",orientation_source="User codebook")
user_key <- orientation_from_metadata(meta,items)
stopifnot(user_key$status[user_key$item=="Happy"]=="user_declared",
  user_key$multiplier[user_key$item=="Happy"]==-1,
  fails(orientation_from_metadata(meta[c("item","orientation_multiplier")],items)))

work <- tempfile("orientation_export_"); dir.create(work)
payload <- list(landscape=raw,report=reports,orientation=d$orientation,documented_orientation_key=d$orientation,
  settings=list(model=attr(raw,"model_settings")), reporting_text="Orientation software test")
archive <- file.path(work,"test.zip")
export_bundle(archive,payload,c(analysis_input.csv="data/gss_year.csv"))
ex <- file.path(work,"ex"); dir.create(ex); unzip(archive,exdir=ex)
stopifnot(all(c("orientation_key.csv","documented_orientation_key.csv") %in% list.files(ex)))
old <- getwd(); setwd(ex)
test <- system2(file.path(R.home("bin"),"Rscript"),"reproduce.R",stdout=TRUE,stderr=TRUE)
setwd(old)
if (!is.null(attr(test,"status"))) cat(test,sep="\n")
stopifnot(is.null(attr(test,"status")),file.exists(file.path(ex,"landscape_oriented_reproduced.csv")))
reproduced <- read.csv(file.path(ex,"landscape_oriented_reproduced.csv"))
stopifnot(isTRUE(all.equal(reproduced$oriented_b,out$oriented_b,tolerance=1e-12)))
unlink(work,recursive=TRUE)
cat("PASS: verified-subset orientation, raw preservation, CI endpoint reversal, unchanged |t|/p, invalid-key rejection, upload declarations, and oriented export reproduction.\n")
