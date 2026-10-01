# Exercise the actual Shiny state and the standalone exported calculation.
env <- new.env(parent=globalenv()); sys.source("app.R", env)
captured <- new.env()
check <- function(x, label) { if (!isTRUE(x)) stop(label); cat("PASS:", label, "\n") }
shiny::testServer(env$server, {
  session$setInputs(source_mode="demo", focal=c("Happy","Trust","Fair"), boundary="community",
    result_scope="local", plot_metric="abs_t", multiplicity=FALSE, orientation_mode="documented",
    question="GSS comparison", population="GSS", analysis_unit="Year", network_unit="Respondent",
    rationale="Example", timing="Retrospective reconstruction", record="", interpretation="")
  session$setInputs(calculate_reference=1)
  check(is.null(current_reference()) && !is.null(rv$reference_error), "GSS keeps its descriptive comparison without a reference p")
  check(all(c("Health","Life") %in% current_rank_pattern()$stronger_unreported), "GSS identifies stronger unreported neighbors")
  captured$demo <- payload()
  session$setInputs(source_mode="upload", predictor="X", candidates=sprintf("Item%02d",1:12),
    covariates=character(), own_lag=FALSE, lag_suffix="Lag", missing="common", se_method="classical",
    min_n=10, max_missing=.2, bootstrap=FALSE, boot_iter="100", seed=20260907)
  path <- normalizePath("examples/upload_example.csv")
  session$setInputs(data_csv=data.frame(name=basename(path),size=file.info(path)$size,type="text/csv",datapath=path))
  key_path <- tempfile(fileext=".csv")
  write.csv(data.frame(item="Item01",orientation_multiplier=-1,high_value_means="Reversed Item01 fixture",orientation_source="Software fixture definition"),key_path,row.names=FALSE)
  session$setInputs(metadata_csv=data.frame(name="key.csv",size=file.info(key_path)$size,type="text/csv",datapath=key_path))
  session$setInputs(predictor="X", candidates=sprintf("Item%02d",1:12), focal=c("Item01","Item02","Item03"),
    covariates=character(), run=1L, question="Software fixture")
  if (!is.null(rv$error)) stop(rv$error)
  unchanged <- result()$landscape
  session$setInputs(reference_family=sprintf("Item%02d",1:12), reference_selected=c("Item01","Item02"),
    reference_statistic="abs_t", reference_condition="uniform", reference_claim="Fixture common claim",
    reference_justification="Fixture equal-subset reporting rule", reference_design="Fixture declared before computation",
    reference_independent=FALSE, calculate_reference=2L)
  check(is.null(current_reference()) && grepl("Confirm", rv$reference_error), "independent specification is required")
  session$setInputs(reference_independent=TRUE, reference_condition="", calculate_reference=3L)
  check(is.null(current_reference()) && !is.null(rv$reference_error), "a reporting/symmetry condition is required")
  session$setInputs(reference_condition="uniform", calculate_reference=4L)
  check(!is.null(current_reference()) && current_reference()$result$m==2L, "reported subset can differ from the three focal outcomes")
  check(current_reference()$result$k==12L && current_reference()$result$n_subsets==66, "explicit comparison set is fully enumerated")
  check(grepl("researcher declaration", paste(report_text(),collapse=" ")), "report retains researcher justification and condition")
  captured$reference <- payload()
  session$setInputs(reference_justification="Changed justification")
  check(is.null(current_reference()) && is.null(payload()$concentration_reference), "changed justification removes stale exported p")
  captured$stale <- payload()
  session$setInputs(reference_condition="symmetry", reference_statistic="negative_t", calculate_reference=5L)
  check(current_reference()$condition=="symmetry" && current_reference()$statistic=="negative_t", "fixed-label symmetry and declared negative direction are recorded")
  session$setInputs(reference_family=sprintf("Item%02d",1:10))
  check(is.null(current_reference()), "changed comparison set invalidates the calculation")
  session$setInputs(calculate_reference=6L)
  check(current_reference()$result$k==10L, "new declared set is recomputed explicitly")
  session$setInputs(reference_independent=FALSE)
  check(is.null(current_reference()), "withdrawing independent-specification declaration invalidates p")
  session$setInputs(reference_independent=TRUE, calculate_reference=7L)
  session$setInputs(boundary="ring15")
  check(is.null(current_reference()), "changed boundary requires renewed calculation")
  session$setInputs(calculate_reference=8L)
  session$setInputs(focal="Item03")
  check(is.null(current_reference()), "changed focal set invalidates p")
  session$setInputs(calculate_reference=9L)
  check(grepl("current focal set", rv$reference_error), "reported subset must belong to the current focal set")
  session$setInputs(focal=c("Item01","Item02","Item03"), calculate_reference=10L)
  session$setInputs(orientation_mode="original")
  check(is.null(current_reference()), "changed semantic direction invalidates the signed reference")
  session$setInputs(calculate_reference=11L)
  check(current_reference()$statistics$statistic[1]== -result()$landscape$t[match("Item01",result()$landscape$item)], "signed reference uses the currently displayed direction")
  session$setInputs(clear_reference=1L)
  check(is.null(current_reference()), "clear removes the reference")
  check(identical(result()$landscape,unchanged), "reference choices leave fitted estimates unchanged")
  unlink(key_path)
})
root <- tempfile("reference-export-"); dir.create(root)
for (case in c("demo","reference","stale")) {
  z <- file.path(root,paste0(case,".zip")); env$export_bundle(z,captured[[case]])
  entries <- unzip(z,list=TRUE)$Name
  expected <- c("reference.json","reference_statistics.csv","reference_distribution.csv","R/concentration.R","reproduce_reference.R")
  check(if(case=="reference") all(expected %in% entries) else !any(expected %in% entries), paste(case,"export contains only its current reference state"))
  if (case=="reference") {
    d <- file.path(root,"unpacked"); dir.create(d); unzip(z,exdir=d)
    saved <- jsonlite::read_json(file.path(d,"reference.json"),simplifyVector=TRUE)
    check(saved$result$m==2 && identical(saved$selected,c("Item01","Item02")), "export preserves claim-specific reported subset")
    previous <- getwd(); setwd(d)
    out <- system2(file.path(R.home("bin"),"Rscript"),c("--vanilla","reproduce_reference.R"),stdout=TRUE,stderr=TRUE)
    setwd(previous)
    if (!is.null(attr(out,"status"))) stop(paste(out,collapse="\n"))
    check(any(grepl("PASS",out)), "standalone exported reference reproduces complete exact distribution")
  }
}
unlink(root,recursive=TRUE)
