# Deterministic software-practice fixture, not scientific validation evidence.
# From any directory: Rscript path/to/examples/make_upload_example.R
args <- commandArgs(trailingOnly=FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)][1L])
outdir <- normalizePath(dirname(script),mustWork=TRUE)
set.seed(20260907L)
n <- 200L
X <- rnorm(n)
factors <- matrix(rnorm(n*2L),n,2L)
items <- sapply(seq_len(12L),function(j) {
  block <- if (j<=6L) 1L else 2L
  beta <- if (block==1L) .20 else .05
  beta*X + .75*factors[,block] + .65*rnorm(n)
})
colnames(items)<-sprintf("Item%02d",seq_len(12L))
data <- data.frame(X=X,items,check.names=FALSE)
write.csv(data,file.path(outdir,"upload_example.csv"),row.names=FALSE,na="")
metadata <- data.frame(item=colnames(items),
  label=paste("Simulated item",seq_len(12L)),
  domain=rep(c("Artificial generating block A","Artificial generating block B"),each=6L),
  respondent_scope="Same 200 simulated rows; no population interpretation",
  coding_note="Artificial continuous score; not a validated construct or scale",
  stringsAsFactors=FALSE)
write.csv(metadata,file.path(outdir,"metadata_example.csv"),row.names=FALSE,na="")
cat("Created upload_example.csv (200 rows; X plus12 numeric items) and metadata_example.csv.\n")
cat("Software demonstration only: no scientific validation or selection-process labels.\n")
