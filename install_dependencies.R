# Explicit installation only; restores the committed dependency graph locally.
args <- commandArgs(trailingOnly=FALSE)
script <- sub("^--file=","",args[grepl("^--file=",args)])
app_dir <- if (length(script)) dirname(normalizePath(script[1])) else getwd()
local_library <- file.path(app_dir,".library")
dir.create(local_library,recursive=TRUE,showWarnings=FALSE)
.libPaths(c(local_library,.libPaths()))
options(repos=c(CRAN="https://cloud.r-project.org"))
renv_version <- "1.1.4"
if (!requireNamespace("renv",quietly=TRUE) || as.character(packageVersion("renv")) != renv_version) {
  install.packages(paste0("https://cran.r-project.org/src/contrib/Archive/renv/renv_",renv_version,".tar.gz"),
    repos=NULL,type="source",lib=local_library)
  if ("renv" %in% loadedNamespaces()) unloadNamespace("renv")
}
renv::restore(project=app_dir,library=local_library,lockfile=file.path(app_dir,"renv.lock"),
  prompt=FALSE,clean=TRUE)
source(file.path(app_dir,"R/runtime.R"))
stopifnot(nrow(runtime_mismatches(app_dir))==0L)
cli::cli_alert_success("Recorded packages restored in {.path {local_library}}.")
cli::cli_alert_info("Start the app with Rscript run_app.R.")
