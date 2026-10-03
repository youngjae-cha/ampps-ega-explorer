#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
app_dir <- if (length(script)) dirname(normalizePath(script[1])) else getwd()
local_library <- file.path(app_dir,".library")
if (dir.exists(local_library)) .libPaths(c(local_library,.libPaths()))
source(file.path(app_dir,"R/runtime.R"))
different <- runtime_mismatches(app_dir)
if (nrow(different)) stop("Restore the recorded versions with Rscript install_dependencies.R. Packages: ",
  paste(paste0(different$package," (need ",different$version,", found ",different$installed,")"),collapse=", "),call.=FALSE)
port <- as.integer(Sys.getenv("AMPPS_PORT", "7860"))
if (is.na(port) || port < 1024L || port > 65535L) cli::cli_abort("AMPPS_PORT must be an integer from 1024 to 65535.")
cli::cli_alert_info("AMPPS EGA Explorer 3.0.1: http://127.0.0.1:{port}")
shiny::runApp(app_dir, host = "127.0.0.1", port = port,
              launch.browser = interactive() || identical(Sys.getenv("AMPPS_OPEN_BROWSER"), "1"))
