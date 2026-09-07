#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
app_dir <- if (length(script)) dirname(normalizePath(script[1])) else getwd()
required <- c("shiny", "ggplot2", "plotly", "DT", "igraph", "EGAnet", "jsonlite", "htmltools", "zip")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install the missing R packages using install_dependencies.R: ", paste(missing, collapse = ", "))
port <- as.integer(Sys.getenv("AMPPS_PORT", "7860"))
message("AMPPS EGA Explorer: http://127.0.0.1:", port)
shiny::runApp(app_dir, host = "127.0.0.1", port = port,
              launch.browser = interactive() || identical(Sys.getenv("AMPPS_OPEN_BROWSER"), "1"))
