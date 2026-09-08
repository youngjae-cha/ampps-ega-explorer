# Real server checks: only an explicitly requested, successful build advances.
app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env, chdir = TRUE)
shiny::testServer(app_env$server, {
  messages <- list()
  session$sendCustomMessage <- function(type, message) {
    messages[[length(messages) + 1L]] <<- list(type = type, message = message)
    invisible()
  }
  session$setInputs(source_mode = "demo", focal = c("Happy", "Trust", "Fair"),
    boundary = "community", result_scope = "local", plot_metric = "abs_t",
    multiplicity = FALSE, interpretation = "", annotation_focal = "Happy")
  types <- function() vapply(messages, `[[`, character(1), "type")
  stopifnot(!"navigateStage" %in% types())
  session$setInputs(run = 1L)
  nav <- messages[types() == "navigateStage"]
  stopifnot(length(nav) == 1L, identical(nav[[1L]]$message$stage, "map"))
  state <- messages[types() == "analysisBuildState"]
  stopifnot(length(state) > 0L, identical(tail(state, 1L)[[1L]]$message$busy, FALSE))
  messages <- list()
  session$setInputs(focal = "Happy", boundary = "ring25")
  stopifnot(!"navigateStage" %in% types())
  session$setInputs(source_mode = "upload", predictor = "X", candidates = paste0("Y", 1:4),
    covariates = character(), own_lag = FALSE, missing = "common", se_method = "classical",
    min_n = 10, max_missing = .2, bootstrap = FALSE, boot_iter = "100", seed = 20260908)
  messages <- list()
  session$setInputs(run = 2L) # no file, therefore the run cannot succeed
  stopifnot(!"navigateStage" %in% types())
  state <- messages[types() == "analysisBuildState"]
  stopifnot(length(state) > 0L, identical(tail(state, 1L)[[1L]]$message$busy, FALSE))
  fixture <- normalizePath("examples/upload_example.csv")
  session$setInputs(data_csv = data.frame(name = "synthetic.csv",
    size = file.info(fixture)$size, type = "text/csv", datapath = fixture))
  messages <- list()
  session$setInputs(predictor = "X", candidates = sprintf("Item%02d", 1:12),
    focal = "Item01", run = 3L)
  stopifnot(is.null(rv$error), !dirty(), length(result()$measures) == 12L)
  nav <- messages[types() == "navigateStage"]
  stopifnot(length(nav) == 1L, identical(nav[[1L]]$message$stage, "map"))
  state <- messages[types() == "analysisBuildState"]
  stopifnot(length(state) > 0L, identical(tail(state, 1L)[[1L]]$message$busy, FALSE))
})
cat("PASS: explicit successful demo/upload builds advance, failed build stays put and releases busy state, setting changes do not navigate.\n")
