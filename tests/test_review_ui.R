# Hosted wording and the readable display are tested separately from model QA.
app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env, chdir = TRUE)
ui_text <- as.character(app_env$ui)
upload_data <- read.csv("examples/upload_example.csv")
names(upload_data)[names(upload_data) == "Item01"] <- "Happy"
upload_path <- tempfile(fileext = ".csv")
write.csv(upload_data, upload_path, row.names = FALSE)
stopifnot(grepl("Use public or synthetic data only", ui_text, fixed = TRUE),
          !grepl("Your files stay on this computer", ui_text, fixed = TRUE),
          !grepl("three focal outcomes", ui_text, fixed = TRUE))
shiny::testServer(app_env$server, {
  session$setInputs(source_mode = "demo", focal = c("Happy", "Trust", "Fair"),
    boundary = "community", result_scope = "local", plot_metric = "abs_t",
    multiplicity = FALSE, interpretation = "", annotation_focal = "Happy")
  html <- output$combined_estimates$html
  stopifnot(grepl("General happiness", html, fixed = TRUE),
    grepl("Self-rated health", html, fixed = TRUE),
    grepl("Coefficient b", html, fixed = TRUE))
  before <- result()$landscape
  for (mode in c("t", "b", "abs_t")) {
    session$setInputs(plot_metric = mode)
    stopifnot(inherits(output$landscape_plot, "json"),
      identical(before, result()$landscape))
  }
  session$setInputs(focal = "Happy")
  stopifnot(grepl("1 focal outcomes", output$completed_comparison$html, fixed = TRUE))
  # User columns named Happy must not silently inherit GSS measurement labels.
  session$setInputs(source_mode = "upload", predictor = "X",
    candidates = setdiff(names(upload_data), "X"), covariates = character(),
    own_lag = FALSE, lag_suffix = "Lag", missing = "common", se_method = "classical",
    min_n = 10, max_missing = .2, bootstrap = FALSE, boot_iter = "100", seed = 123)
  session$setInputs(data_csv = data.frame(name = "synthetic.csv", size = file.info(upload_path)$size,
    type = "text/csv", datapath = upload_path))
  session$setInputs(predictor = "X", candidates = setdiff(names(upload_data), "X"),
    focal = "Happy", run = 1L)
  stopifnot(is.null(rv$error), !dirty(), identical(outcome_labels("Happy"), "Happy"),
    !grepl("General happiness", output$combined_estimates$html, fixed = TRUE))
})
unlink(upload_path)
cat("PASS: hosted privacy wording, readable estimate table, three metric views, single-focal display, user labels isolated from GSS.\n")
