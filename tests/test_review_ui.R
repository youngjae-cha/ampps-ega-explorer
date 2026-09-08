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
  stopifnot(grepl("HAPPY) [RC]",html,fixed=TRUE),
    grepl("TRUST) [original]",html,fixed=TRUE),
    plot_data()$b[plot_data()$item=="Happy"]>0,
    reports()$focal$b[reports()$focal$item=="Happy"]<0,
    reports()$focal$oriented_b[reports()$focal$item=="Happy"]>0,
    grepl("Happy [RC]: b = 0.0076",paste(report_text(),collapse=" "),fixed=TRUE))
  before <- result()$landscape
  session$setInputs(orientation_mode="original")
  stopifnot(!dirty(),identical(before,result()$landscape),
    plot_data()$b[plot_data()$item=="Happy"]<0,
    !grepl("[RC]",output$combined_estimates$html,fixed=TRUE),
    grepl("Original numerical coding (no display reversal)",output$combined_estimates$html,fixed=TRUE),
    grepl("Display direction: Original coding",paste(report_text(),collapse=" "),fixed=TRUE),
    all(payload()$orientation$multiplier==1),
    any(payload()$documented_orientation_key$multiplier==-1))
  session$setInputs(orientation_mode="documented")
  stopifnot(!dirty(),identical(before,result()$landscape),
    plot_data()$b[plot_data()$item=="Happy"]>0)
  custom_messages <- list()
  session$sendCustomMessage <- function(type,message) {
    custom_messages[[length(custom_messages)+1L]] <<- list(type=type,message=message)
    invisible()
  }
  session$setInputs(run=1L)
  stopifnot(any(vapply(custom_messages,function(x) x$type=="navigateStage" && identical(x$message$stage,"map"),logical(1))),
    identical(tail(custom_messages,1)[[1]]$type,"analysisBuildState"),
    identical(tail(custom_messages,1)[[1]]$message$busy,FALSE))
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
    focal = "Happy", run = 2L)
  stopifnot(is.null(rv$error), !dirty(), identical(outcome_labels("Happy"), "Happy"),
    !grepl("General happiness", output$combined_estimates$html, fixed = TRUE))
})
unlink(upload_path)
cat("PASS: hosted privacy wording, readable estimate table, three metric views, single-focal display, user labels isolated from GSS.\n")
cat("PASS: orientation toggle updates all display estimates and reports without refitting; unresolved labels remain original; Build success navigates and releases busy state.\n")
