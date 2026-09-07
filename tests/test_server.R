# Run from app folder: Rscript tests/test_server.R
# Integration tests execute the real Shiny server, not a replacement server.
app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env, chdir = TRUE)

expect_server_error <- function(code, pattern) {
  msg <- tryCatch({force(code); NULL}, error = function(e) conditionMessage(e))
  stopifnot(!is.null(msg), grepl(pattern, msg, ignore.case = TRUE))
}
as_upload <- function(path, name = basename(path)) {
  data.frame(name = name, size = unname(file.info(path)$size),
             type = "text/csv", datapath = path, stringsAsFactors = FALSE)
}

# The generated upload has no relation to GSS and an explicitly independent
# predictor column; one shared covariate lets us test model-state invalidation.
set.seed(20260907)
server_n <- 160L
server_factors <- matrix(rnorm(server_n * 2L), server_n, 2L)
server_data <- data.frame(X = rnorm(server_n), Z = rnorm(server_n))
for (j in 1:8) server_data[[paste0("Y", j)]] <- .3 * server_data$X +
  .7 * server_factors[, 1L + (j > 4L)] + .6 * rnorm(server_n)
server_data_path <- tempfile("ampps_server_upload_", fileext = ".csv")
server_new_path <- tempfile("ampps_server_replacement_", fileext = ".csv")
server_bad_path <- tempfile("ampps_server_invalid_", fileext = ".csv")
utils::write.csv(server_data, server_data_path, row.names = FALSE)
server_changed_data <- server_data
server_changed_data$Y1 <- server_changed_data$Y1 + server_changed_data$X
utils::write.csv(server_changed_data, server_new_path, row.names = FALSE)
utils::write.csv(data.frame(text = c("a", "b")), server_bad_path, row.names = FALSE)

shiny::testServer(app_env$server, {
  # testServer has no browser to send UI defaults, so send the same defaults.
  session$setInputs(source_mode = "demo", focal = c("Happy", "Trust", "Fair"),
    boundary = "community", result_scope = "local", plot_metric = "t",
    multiplicity = FALSE, question = "Demonstration", population = "GSS example",
    analysis_unit = "Year", network_unit = "Respondent", rationale = "Example",
    timing = "Retrospective reconstruction", record = "", interpretation = "",
    annotation_focal = "Happy")
  stopifnot(!dirty(), length(result()$measures) == 44L,
            nrow(result()$landscape) == 44L,
            identical(sort(focal()), sort(c("Happy", "Trust", "Fair"))),
            identical(as.integer(neighborhoods()$summary$k), c(11L, 10L, 14L, 18L)))
  original_fits <- result()$landscape
  original_layout <- result()$network$coordinates
  ia <- result()$input_provenance
  stopifnot(all(ia$summary$primary_n == 46L),
            ia$summary$restricted_n[ia$summary$item == "Happy"] == 31L,
            abs(ia$restricted_landscape$p_bonferroni_universe[ia$restricted_landscape$item == "Happy"] - .140196802594528) < 1e-9,
            "input_provenance" %in% names(payload()),
            grepl("not effective sample", paste(report_text(), collapse = " "), fixed = TRUE))
  stopifnot(nrow(reports()$focal) == 3L,
            all(c("Happy", "Trust", "Fair") %in% reports()$focal$item))
  stopifnot(inherits(output$network_plot, "json"),
            inherits(output$landscape_plot, "json"))
  for (boundary_name in c("ring15", "ring25", "ring35", "full", "community")) {
    session$setInputs(boundary = boundary_name)
    expected_k <- c(ring15 = 10L, ring25 = 14L, ring35 = 18L, full = 44L, community = 11L)[boundary_name]
    stopifnot(length(neighborhood()) == expected_k, !dirty(),
              identical(original_fits, result()$landscape),
              identical(original_layout, result()$network$coordinates))
  }
  session$setInputs(multiplicity = TRUE)
  stopifnot(!dirty(), identical(original_fits, result()$landscape))
  session$setInputs(focal = character())
  expect_server_error(focal(), "at least one focal")
  expect_server_error(reports(), "at least one focal")
  expect_server_error(payload(), "at least one focal")
  session$setInputs(focal = c("Happy", "Trust", "Fair"))
  stopifnot(nrow(reports()$focal) == 3L)

  # MockShinySession intentionally does not apply server-to-browser updates.
  # Capture the actual messages to verify that switching source clears the
  # demonstration narrative even before any new file is loaded or built.
  input_updates <- list()
  session$sendInputMessage <- function(inputId, message) {
    input_updates[[inputId]] <<- message
    invisible()
  }

  # Merely changing source must withhold the existing GSS results and export.
  session$setInputs(source_mode = "upload", predictor = "X", candidates = paste0("Y", 1:8),
    covariates = character(), own_lag = FALSE, lag_suffix = "Lag",
    missing = "common", se_method = "classical", min_n = 10,
    max_missing = .2, bootstrap = FALSE, boot_iter = "100", seed = 20260907)
  stopifnot(dirty())
  stopifnot(is.null(rv$upload), identical(input_updates$question$value, ""),
            identical(input_updates$rationale$value, ""),
            identical(input_updates$interpretation$value, ""),
            identical(input_updates$record$value, ""),
            identical(input_updates$population$value, "Not supplied"),
            identical(input_updates$analysis_unit$value, "Not supplied"),
            identical(input_updates$include_inputs$value, FALSE),
            identical(input_updates$relation_reason$value, ""),
            identical(input_updates$timing$value, "Not documented here"))
  expect_server_error(result(), "Settings changed")
  expect_server_error(payload(), "Settings changed")

  session$setInputs(data_csv = as_upload(server_data_path, "independent_test.csv"))
  stopifnot(is.null(rv$error), nrow(rv$upload) == server_n,
            identical(rv$source_id, unname(tools::md5sum(server_data_path))), dirty())
  # selectInput update messages go to a real browser; explicitly return choices.
  session$setInputs(predictor = "X", candidates = paste0("Y", 1:8),
    covariates = character(), focal = c("Y1", "Y2"), run = 1L)
  stopifnot(is.null(rv$error), !dirty(),
            identical(result()$measures, paste0("Y", 1:8)),
            nrow(result()$landscape) == 8L, result()$network$n == server_n,
            result()$network$settings$source == "computed",
            result()$label == "independent_test.csv",
            all(c("Y1", "Y2") %in% payload()$report$focal$item),
            identical(input_updates$annotation_focal$value, "Y1"))
  session$setInputs(annotation_focal = "Happy", annotation_item = "Y3",
    relation = "related", relation_reason = "Invalid stale pair must not save.",
    save_annotation = 1L)
  stopifnot(nrow(rv$annotations) == 0L)
  upload_fits <- result()$landscape
  upload_settings <- rv$settings
  session$setInputs(boundary = "ring35", plot_metric = "b")
  stopifnot(!dirty(), identical(upload_fits, result()$landscape))
  stopifnot(inherits(output$network_plot, "json"),
            inherits(output$landscape_plot, "json"))

  # Alter a supported model input: stale output must be unavailable until run.
  session$setInputs(covariates = "Z")
  stopifnot(dirty(), identical(upload_settings, rv$settings))
  expect_server_error(result(), "Settings changed")
  expect_server_error(reports(), "Settings changed")
  expect_server_error(report_text(), "Settings changed")
  expect_server_error(payload(), "Settings changed")
  expect_server_error(output$network_plot, "Settings changed")
  expect_server_error(output$landscape_plot, "Settings changed")
  session$setInputs(run = 2L)
  stopifnot(is.null(rv$error), !dirty(), identical(result()$model$covariates, "Z"),
            !isTRUE(all.equal(upload_fits$b, result()$landscape$b)),
            identical(upload_settings$candidates, rv$settings$candidates))

  # A different file under the same displayed name invalidates by its digest.
  old_source <- rv$source_id
  session$setInputs(data_csv = as_upload(server_new_path, "independent_test.csv"))
  stopifnot(dirty(), rv$source_id != old_source)
  stopifnot(identical(input_updates$include_inputs$value, FALSE),
            identical(input_updates$relation_reason$value, ""))
  expect_server_error(payload(), "Settings changed")
  session$setInputs(predictor = "X", candidates = paste0("Y", 1:8),
    covariates = "Z", focal = "Y1", run = 3L)
  stopifnot(is.null(rv$error), !dirty(), result()$landscape$b[1] > .8)

  # An invalid new file must not leave old upload results accessible.
  session$setInputs(data_csv = as_upload(server_bad_path, "invalid.csv"))
  stopifnot(!is.null(rv$error), is.null(rv$upload), dirty())
  expect_server_error(result(), "Settings changed")
  expect_server_error(payload(), "Settings changed")

  # Paste follows the same validated upload handler and analyzes pasted rows,
  # not stale file data. No browser upload-dialog emulation is involved.
  session$setInputs(pasted_csv = paste(readLines(server_data_path, warn = FALSE), collapse = "\n"),
                     load_paste = 1L)
  stopifnot(is.null(rv$error), nrow(rv$upload) == server_n,
            identical(rv$upload_label, "Pasted CSV"), dirty(),
            identical(input_updates$rationale$value, ""),
            identical(input_updates$population$value, "Not supplied"))
  session$setInputs(predictor = "X", candidates = paste0("Y", 1:8),
    covariates = character(), focal = "Y1", run = 4L)
  stopifnot(is.null(rv$error), !dirty(), result()$label == "Pasted CSV",
            nrow(result()$landscape) == 8L, result()$network$n == server_n,
            result()$network$settings$source == "computed",
            isTRUE(all.equal(upload_fits$b, result()$landscape$b)))
  session$setInputs(boundary = "full", annotation_focal = "Y1", annotation_item = "Y2",
    relation = "related", relation_reason = "Different outcome; interpretation note only.",
    save_annotation = 2L)
  stopifnot(nrow(active_annotations()) == 1L, nrow(rv$annotations) == 1L)
  session$setInputs(focal = c("Y1", "Y2"))
  stopifnot(nrow(active_annotations()) == 0L, nrow(rv$annotations) == 1L)
  session$setInputs(focal = "Y1")
  stopifnot(nrow(active_annotations()) == 1L)

  # Explicitly returning to the bundled example restores all three focal rows.
  session$setInputs(source_mode = "demo", focal = c("Happy", "Trust", "Fair"), run = 5L)
  stopifnot(is.null(rv$error), !dirty(), length(result()$measures) == 44L,
            identical(input_updates$include_inputs$value, FALSE),
            identical(input_updates$relation_reason$value, ""),
            nrow(reports()$focal) == 3L,
            identical(original_fits, result()$landscape))
})
unlink(c(server_data_path, server_new_path, server_bad_path))
cat("PASS: real Shiny server demo, boundary invariance, empty focal blocking, live uploaded-data analysis, dirty model/file blocking, invalid file handling, and demo restoration.\n")
cat("PASS: source switch sends narrative-clearing messages before upload; pasted CSV gets its own real analysis; alternative annotations exclude newly focal outcomes.\n")
cat("PASS: a stale GSS annotation focal is reset for new data, and an invalid stale comparison pair cannot be saved.\n")
