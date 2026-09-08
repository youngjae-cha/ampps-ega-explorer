# Progressive disclosure changes the interface, not the comparison algorithm.
app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env, chdir = TRUE)
txt <- as.character(app_env$ui)
at <- regexpr('<details id="boundary_checks">', txt, fixed = TRUE)[1]
stopifnot(at > 0,
  grepl('no distance setting is needed', txt, fixed = TRUE),
  grepl('meet the same analysis requirements', txt, fixed = TRUE),
  !grepl('<details id="boundary_checks" open', txt, fixed = TRUE))
section <- substring(txt, at)
section <- substring(section, 1L, regexpr('</details>', section, fixed = TRUE)[1])
stopifnot(grepl('id="boundary"', section, fixed = TRUE),
  grepl('id="boundary_table"', section, fixed = TRUE),
  grepl('value="community" selected', section, fixed = TRUE))
shiny::testServer(app_env$server, {
  session$setInputs(source_mode="demo", focal=c("Happy","Trust","Fair"),
    result_scope="local", plot_metric="abs_t", multiplicity=FALSE,
    interpretation="", annotation_focal="Happy")
  # Null UI input also resolves to the default community.
  stopifnot(length(neighborhood()) == 11,
    grepl('EGA communities (default)', output$active_boundary$html, fixed=TRUE),
    !grepl('Return to EGA communities', output$active_boundary$html, fixed=TRUE))
  before <- result()$landscape
  stopifnot(identical(vapply(neighborhoods()$sets, length, integer(1)),
    c(community=11L, ring15=10L, ring25=14L, ring35=18L)))
  session$setInputs(boundary="ring25")
  stopifnot(length(neighborhood())==14, !dirty(), identical(before,result()$landscape),
    grepl('Distance ring', output$active_boundary$html, fixed=TRUE),
    grepl('Return to EGA communities', output$active_boundary$html, fixed=TRUE),
    identical(names(payload()$neighborhoods$sets), c("community","ring15","ring25","ring35")))
  messages <- list()
  session$sendInputMessage <- function(inputId, message) {
    messages[[length(messages)+1L]] <<- list(id=inputId, message=message)
  }
  session$setInputs(reset_boundary=1L)
  stopifnot(any(vapply(messages, function(x) identical(x$id,"boundary") &&
    identical(x$message$value,"community"), logical(1))))
  session$setInputs(boundary="community")
  stopifnot(length(neighborhood())==11, identical(before,result()$landscape), !dirty())
})
cat("PASS: community default; optional collapsed rings; visible nondefault view and reset; all boundaries retained without refitting.\n")
