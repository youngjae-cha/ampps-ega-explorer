# AMPPS Explorer: the same four-step workflow locally or on a Shiny host.
if (dir.exists(".library")) .libPaths(c(normalizePath(".library"), .libPaths()))
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
source(file.path("R", "model.R"), local = TRUE)
source(file.path("R", "provenance.R"), local = TRUE)
source(file.path("R", "ega.R"), local = TRUE)
source(file.path("R", "export.R"), local = TRUE)
source(file.path("R", "concentration.R"), local = TRUE)
source(file.path("R", "comparison.R"), local = TRUE)
source(file.path("R", "map_display.R"), local = TRUE)
options(shiny.maxRequestSize = 50 * 1024^2, shiny.sanitize.errors = TRUE)

APP_VERSION <- "3.0.1"
DEMO <- load_demo_data("data")
DEMO_SOURCES <- read.csv(file.path("data", "gss_year_cell_source.csv"), check.names = FALSE)
DEMO_NETWORK <- load_demo_network("data")
DEMO_ITEMS <- colnames(DEMO_NETWORK$adjacency)
ACCENT <- "#176d78"
`%or%` <- function(x, y) if (is.null(x) || !length(x)) y else x
fmt <- function(x, d = 3) ifelse(is.finite(x), formatC(x, digits = d, format = "f"), "—")
READABLE_LABELS <- c(Happy="General happiness", Trust="Interpersonal trust", Fair="Perceived fairness",
  Health="Self-rated health", Life="Life excitement", Satjob="Job / housework satisfaction",
  Hapmar="Marital happiness", Helpful="Perceived helpfulness", Finrela="Relative family income",
  Finalter="Change in finances", News="Newspaper reading")
readable_label <- function(items) {
  labels <- unname(READABLE_LABELS[items])
  labels[is.na(labels)] <- items[is.na(labels)]
  paste0(labels, " (", toupper(items), ")")
}
safe_csv <- function(path) {
  d <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                na.strings = c("", "NA", "NaN"), fileEncoding = "UTF-8-BOM")
  if (!nrow(d) || nrow(d) > 200000L || ncol(d) > 1000L)
    stop("Use a CSV with 1–200,000 rows and at most 1,000 columns.")
  if (anyDuplicated(names(d)) || any(!nzchar(trimws(names(d)))))
    stop("Column names must be nonempty and unique.")
  d
}
help_box <- function(title, ...) tags$details(tags$summary(title), ...)
lead <- function(k, title, text) div(class = "step-lead", div(class = "eyebrow", paste("STEP", k, "OF 4")), h2(title), p(text))
next_btn <- function(id, text) actionButton(id, text, class = "btn-primary next-button")
table_opts <- list(pageLength = 12, scrollX = TRUE, dom = "tip", order = list())
display_estimates <- function(x, all = FALSE) {
  x <- display_oriented_estimates(x)
  out <- data.frame(Outcome = paste0(x$item, ifelse(x$orientation_reversed, " [RC]", "")),
    `Higher displayed value` = x$high_value_means, n = x$n, b = round(x$b, 4), SE = round(x$se, 4),
    `95% interval` = paste0("[", fmt(x$ci_lo, 4), ", ", fmt(x$ci_hi, 4), "]"),
    t = round(x$t, 3), p = signif(x$p, 4), check.names = FALSE)
  if (all) out$Role <- ifelse(x$focal, "Focal", ifelse(x$in_neighborhood, "Neighbor", "Other"))
  else out$`Unreported with larger |t|` <- x$n_unreported_larger_abs_t_neighborhood
  if ("p_bonferroni_neighborhood" %in% names(x)) {
    out$`p × boundary k` <- signif(x$p_bonferroni_neighborhood, 4)
    out$`p × universe k` <- signif(x$p_bonferroni_universe, 4)
  }
  if (any(x$status != "ok")) out$Status <- x$status
  out
}

ui <- fluidPage(title = "AMPPS EGA Explorer",
  tags$head(tags$link(rel = "stylesheet", href = "app.css"), tags$script(src = "app.js")),
  div(class = "masthead", div(div(class = "eyebrow", "AMPPS · POST-HOC MULTIVERSE ANALYSIS"),
      h1("EGA Explorer"), p("Compare reported findings with available alternatives to examine support for a claim and possible selective reporting.")),
      div(class = "local-badge", "Research application", tags$small("Use public or synthetic data only"))),
  uiOutput("run_status"),
  tabsetPanel(id = "stage", type = "pills",
    tabPanel("1 · Define", value = "define",
      lead(1, "What can be compared?", "Start with the focal outcomes, predictor, and model. Include the other outcomes that meet the same analysis requirements."),
      fluidRow(column(4, div(class = "panel-card controls",
        radioButtons("source_mode", "Start with", c("GSS worked example" = "demo", "My CSV data" = "upload")),
        conditionalPanel("input.source_mode == 'demo'",
          p("44 supplied GSS outcomes · 47 annual rows"),
          p(class = "muted", "Model: outcome ~ MobilityLag + outcome's supplied lag. The relationship map uses archived respondent-level EGA outputs.")),
        conditionalPanel("input.source_mode == 'upload'",
          p(class = "muted", "In the hosted app, uploaded or pasted data are sent to the analysis server. Do not use confidential, identifiable, or restricted data. For those data, run a local copy instead."),
          help_box("Try a synthetic dataset", p("Download these files, then upload the data below. Select X as predictor and Item01–Item12 as outcomes. The descriptions file is optional."),
            downloadButton("download_example", "Example CSV"),
            downloadButton("download_example_metadata", "Example descriptions")),
          fileInput("data_csv", "Analysis data (.csv)", accept = ".csv"),
          help_box("Or paste a CSV", textAreaInput("pasted_csv", "CSV text (header row required)", rows = 5, width = "100%"),
            actionButton("load_paste", "Load pasted data")),
          selectInput("predictor", "Numeric predictor", choices = character()),
          selectizeInput("candidates", "Candidate outcomes", choices = character(), multiple = TRUE),
          selectizeInput("covariates", "Shared numeric covariates (optional)", choices = character(), multiple = TRUE),
          checkboxInput("own_lag", "Add each outcome's supplied lag column", FALSE),
          conditionalPanel("input.own_lag", textInput("lag_suffix", "Lag-column suffix", "Lag")),
          selectInput("missing", "Regression missingness", c("One common complete-case sample" = "common", "Complete cases for each outcome" = "per_outcome")),
          selectInput("se_method", "Uncertainty estimate", c("Classical OLS" = "classical", "HC3 heteroskedasticity-robust" = "HC3")),
          numericInput("min_n", "Minimum usable model rows per outcome", 10, min = 5, step = 1),
          sliderInput("max_missing", "Maximum missing outcome fraction", min = 0, max = .9, value = .2, step = .05),
          fileInput("network_csv", "Separate network data (optional .csv)", accept = ".csv"),
          p(class = "muted", "Supply the same outcome column names. Otherwise the outcome columns of the analysis data build the map; the predictor never enters EGA."),
          fileInput("metadata_csv", "Measure descriptions (optional .csv)", accept = ".csv"),
          help_box("Metadata format", p("Columns: item, label, domain, respondent_scope, coding_note. Missing descriptions remain unclassified. For an explicit display direction, add orientation_multiplier (-1 or +1), high_value_means, and orientation_source. Uploaded keys are labeled user-declared, not independently verified.")),
          checkboxInput("bootstrap", "Estimate bootstrap stability and a typical network", FALSE),
          conditionalPanel("input.bootstrap", selectInput("boot_iter", "Bootstrap replications", c(100, 500), selected = 100)),
          numericInput("seed", "Network seed", 20260907, min = 1, max = 2147483646)),
        actionButton("run", "Build analysis", class = "btn-primary run-button"),
        p(class = "muted", "Changing analysis settings requires a new run. Changing focal measures or map boundaries reuses the same estimates."))),
      column(8, div(class = "panel-card",
        selectizeInput("focal", "Focal outcomes", choices = DEMO_ITEMS, selected = c("Happy", "Trust", "Fair"), multiple = TRUE),
        p(class = "muted", "These may be reported findings or theory-selected outcomes. The label records their role, not when they were selected."),
        textAreaInput("question", "Question or claim", "Declines in U.S. residential mobility predict subsequent declines in happiness, trust in others, and perceived fairness.", rows = 2, width = "100%"),
        fluidRow(column(6, textInput("population", "Population / period", "GSS example · United States, 1972–2018")),
                 column(6, textInput("analysis_unit", "Analysis unit", "Year (annual aggregate)"))),
        textInput("network_unit", "Network-data unit", "Individual survey respondents (archived map)"),
        textAreaInput("rationale", "Why these focal outcomes?", "Linked to the original study's theoretical model.", rows = 2, width = "100%"),
        selectInput("timing", "Selection timing (your declaration)", c("Not documented here", "Before inspecting focal predictor results", "After inspecting some results", "Retrospective reconstruction"), selected = "Retrospective reconstruction"),
        textInput("record", "Dated record or source (optional)", "")),
        div(class = "panel-card", h3("Analyzable set"), uiOutput("scope_summary"), DTOutput("eligibility_table"),
          help_box("Which other outcomes can be compared?",
            p("First specify the focal outcomes, predictor, and model. For a time-series analysis, prepare the intended years and lag columns in the input data. Apply the same coding and data-availability requirements to focal and comparison outcomes, including the minimum usable rows and missingness rule."),
            p("Include every candidate meeting those requirements, whether or not the theory predicts an association. The exclusion record shows which outcomes could not be analyzed. EGA organizes the included outcomes in Step 2.")),
          next_btn("to_map", "Explore the map →"))))),
    tabPanel("2 · Map", value = "map",
      lead(2, "What lies beside the focal outcomes?", "Start with the communities EGA finds around the focal outcomes. The app selects these automatically; no distance setting is needed."),
      fluidRow(column(3, div(class = "panel-card controls",
        uiOutput("active_boundary"),
        uiOutput("network_method"),
        next_btn("to_results", "View the comparison results →"))),
      column(9, div(class = "panel-card", plotlyOutput("network_plot", height = "720px"),
        p(class = "figure-note", "Color = EGA community. Diamonds mark focal outcomes; dark-outlined circles mark their current unreported neighbors. Every outcome is labeled. Hover for item wording, response options, and measurement notes where supplied. Node positions remain fixed across comparison settings.")),
        div(class = "panel-card", tags$details(id = "boundary_checks",
          tags$summary("Optional: compare a narrower or wider neighborhood"),
          p("A nearby outcome can fall just outside a community. These additional views show what enters or leaves when the comparison range changes. You can continue with the EGA community without opening this check."),
          selectInput("boundary", "Additional comparison view", c("Focal EGA communities (default)" = "community", "Distance ring · 15%" = "ring15", "Distance ring · 25%" = "ring25", "Distance ring · 35%" = "ring35", "Full analyzable set" = "full"), selected = "community"),
          p(class = "muted", "The selected view stays active if this section is closed. The current view is labeled beside the map. All three rings are calculated and exported automatically; fitted results stay fixed."),
          uiOutput("boundary_notes"), DTOutput("boundary_table"),
          help_box("Distance calculation", p("A community view includes all communities containing a focal outcome. Rings use the shortest path to the nearest focal outcome, with retained edge length 1/|r|. Cutoffs are the 15th, 25th and 35th percentiles of finite nonfocal distances (type 7); ties are retained. These are predefined sensitivity settings, not estimated optimal boundaries."),
            p("Map coordinates are for display. The ring rule uses graph paths, not distances on the screen.")))),
        div(class = "panel-card", h3("Neighborhood and measurement details"), DTOutput("neighbor_table"))))),
    tabPanel("3 · Results", value = "results",
      lead(3, "What do the alternatives show?", "Read the fixed-model results alongside the EGA neighborhood, keeping each focal outcome visible."),
      fluidRow(column(3, div(class = "panel-card controls",
        radioButtons("result_scope", "Show", c("Current map boundary" = "local", "All analyzable outcomes" = "all")),
        radioButtons("plot_metric", "Display", c("Order results by absolute t" = "abs_t", "Signed t-statistic" = "t", "Estimate with 95% interval" = "b")),
        radioButtons("orientation_mode", "Display direction", c("Documented semantic key"="documented", "Original coding"="original"), selected="documented"),
        checkboxInput("multiplicity", "Show optional Bonferroni sensitivity", FALSE),
        help_box("Reading the display", p("Absolute t orders each estimate relative to its standard error. Read magnitude and direction from the coefficients and intervals. RC marks directions reversed using a documented semantic key. Original coefficients remain in the downloads; unverified or nonmonotone items retain original coding."),
          p("Check the higher-value meaning for each outcome. Coding alignment preserves its units but does not make different constructs equivalent.")),
        conditionalPanel("input.multiplicity", p(class = "muted", "Two declared families are shown separately: the current boundary and the full analyzable set. These are regression p-value sensitivities, not selection verdicts. Valid individual tests and a justified family remain necessary.")),
        downloadButton("download_plot", "Download result plot (PDF)"),
        next_btn("to_interpret", "Interpret and report →"))),
      column(9, conditionalPanel("input.plot_metric == 'abs_t'",
        div(class="panel-card", h3("Where do the reported results fall?"),
          plotlyOutput("distribution_plot", height="330px"),
          p(class="figure-note", "Raw points and box plots show absolute t for reported outcomes, unreported neighbors, and other analyzable outcomes. The groups organize the comparison; the reference calculation below uses an explicitly specified set.")),
        div(class="panel-card", id="reading_combined", h3("Read the individual results"),
          p(class="reading-intro", "Compare positions by absolute t, then read each coefficient and interval in its outcome's units and documented direction."),
          uiOutput("combined_estimates"),
          p(class="reading-intro", "Filled points mark focal outcomes; open points mark unreported alternatives. |t| compares an estimate with its standard error, not effect magnitude."),
          p(class="reading-intro", "RC = reverse-coded display direction. Higher-value meanings appear beside each estimate. Original marks an unreversed direction, either because the key is unresolved or Original coding is selected; consult the item notes."))),
        conditionalPanel("input.plot_metric != 'abs_t'", div(class = "panel-card", id = "prominence_panel",
        h3("A · Read the selected result display"),
        p(class = "reading-intro", "Signed t compares the estimate with its standard error. The coefficient view shows each estimate and interval on its own outcome scale."),
        uiOutput("landscape_holder"),
        p(class = "reading-intro", "Directions follow the selected display direction; raw coefficients and intervals remain in the downloads.")),
        div(class = "panel-card", id = "coefficient_panel", h3("B · Read the displayed estimates"),
          p(class = "reading-intro", "The same outcomes, in the same order. RC marks the documented reversals applied consistently to coefficients, t statistics, and both interval endpoints."),
          uiOutput("reading_estimates"),
          p(class = "reading-intro", "When the documented key is selected, a positive coefficient points toward the stated higher-value meaning for aligned items. Original-coded items require their separate coding notes."))),
        div(class = "panel-card", h3("Every focal outcome"), DTOutput("focal_table")),
        uiOutput("input_sensitivity_panel"),
        div(class = "panel-card", h3("Full estimates"), DTOutput("landscape_table")),
        div(class = "panel-card", id="reference_panel", h3("Are reported results concentrated near the top?"),
          p("The rank-sum reference quantifies concentration within a specified comparison set. Use it alongside the estimates and measurement information to examine result-based selection and other explanations."),
          uiOutput("reference_status"),
          conditionalPanel("input.source_mode == 'upload'",
            selectizeInput("reference_family", "Comparison set for this claim", choices=character(), multiple=TRUE),
            selectizeInput("reference_selected", "Reported outcomes for this claim (a subset of the focal outcomes)", choices=character(), multiple=TRUE),
            selectInput("reference_statistic", "Ranking statistic specified for this comparison",
              c("Choose a statistic"="", "Absolute t"="abs_t", "Signed t: positive direction"="positive_t", "Signed t: negative direction"="negative_t")),
            p(class="muted", "Signed t uses the recorded display direction. Changing that direction requires recalculation."),
            textAreaInput("reference_claim", "Claim addressed by this comparison", rows=2, width="100%"),
            selectInput("reference_condition", "Statistical condition justified for this set",
              c("Choose a condition"="", "Uniform reporting among equal-sized subsets"="uniform", "Joint label symmetry for a fixed report"="symmetry")),
            help_box("What do these conditions mean?",
              p("Uniform reporting: conditional on the observed statistics, every same-sized subset has equal reporting probability independently of those statistics."),
              p("Fixed-label symmetry: with reported labels fixed, the joint distribution of the statistics is invariant to permutations of candidate labels."),
              p("Justify the statistical condition separately from the substantive comparison. Advance selection, a shared construct, and EGA membership each provide context; measurement quality or expected associations can favor particular ranks.")),
            textAreaInput("reference_justification", "Why is this statistical condition defensible?", rows=3, width="100%"),
            textAreaInput("reference_design", "How were the set and statistic specified independently of the focal predictor results?", rows=2, width="100%"),
            checkboxInput("reference_independent", "I specified this set and statistic independently of the focal predictor results.", FALSE),
            actionButton("calculate_reference", "Calculate rank-sum reference", class="btn-primary"),
            actionButton("clear_reference", "Clear reference")),
          uiOutput("reference_result"))))),
    tabPanel("4 · Interpret & export", value = "interpret",
      lead(4, "What do these comparisons add?", "Connect the reported findings to unreported outcomes, their measurement context, and the questions worth pursuing next."),
      fluidRow(column(8, div(class = "panel-card", id="completed_comparison_panel", h3("From the EGA neighborhood to an interpretation"),
          uiOutput("completed_comparison"), uiOutput("rank_pattern")),
        div(class="panel-card", h3("Concentration reference and its interpretation"), uiOutput("reference_report")),
        div(class = "panel-card", h3("Every focal outcome remains in the record"), uiOutput("report_preview")),
        div(class = "panel-card", h3("Explain a comparison"),
          fluidRow(column(6, selectInput("annotation_focal", "Focal outcome", choices = c("Happy", "Trust", "Fair"))),
                   column(6, selectInput("annotation_item", "Unreported alternative", choices = setdiff(DEMO_ITEMS, c("Happy", "Trust", "Fair"))))),
          selectInput("relation", "Relation to this focal claim", c("Unclassified empirical neighbor" = "unclassified", "Related outcome / different scope" = "related", "Same-claim substitute (my justification)" = "substitute")),
          textAreaInput("relation_reason", "Measurement or population justification", rows = 2, width = "100%"),
          actionButton("save_annotation", "Save comparison note"), DTOutput("annotations_table")),
        div(class = "panel-card", h3("Your interpretation"),
          textAreaInput("interpretation", "What is supported, qualified, or newly suggested?", rows = 4, width = "100%", placeholder = "Explain whether support extends to same-claim alternatives, where the explanation applies, and what new questions arise. For results that stand out, consider result-based selection, measurement quality, underlying associations, and comparison-set composition."))),
      column(4, div(class = "panel-card controls", h3("Take the analysis with you"),
        p("Download the full estimates, every map boundary, measurement notes, analysis settings, network, and a readable report."),
        checkboxInput("include_inputs", "Also include my uploaded input files in the ZIP", FALSE),
        p(class = "muted", "Uploaded rows are excluded by default. Reproduction then requires your original files. The built-in aggregate example is included for reproducing that example."),
        downloadButton("download_bundle", "Download reproducibility ZIP", class = "btn-primary"),
        downloadButton("download_report", "Download readable report (.html)"),
        downloadButton("download_csv", "Download all estimates (.csv)"),
        help_box("What the record establishes", p("It records this app session, inputs and declared choices. It is not a preregistration or a tamper-proof record of past analyses. Empirical neighbors require your conceptual judgment; ranks do not identify how an author selected outcomes.")),
        uiOutput("provenance_panel")))))
  ),
  div(class = "footer", paste("AMPPS EGA Explorer", APP_VERSION), span(" · Numeric linear models · EGA / TMFG / Walktrap"))
)

server <- function(input, output, session) {
  rv <- reactiveValues(result = NULL, settings = NULL, error = NULL, upload = NULL,
    network_upload = NULL, metadata_upload = NULL, inputs = list(), events = list(),
    annotations = data.frame(focal = character(), item = character(), relation = character(), reason = character()),
    reference = NULL, reference_error = NULL,
    source_id = "bundled_gss", network_id = "", metadata_id = "", upload_label = "My data")
  log_event <- function(action, detail = "") {
    rv$events <- append(isolate(rv$events), list(list(time_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), action = action, detail = detail)))
  }
  config <- reactive({
    if (input$source_mode == "demo") return(list(source = "demo"))
    list(source = "upload", source_id = rv$source_id, network_id = rv$network_id, metadata_id = rv$metadata_id,
      predictor = input$predictor, candidates = input$candidates, covariates = input$covariates %or% character(),
      own_lag = isTRUE(input$own_lag), lag_suffix = input$lag_suffix %or% "Lag",
      missing = input$missing, se_method = input$se_method, min_n = input$min_n,
      max_missing = input$max_missing, bootstrap = isTRUE(input$bootstrap),
      iter = as.integer(input$boot_iter %or% 100), seed = as.integer(input$seed))
  })
  dirty <- reactive(!identical(config(), rv$settings))
  result <- reactive({
    validate(need(!is.null(rv$result), "Load data and build an analysis to continue."),
             need(is.null(rv$error), "The current run has an error. Resolve it in Step 1 before using or exporting results."),
             need(!dirty(), "Settings changed. Return to Step 1 and build the analysis before using these results."))
    rv$result
  })
  outcome_labels <- function(items) {
    a <- result()
    if (identical(rv$settings$source, "demo")) {
      key <- normalize_orientation_key(display_key(), items)
      return(paste0(readable_label(items), ifelse(key$multiplier == -1, " [RC]", ifelse(startsWith(key$status,"raw_")," [original]",""))))
    }
    labels <- as.character(a$metadata$label[match(items, a$metadata$item)])
    if (length(labels) != length(items)) return(items)
    missing_label <- is.na(labels) | !nzchar(trimws(labels)) | labels == items
    labels[missing_label] <- items[missing_label]
    key <- normalize_orientation_key(display_key(), items)
    paste0(ifelse(missing_label, items, paste0(labels, " (", items, ")")), ifelse(key$multiplier == -1," [RC]",""))
  }
  focal <- reactive({
    a <- result()
    f <- intersect(input$focal %or% character(), a$measures)
    validate(need(length(f) > 0, "Select at least one focal outcome in Step 1."))
    f
  })
  neighborhoods <- reactive(compare_neighborhoods(result()$network, focal()))
  neighborhood <- reactive({
    if (identical(input$boundary, "full")) result()$measures else neighborhoods()$sets[[input$boundary %or% "community"]]
  })
  display_key <- reactive(if (identical(input$orientation_mode,"original")) NULL else result()$orientation)
  reports <- reactive(make_report_tables(result()$landscape, focal(), neighborhood(), result()$metadata,
                                          multiplicity = isTRUE(input$multiplicity), orientation=display_key()))
  reference_config <- reactive(list(
    family=sort(input$reference_family %or% character()), selected=sort(input$reference_selected %or% character()),
    statistic=input$reference_statistic %or% "", condition=input$reference_condition %or% "",
    justification=input$reference_justification %or% "", design_record=input$reference_design %or% "",
    independent=isTRUE(input$reference_independent), claim=input$reference_claim %or% "",
    orientation=normalize_orientation_key(display_key(), result()$measures),
    source=rv$settings, landscape=result()$landscape, focal=sort(focal()),
    boundary=input$boundary %or% "community", question=input$question %or% ""))
  current_reference <- reactive({
    result()
    if (identical(rv$settings$source, "demo") || is.null(rv$reference) ||
        !identical(rv$reference$config, reference_config())) return(NULL)
    rv$reference$record
  })
  observe({
    a <- result()
    updateSelectizeInput(session, "reference_family", choices=a$measures,
      selected=intersect(isolate(input$reference_family), a$measures))
    updateSelectizeInput(session, "reference_selected", choices=focal(),
      selected=intersect(isolate(input$reference_selected), focal()))
  })
  observeEvent(input$calculate_reference, {
    rv$reference <- NULL; rv$reference_error <- NULL
    tryCatch({
      a <- result()
      if (identical(rv$settings$source, "demo"))
        stop("The GSS example uses estimates and ranks without a reference probability; its reporting or symmetry condition has not been justified.")
      cfg <- reference_config()
      if (!all(cfg$selected %in% focal())) stop("Select reported outcomes from the current focal set.")
      record <- make_reference_record(display_oriented_estimates(reports()$all),
        cfg$family, cfg$selected, cfg$statistic, cfg$condition, cfg$justification,
        cfg$design_record, cfg$independent, cfg$claim, cfg$orientation)
      rv$reference <- list(config=cfg, record=record)
      log_event("reference_calculated", paste(record$result$k, "candidates;", record$result$m, "reported outcomes"))
    }, error=function(e) {rv$reference_error <- conditionMessage(e)})
  })
  observeEvent(input$clear_reference, {
    rv$reference <- NULL; rv$reference_error <- NULL
    log_event("reference_cleared")
  })
  output$reference_status <- renderUI({
    a <- result()
    if (identical(rv$settings$source, "demo")) return(p(class="muted",
      "GSS example: compare the estimates, ranks, and item meanings. These outcomes differ in content, response formats, and respondent groups; neither uniform reporting nor joint label symmetry has been established. This example therefore reports the comparison without a reference probability."))
    tagList(if (!is.null(rv$reference_error)) p(class="notice", rv$reference_error),
      if (!is.null(rv$reference) && is.null(current_reference())) p(class="notice", "Comparison settings changed. Recalculate to include a reference in the report."),
      p(class="muted", "Choose the comparison set and reported subset explicitly. The app records your justification; it cannot establish the assumption from the results."))
  })
  output$reference_result <- renderUI({
    r <- current_reference()
    if (is.null(r)) return(NULL)
    z <- r$result
    tagList(p(strong(sprintf("Reference p = %.6g", z$p_ref)),
      sprintf(" · Rank sum W = %s · %d reported / %d candidates", format(z$rank_sum), z$m, z$k)),
      p(sprintf("Exact enumeration: %s subsets. Smallest attainable p = %.6g. Attainable rate at alpha = .05: %.6g.", format(z$n_subsets, scientific=FALSE), z$minimum_reference, z$attainable_rate)),
      if (z$minimum_reference > .05) p("This set cannot reach the .05 cutoff. Its estimates, ranks, and reference value still describe the comparison."),
      p("Interpret this concentration alongside measurement quality, expected associations, comparison-set composition, and selection records. The recorded condition and calculation are included in Interpret & export."))
  })
  output$reference_report <- renderUI(tagList(lapply(reference_report_text(current_reference())[-1], p)))
  current_rank_pattern <- reactive(tryCatch(rank_pattern(result()$landscape, neighborhood(), focal()),
    error=function(e) list(text=paste("Rank comparison:", conditionMessage(e)))))
  output$rank_pattern <- renderUI(tagList(h4("What the observed ordering shows"), p(current_rank_pattern()$text),
    p("For defensible alternatives addressing the same claim, examine how much support depends on the measures selected. Related outcomes can clarify the explanation's scope or motivate follow-up.")))

  active_annotations <- reactive({
    a <- result(); x <- rv$annotations
    x[x$focal %in% focal() & x$item %in% a$measures & !x$item %in% focal(), , drop = FALSE]
  })
  reset_comparison_editor <- function() {
    updateSelectInput(session, "relation", selected = "unclassified")
    updateTextAreaInput(session, "relation_reason", value = "")
    updateCheckboxInput(session, "include_inputs", value = FALSE)
  }
  load_demo <- function() {
    rv$reference <- NULL; rv$reference_error <- NULL
    led <- audit_universe(DEMO$data, DEMO_ITEMS, "MobilityLag", own_lag = TRUE, max_missing = .2)
    ls <- fit_landscape(DEMO$data, DEMO_ITEMS, "MobilityLag", own_lag = TRUE, missing = "common")
    rv$result <- list(data = DEMO$data, landscape = ls, network = DEMO_NETWORK, metadata = DEMO$metadata, orientation=DEMO$orientation,
      measures = DEMO_ITEMS, eligibility = led, label = "GSS worked example", provenance = DEMO$provenance,
      input_provenance = audit_input_provenance(DEMO$data, DEMO_SOURCES, DEMO_ITEMS,
        "MobilityLag", own_lag = TRUE, missing = "common", primary = ls),
      model = list(predictor = "MobilityLag", covariates = character(), own_lag = TRUE, lag_suffix = "Lag", missing = "common", se_method = "classical"))
    rv$settings <- list(source = "demo"); rv$error <- NULL
    rv$annotations <- data.frame(focal = character(), item = character(), relation = character(), reason = character())
    reset_comparison_editor()
    updateSelectizeInput(session, "focal", choices = setNames(DEMO_ITEMS, DEMO_ITEMS), selected = c("Happy", "Trust", "Fair"))
    updateTextInput(session, "population", value = "GSS example · United States, 1972–2018")
    updateTextInput(session, "analysis_unit", value = "Year (annual aggregate)")
    updateTextInput(session, "network_unit", value = "Individual survey respondents (archived map)")
    updateTextAreaInput(session, "question", value = "Declines in U.S. residential mobility predict subsequent declines in happiness, trust in others, and perceived fairness.")
    updateTextAreaInput(session, "rationale", value = "Linked to the original study's theoretical model.")
    updateTextAreaInput(session, "interpretation", value = "")
    updateTextInput(session, "record", value = "")
    updateSelectInput(session, "timing", selected = "Retrospective reconstruction")
    log_event("loaded_example", "Live annual-model fits; archived respondent EGA")
  }
  load_demo()
  observeEvent(input$source_mode, {
    if (input$source_mode == "demo") load_demo() else {
      rv$annotations <- data.frame(focal = character(), item = character(), relation = character(), reason = character())
      reset_comparison_editor()
      updateTextAreaInput(session, "question", value = "")
      updateTextAreaInput(session, "rationale", value = "")
      updateTextAreaInput(session, "interpretation", value = "")
      updateTextInput(session, "record", value = "")
      updateTextInput(session, "population", value = "Not supplied")
      updateTextInput(session, "analysis_unit", value = "Not supplied")
      updateTextInput(session, "network_unit", value = "Same input rows unless a separate network file is supplied")
      updateSelectInput(session, "timing", selected = "Not documented here")
      if (!is.null(rv$upload)) updateSelectizeInput(session, "focal", choices = input$candidates, selected = head(input$candidates, 1))
    }
  }, ignoreInit = TRUE)
  handle_analysis_upload <- function(file) {
    reset_comparison_editor()
    tryCatch({
      d <- safe_csv(file$datapath); rv$upload <- d; rv$upload_label <- file$name
      rv$network_upload <- NULL; rv$metadata_upload <- NULL
      rv$network_id <- ""; rv$metadata_id <- ""; rv$inputs <- list()
      session$sendCustomMessage("resetOptionalFiles", c("network_csv", "metadata_csv"))
      rv$source_id <- unname(tools::md5sum(file$datapath))
      rv$inputs$analysis <- file$datapath
      nums <- names(d)[vapply(d, is.numeric, logical(1))]
      if (length(nums) < 5L) stop("Provide a numeric predictor and at least four numeric candidate outcomes.")
      pred <- if ("X" %in% nums) "X" else nums[1]
      cand <- setdiff(nums, c(pred, "year", "Year", "id", "ID", nums[endsWith(nums, "Lag")]))
      updateSelectInput(session, "predictor", choices = nums, selected = pred)
      updateSelectizeInput(session, "candidates", choices = nums, selected = cand)
      updateSelectizeInput(session, "covariates", choices = nums, selected = character())
      updateSelectizeInput(session, "focal", choices = cand, selected = head(cand, 1))
      updateTextInput(session, "population", value = "Not supplied")
      updateTextInput(session, "analysis_unit", value = "Not supplied")
      updateTextInput(session, "network_unit", value = "Same input rows unless a separate network file is supplied")
      updateTextAreaInput(session, "question", value = "")
      updateTextAreaInput(session, "rationale", value = "")
      updateTextAreaInput(session, "interpretation", value = "")
      updateTextInput(session, "record", value = "")
      updateSelectInput(session, "timing", selected = "Not documented here")
      rv$annotations <- rv$annotations[FALSE, ]; rv$error <- NULL
      log_event("analysis_file_loaded", file$name)
    }, error = function(e) {rv$upload <- NULL; rv$source_id <- paste("invalid", Sys.time()); rv$error <- conditionMessage(e)})
  }
  observeEvent(input$data_csv, handle_analysis_upload(input$data_csv))
  observeEvent(input$load_paste, {
    req(input$pasted_csv)
    if (nchar(input$pasted_csv, type = "bytes") > 5 * 1024^2) {rv$error <- "Pasted CSV is limited to 5 MB; use file upload for larger data."; return()}
    path <- tempfile("ampps_pasted_", fileext = ".csv")
    writeLines(input$pasted_csv, path, useBytes = TRUE)
    session$onSessionEnded(function() unlink(path))
    handle_analysis_upload(list(datapath = path, name = "Pasted CSV"))
  })
  observeEvent(input$network_csv, {
    updateCheckboxInput(session, "include_inputs", value = FALSE)
    tryCatch({rv$network_upload <- safe_csv(input$network_csv$datapath)
      rv$network_id <- unname(tools::md5sum(input$network_csv$datapath)); rv$inputs$network <- input$network_csv$datapath
      log_event("network_file_loaded", input$network_csv$name)
    }, error = function(e) {rv$network_upload <- NULL; rv$network_id <- "invalid"; rv$error <- conditionMessage(e)})
  })
  observeEvent(input$metadata_csv, {
    updateCheckboxInput(session, "include_inputs", value = FALSE)
    tryCatch({d <- safe_csv(input$metadata_csv$datapath)
      if (!("item" %in% names(d)) || anyDuplicated(d$item)) stop("Metadata needs a unique item column.")
      rv$metadata_upload <- d; rv$metadata_id <- unname(tools::md5sum(input$metadata_csv$datapath))
      rv$inputs$metadata <- input$metadata_csv$datapath
    }, error = function(e) {rv$metadata_upload <- NULL; rv$metadata_id <- "invalid"; rv$error <- conditionMessage(e)})
  })
  observeEvent(input$run, {
    session$sendCustomMessage("analysisBuildState", list(busy=TRUE))
    tryCatch({
      if (input$source_mode == "demo") {load_demo(); session$sendCustomMessage("navigateStage", list(stage="map")); return()}
      req(rv$upload)
      cf <- config(); d <- rv$upload
      if (rv$network_id == "invalid" || rv$metadata_id == "invalid") stop("Replace the invalid optional CSV before running.")
      if (length(cf$candidates) < 4L || length(cf$candidates) > 200L) stop("Select 4–200 candidate outcomes for EGA.")
      if (cf$predictor %in% cf$candidates || any(cf$covariates %in% cf$candidates)) stop("Predictor and covariates must be separate from outcome candidates.")
      withProgress(message = "Building the outcome comparison", value = 0, {
        led <- audit_universe(d, cf$candidates, cf$predictor, cf$covariates, cf$min_n, cf$max_missing, cf$own_lag, cf$lag_suffix)
        ms <- led$item[led$included]
        if (length(ms) < 4) stop("Fewer than four outcomes pass eligibility. Review coding, missingness and lag columns.")
        excluded_focal <- setdiff(input$focal %or% character(), ms)
        if (length(excluded_focal)) stop("A selected focal outcome is not eligible: ", paste(excluded_focal, collapse = ", "), ". Review eligibility or explicitly select another focal outcome.")
        incProgress(.1, "Fitting the common model")
        ls <- fit_landscape(d, ms, cf$predictor, cf$covariates, cf$own_lag, cf$lag_suffix, cf$missing, cf$se_method)
        incProgress(.15, if (cf$bootstrap) "Bootstrapping the outcome network" else "Estimating the outcome network")
        nd <- rv$network_upload %or% d
        net <- compute_ega(nd, ms, bootstrap = cf$bootstrap, iter = cf$iter, seed = cf$seed)
        meta <- rv$metadata_upload
        if (is.null(meta)) meta <- data.frame(item = ms, label = ms, domain = "Not supplied", respondent_scope = "Not supplied", coding_note = "As supplied; direction not independently verified")
        key <- orientation_from_metadata(meta, ms)
        rv$result <- list(data = d, landscape = ls, network = net, metadata = meta, orientation=key, measures = ms,
          eligibility = led, label = rv$upload_label, provenance = "User-supplied CSV; no imputation. Source and selection descriptions are user declarations.",
          model = cf[c("predictor", "covariates", "own_lag", "lag_suffix", "missing", "se_method")])
        rv$settings <- cf; rv$error <- NULL
        rv$reference <- NULL; rv$reference_error <- NULL
        sel <- intersect(isolate(input$focal), ms); if (!length(sel)) sel <- head(ms, 1)
        updateSelectizeInput(session, "focal", choices = ms, selected = sel)
        incProgress(.75, "Ready")
      })
      log_event("analysis_built", paste(length(rv$result$measures), "outcomes"))
      session$sendCustomMessage("navigateStage", list(stage="map"))
    }, error = function(e) {rv$error <- conditionMessage(e); showNotification(conditionMessage(e), type = "error", duration = NULL)},
      finally=session$sendCustomMessage("analysisBuildState", list(busy=FALSE)))
  })
  observeEvent(input$boundary, log_event("boundary_view", input$boundary), ignoreInit = TRUE)
  observeEvent(input$reset_boundary, {
    updateSelectInput(session, "boundary", selected = "community")
  }, ignoreInit = TRUE)
  observeEvent(input$focal, log_event("focal_view", paste(input$focal, collapse = ", ")), ignoreInit = TRUE)
  observe({
    f <- focal(); a <- result(); al <- setdiff(neighborhood(), f)
    old_focal <- isolate(input$annotation_focal)
    if (!((old_focal %or% "") %in% f)) old_focal <- head(f, 1)
    updateSelectInput(session, "annotation_focal", choices = f, selected = old_focal)
    old <- isolate(input$annotation_item); if (!((old %or% "") %in% al)) old <- head(al, 1)
    updateSelectInput(session, "annotation_item", choices = al, selected = old)
  })
  observeEvent(list(input$annotation_focal, input$annotation_item), {
    req(input$annotation_focal, input$annotation_item)
    x <- rv$annotations
    row <- x[x$focal == input$annotation_focal & x$item == input$annotation_item, , drop = FALSE]
    updateSelectInput(session, "relation", selected = if (nrow(row)) row$relation[1] else "unclassified")
    updateTextAreaInput(session, "relation_reason", value = if (nrow(row)) row$reason[1] else "")
  }, ignoreInit = TRUE)
  observeEvent(input$save_annotation, {
    req(input$annotation_focal, input$annotation_item)
    if (!(input$annotation_focal %in% focal()) || !(input$annotation_item %in% setdiff(neighborhood(), focal()))) {
      showNotification("Choose a current focal outcome and an unreported alternative from this boundary.", type = "warning"); return()
    }
    if (input$relation == "substitute" && !nzchar(trimws(input$relation_reason))) {
      showNotification("Add a measurement/population justification for a same-claim substitute.", type = "warning"); return()
    }
    row <- data.frame(focal = input$annotation_focal, item = input$annotation_item, relation = input$relation, reason = input$relation_reason)
    keep <- !(rv$annotations$focal == row$focal & rv$annotations$item == row$item)
    rv$annotations <- rbind(rv$annotations[keep, , drop = FALSE], row)
    log_event("comparison_note", paste(row$focal, row$item, row$relation))
    showNotification("Comparison note saved.", type = "message")
  })

  output$run_status <- renderUI({
    if (!is.null(rv$error)) return(div(class = "status error", strong("Cannot complete this run. "), rv$error))
    if (dirty()) return(div(class = "status pending", "Settings have changed. Build the analysis in Step 1; previous results are withheld."))
    div(class = "status ready", strong(rv$result$label), " · ", length(rv$result$measures), " outcomes · ",
        if (rv$settings$source == "demo") "Archived EGA + live model fits" else if (isTRUE(rv$settings$bootstrap)) "New bootstrap EGA + model fits" else "New EGA preview + model fits", " · Ready")
  })
  output$scope_summary <- renderUI({a <- result(); p(paste(sum(a$eligibility$included), "outcomes included."),
    paste(sum(!a$eligibility$included), "excluded by recorded eligibility criteria."))})
  output$eligibility_table <- renderDT(datatable(result()$eligibility, rownames = FALSE, options = table_opts))
  output$active_boundary <- renderUI({
    selected <- input$boundary %or% "community"
    label <- switch(selected, community = "EGA communities (default)",
      ring15 = "Distance ring · 15%", ring25 = "Distance ring · 25%",
      ring35 = "Distance ring · 35%", full = "Full analyzable set")
    tagList(p(strong("Current comparison")), p(label),
      p(class = "muted", paste(length(neighborhood()), "outcomes included.")),
      if (selected != "community") actionButton("reset_boundary", "Return to EGA communities"))
  })
  output$network_method <- renderUI({n <- result()$network
    tagList(p(strong(if (rv$settings$source == "demo") "Archived GSS EGA" else if (isTRUE(rv$settings$bootstrap)) "Bootstrap EGA · typical network" else "EGA preview · no bootstrap")),
      p(class = "muted", "EGA connects measures that covary and locates unreported measures to compare. Shared constructs, related domains, response formats, or respondent groups can contribute to these connections. Item content guides their interpretation."),
      help_box("Network source and stability", p(n$method), p(paste(n$provenance, collapse = " ")),
        if (length(n$warnings)) p(paste(n$warnings, collapse = " "))))})
  output$boundary_notes <- renderUI({
    nb <- neighborhoods()
    tagList(p(class = "muted", "Each cutoff uses the finite nonfocal distances to the nearest focal outcome; ties stay together."),
      if (length(nb$warnings)) p(class = "notice", paste(nb$warnings, collapse = " ")))
  })
  output$boundary_table <- renderDT({
    nb <- neighborhoods(); a <- result(); allsets <- nb$sets
    rows <- lapply(names(allsets), function(nm) {
      m <- allsets[[nm]]; base <- allsets$community
      cut <- nb$summary$cutoff[nb$summary$boundary == nm]
      pool <- nb$summary$pool_n[nb$summary$boundary == nm]
      data.frame(Boundary = nm, Outcomes = length(m), Cutoff = if (length(cut)) fmt(cut, 5) else "—",
        Distance_pool = if (length(pool)) pool else NA_integer_,
        Added_to_community = paste(setdiff(m, base), collapse = ", "),
        Outside_this_view = paste(setdiff(base, m), collapse = ", "))
    })
    datatable(do.call(rbind, rows), rownames = FALSE, options = modifyList(table_opts, list(paging = FALSE)))
  })
  output$neighbor_table <- renderDT({
    a <- result(); ms <- neighborhood()
    x <- data.frame(item = ms, focal = ms %in% focal(), community = unname(a$network$membership[ms]),
                    distance = unname(neighborhoods()$distances[ms]))
    x <- merge(x, a$metadata, by = "item", all.x = TRUE, sort = FALSE)
    if (!is.null(a$network$stability)) x <- merge(x, a$network$stability, by = "item", all.x = TRUE, sort = FALSE)
    datatable(x, rownames = FALSE, options = table_opts) %>% formatRound(intersect(c("distance", "stability"), names(x)), 3)
  })
  output$network_plot <- renderPlotly({
    a <- result(); net <- a$network; ids <- colnames(net$adjacency); xy <- net$coordinates[ids, , drop = FALSE]
    ed <- which(upper.tri(net$adjacency) & abs(net$adjacency) > 0, arr.ind = TRUE)
    ex <- as.vector(t(cbind(xy[ed[,1],1], xy[ed[,2],1], NA_real_)))
    ey <- as.vector(t(cbind(xy[ed[,1],2], xy[ed[,2],2], NA_real_)))
    map_metadata <- a$metadata
    key <- normalize_orientation_key(display_key(), map_metadata$item)
    map_metadata$coding_note <- paste0(ifelse(key$multiplier == -1, "RC: display direction reversed. ", ""),
      key$high_value_means, " [", key$status, "]")
    nodes <- network_display(net, map_metadata, focal(), neighborhood(), identical(rv$settings$source, "demo"))
    p <- plot_ly(source = "network", type = "scatter", mode = "lines", x = ex, y = ey,
      line = list(color = "#d2dbdd", width = .65), hoverinfo = "skip", showlegend = FALSE)
    for (group in sort(unique(nodes$community))) {
      at <- which(nodes$community == group)
      p <- add_trace(p, x = xy[at,1], y = xy[at,2], type = "scatter", mode = "markers+text", inherit = FALSE,
        name = paste0("Community ", group, " · ", length(at)), legendgroup = group,
        marker = list(size = nodes$size[at], color = nodes$color[at], symbol = nodes$symbol[at],
          line = list(color = nodes$outline[at], width = nodes$outline_width[at])),
        text = nodes$label[at], textposition = nodes$position[at], textfont = list(size = 11, color = "#284951"),
        customdata = ids[at], hovertext = nodes$hover[at], hoverinfo = "text", showlegend = TRUE,
        cliponaxis = FALSE)
    }
    x_span <- max(diff(range(xy[,1])), .1)
    layout(p, xaxis = list(visible = FALSE, range = range(xy[,1]) + c(-.18, .18) * x_span),
      yaxis = list(visible = FALSE, scaleanchor = "x"),
      legend = list(orientation = "h", x = .5, xanchor = "center", y = -.04,
        itemclick = FALSE, itemdoubleclick = FALSE),
      margin = list(l = 15, r = 15, b = 60, t = 30), paper_bgcolor = "white", plot_bgcolor = "white", dragmode = "pan") %>%
      plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })
  plot_data <- reactive({
    a <- result(); x <- display_oriented_estimates(reports()$all)
    if (input$result_scope == "local") x <- x[x$item %in% neighborhood(), , drop = FALSE]
    x <- x[is.finite(x$t), , drop = FALSE]
    x <- x[order(abs(x$t)), , drop = FALSE]
    facet_labels <- paste0(x$item,ifelse(x$orientation_reversed," [RC]",ifelse(startsWith(x$orientation_status,"raw_")," [original]","")))
    x$item_display <- factor(facet_labels, levels = facet_labels)
    x$readable_display <- factor(outcome_labels(x$item), levels = outcome_labels(x$item))
    x$role <- factor(ifelse(x$item %in% focal(), "Focal", "Unreported alternative"), levels = c("Focal", "Unreported alternative"))
    x$hover <- paste0(htmltools::htmlEscape(x$item), "<br>b = ", fmt(x$b, 4), "; SE = ", fmt(x$se, 4),
      "<br>95% interval [", fmt(x$ci_lo, 4), ", ", fmt(x$ci_hi, 4), "]<br>t = ", fmt(x$t), "; p = ", fmt(x$p, 5), "<br>n = ", x$n,
      "<br>", ifelse(x$orientation_reversed,"RC; ",""), htmltools::htmlEscape(x$high_value_means),
      "<br>Raw b = ",fmt(x$raw_b,4),"; raw t = ",fmt(x$raw_t))
    x
  })
  result_ggplot <- reactive({
    x <- plot_data(); validate(need(nrow(x), "No estimable results in this view."))
    if (input$plot_metric == "b") {
      g <- ggplot(x, aes(y = 0, color = role)) + geom_vline(xintercept = 0, color = "#aebabb", linewidth = .4) +
        geom_segment(aes(x = ci_lo, xend = ci_hi, yend = 0), linewidth = .65) +
        geom_point(aes(x = b, text = hover), size = 2.7) + facet_wrap(~item_display, scales = "free_x", ncol = 2) +
        scale_y_continuous(breaks = NULL) + labs(x = "Displayed estimate and 95% interval · separate outcome axes")
    } else if (input$plot_metric == "abs_t") g <- ggplot(x, aes(y = readable_display, color = role)) +
      geom_segment(aes(x = 0, xend = abs(t), yend = readable_display), color = "#d8e1e2", linewidth = .45) +
      geom_point(aes(x = abs(t), text = hover, shape = role), size = 3.2, stroke = 1.2) +
      scale_shape_manual(values = c("Focal" = 16, "Unreported alternative" = 1)) +
      scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(.01, .08))) +
      labs(x = "Absolute t-statistic |t| · estimate relative to SE", shape = NULL)
    else g <- ggplot(x, aes(y = readable_display, color = role)) + geom_vline(xintercept = 0, color = "#aebabb", linewidth = .4) +
      geom_point(aes(x = t, text = hover), size = 2.8) + labs(x = "Displayed signed t · ordered by absolute t")
    g + scale_color_manual(values = c("Focal" = ACCENT, "Unreported alternative" = "#778b91")) +
      labs(y = NULL, color = NULL) + theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom",
            plot.margin = margin(12, 20, 12, 8), axis.text.y = element_text(color = "#213f48"))
  })
  output$landscape_holder <- renderUI({
    n <- nrow(plot_data())
    height <- if (input$plot_metric == "b") max(440, ceiling(n / 2) * 140) else max(460, n * 23 + 80)
    plotlyOutput("landscape_plot", height = paste0(height, "px"))
  })
  output$landscape_plot <- renderPlotly(ggplotly(result_ggplot(), tooltip = "text") %>% plotly::config(displaylogo = FALSE))
  distribution_data <- reactive({
    x <- result()$landscape
    x <- x[is.finite(x$t), , drop=FALSE]
    x$group <- ifelse(x$item %in% focal(), "Reported", ifelse(x$item %in% neighborhood(), "Unreported neighbors", "Other outcomes"))
    x$hover <- paste0(htmltools::htmlEscape(x$item), "<br>|t| = ", fmt(abs(x$t)))
    x
  })
  output$distribution_plot <- renderPlotly({
    x <- distribution_data()
    colors <- c("Reported"=ACCENT, "Unreported neighbors"="#778b91", "Other outcomes"="#afb9bd")
    p <- plotly::plot_ly()
    for (group in names(colors)) {
      rows <- x[x$group==group,,drop=FALSE]
      if (!nrow(rows)) next
      p <- plotly::add_boxplot(p, y=abs(rows$t), name=group, text=rows$hover,
        hoverinfo="text", boxpoints="all", jitter=.3, pointpos=0, quartilemethod="linear",
        line=list(color=unname(colors[group])), marker=list(color=unname(colors[group]),size=6),
        showlegend=FALSE)
    }
    plotly::layout(p, yaxis=list(title="Absolute t", rangemode="tozero"),
      xaxis=list(title="",categoryorder="array",categoryarray=names(colors)),
      margin=list(l=55,r=15,t=15,b=65),paper_bgcolor="white",plot_bgcolor="white") %>%
      plotly::config(displaylogo=FALSE)
  })
  output$combined_estimates <- renderUI({
    x <- plot_data(); x <- x[rev(seq_len(nrow(x))), , drop=FALSE]
    validate(need(nrow(x), "No estimable results in this view."))
    upper <- max(1, ceiling(max(abs(x$t), na.rm=TRUE)))
    dot <- function(value, focal_flag) {
      pos <- 5 + 130 * abs(value)/upper
      htmltools::HTML(sprintf('<svg viewBox="0 0 142 24" width="142" height="24" aria-label="absolute t %.2f"><line x1="5" y1="12" x2="135" y2="12" stroke="#c7d2d5" stroke-width="1"/><circle cx="%.2f" cy="12" r="4.3" fill="%s" stroke="#24545f" stroke-width="1.7"/></svg>', abs(value),pos,if(focal_flag) "#24545f" else "white"))
    }
    tags$table(class="reading-table combined-table",tags$thead(
      tags$tr(tags$th("Outcome / higher-value meaning"),tags$th(paste0("|t|: 0 to ",upper)),tags$th("|t|"),tags$th("Coefficient b"),tags$th("95% interval"))),
      tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(class=if(x$item[i]%in%focal()) "focal-reading" else "",
        tags$td(outcome_labels(x$item[i]), tags$small(style="display:block;font-weight:normal",x$high_value_means[i])), tags$td(dot(x$t[i],x$item[i]%in%focal())),tags$td(fmt(abs(x$t[i]),2)),
        tags$td(fmt(x$b[i],4)),tags$td(paste0("[",fmt(x$ci_lo[i],4),", ",fmt(x$ci_hi[i],4),"]"))))))
  })
  output$reading_estimates <- renderUI({
    x <- plot_data(); x <- x[rev(seq_len(nrow(x))), , drop=FALSE]
    tags$table(class="reading-table", tags$thead(tags$tr(tags$th("Outcome"), tags$th("Role"),
      tags$th("Coefficient b"), tags$th("95% interval"), tags$th("Displayed t"))),
      tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(
        tags$td(outcome_labels(x$item[i]),tags$small(style="display:block",x$high_value_means[i])), tags$td(as.character(x$role[i])),
        tags$td(fmt(x$b[i],4)), tags$td(paste0("[",fmt(x$ci_lo[i],4),", ",fmt(x$ci_hi[i],4),"]")),
        tags$td(fmt(x$t[i],2))))))
  })
  output$completed_comparison <- renderUI({
    a <- result(); selected <- unique(c(focal(), active_annotations()$item))
    x <- display_oriented_estimates(reports()$all)
    x <- x[x$item %in% selected, , drop=FALSE]
    tags$div(
      p(class="reading-intro", paste0("Current neighborhood: ", length(neighborhood()), " outcomes; ", length(focal()), " focal outcomes.")),
      tags$table(class="reading-table", tags$thead(tags$tr(tags$th("Outcome"), tags$th("Role"), tags$th("|t|"), tags$th("95% interval"))),
        tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(tags$td(outcome_labels(x$item[i]),tags$small(style="display:block",x$high_value_means[i])),
          tags$td(if (x$item[i] %in% focal()) "Focal" else if (x$item[i] %in% neighborhood()) "Unreported neighbor" else "Other unreported alternative"),
          tags$td(fmt(abs(x$t[i]),2)), tags$td(paste0("[",fmt(x$ci_lo[i],4),", ",fmt(x$ci_hi[i],4),"]")))))),
      h4("Measurement comparison (researcher's note)"),
      if (nrow(active_annotations())) tagList(lapply(seq_len(nrow(active_annotations())), function(i) {
        z <- active_annotations()[i,]; p(paste0(toupper(z$focal)," / ",toupper(z$item),": ",z$reason))
      })) else p(class="muted","Choose an alternative and save its measurement context below."),
      h4("What the comparison adds (researcher's interpretation)"),
      if (nzchar(trimws(input$interpretation %or% ""))) p(class="completed-interpretation", input$interpretation) else
        p(class="muted", "Write the interpretation below; it will appear here and in the exported report."))
  })
  output$focal_table <- renderDT(datatable(display_estimates(reports()$focal), rownames = FALSE, options = modifyList(table_opts, list(paging = FALSE))))
  output$download_example <- downloadHandler(filename = function() "ega_upload_example.csv",
    content = function(file) file.copy(file.path("examples", "upload_example.csv"), file, overwrite = TRUE))
  output$download_example_metadata <- downloadHandler(filename = function() "ega_example_descriptions.csv",
    content = function(file) file.copy(file.path("examples", "metadata_example.csv"), file, overwrite = TRUE))
  output$landscape_table <- renderDT(datatable(display_estimates(reports()$all, all = TRUE), rownames = FALSE, options = modifyList(table_opts, list(dom = "ftip"))))
  output$input_sensitivity_panel <- renderUI({
    audit <- result()[["input_provenance"]]
    if (is.null(audit)) return(NULL)
    div(class = "panel-card", help_box("Annual input sensitivity · GSS example",
      p("The primary comparison keeps the supplied annual inputs fixed. This separate check uses only rows whose current outcome was recovered from a survey extract."),
      p("Inherited one-year lag values can remain. Row counts describe provenance, not effective sample size; this is not a correction for serial dependence or interpolation uncertainty."),
      DTOutput("input_sensitivity_table"),
      p(class = "muted", "All 44 outcomes, exact source counts, and a script to reproduce this check are included in the GSS export. Bonferroni columns appear here when the optional check is selected.")))
  })
  output$input_sensitivity_table <- renderDT({
    audit <- result()[["input_provenance"]]; req(audit)
    ms <- focal()
    block <- function(x, label, restricted = FALSE) {
      x <- display_oriented_estimates(orient_landscape(x, display_key()))
      x <- x[match(ms, x$item), , drop = FALSE]
      info <- audit$summary[match(ms, audit$summary$item), , drop = FALSE]
      tab <- data.frame(Outcome = paste0(x$item,ifelse(x$orientation_reversed," [RC]","")), Inputs = label, n = x$n,
        `Inherited own-lags` = if (restricted) info$restricted_own_lag_carried_n else info$primary_own_lag_carried_n,
        b = round(x$b, 5), `95% interval` = paste0("[", fmt(x$ci_lo, 5), ", ", fmt(x$ci_hi, 5), "]"),
        p = signif(x$p, 4), check.names = FALSE)
      if (isTRUE(input$multiplicity)) tab$`p × universe k` <- signif(x$p_bonferroni_universe, 4)
      tab
    }
    datatable(rbind(block(audit$current_landscape, "Supplied"),
                    block(audit$restricted_landscape, "Current-outcome extract", TRUE)),
      rownames = FALSE, options = modifyList(table_opts, list(paging = FALSE)))
  })
  output$annotations_table <- renderDT(datatable(active_annotations(), rownames = FALSE, options = list(dom = "t", scrollX = TRUE)))

  report_text <- reactive({
    a <- result(); ls <- display_oriented_estimates(reports()$all); nb <- neighborhood(); fs <- focal()
    txt <- c("# AMPPS outcome-comparison record", "", paste("Dataset:", a$label),
      paste("Question:", input$question), paste("Population / period:", input$population),
      paste("Analysis unit:", input$analysis_unit), paste("Network unit:", input$network_unit), "",
      paste("We estimated", length(a$measures), "outcomes with predictor", a$model$predictor,
        if (length(a$model$covariates)) paste("and covariates", paste(a$model$covariates, collapse = ", ")) else "",
        if (isTRUE(a$model$own_lag)) "plus each outcome's supplied lag." else "with an intercept."),
      paste("Missingness:", a$model$missing, "; uncertainty:", a$model$se_method, "."),
      paste("The", input$boundary, "view contains", length(nb), "outcomes; focal outcomes:", paste(fs, collapse = ", "), "."),
      paste("Map:", a$network$method), "", "## What the comparison shows", "")
    peers <- ls[ls$item %in% setdiff(nb, fs) & is.finite(ls$t), , drop = FALSE]
    for (f in fs) {
      r <- ls[ls$item == f, , drop = FALSE]
      if (!nrow(r) || !is.finite(r$t)) {txt <- c(txt, paste("-", f, ": model not estimable; see status in results CSV.")); next}
      higher <- peers$item[abs(peers$t) > abs(r$t)]
      excludes <- r$ci_lo > 0 || r$ci_hi < 0
      txt <- c(txt, paste0("- ", f, if (r$orientation_reversed) " [RC]" else "", ": b = ", fmt(r$b, 4), ", SE = ", fmt(r$se, 4), ", 95% interval [", fmt(r$ci_lo, 4), ", ", fmt(r$ci_hi, 4),
        "], t = ", fmt(r$t), ", p = ", fmt(r$p, 5), ", n = ", r$n, ". The interval ", if (excludes) "excludes" else "includes", " zero. ",
        length(higher), if (length(higher) == 1L) " unreported outcome in this view has larger absolute t" else " unreported outcomes in this view have larger absolute t", if (length(higher)) paste0(": ", paste(higher, collapse = ", ")) else "", ". Higher-value meaning: ",r$high_value_means," (",r$orientation_status,")."))
    }
    txt <- c(txt, "", current_rank_pattern()$text, "", paste0("Display direction: ", if (identical(input$orientation_mode,"original")) "Original coding" else "Documented semantic key", ". These are observed estimates using the selected display direction. RC marks reversed coefficients, t statistics and intervals; raw fits remain in the downloads. Items without a verified or explicitly declared key retain original coding, as do all items when Original coding is selected. Absolute t and p-values do not change under a sign reversal; absolute t orders estimates relative to their standard errors; coefficients and intervals show magnitude and direction."),
      "", "## Boundary comparison", "The regression estimates are unchanged across these views.")
    sets <- neighborhoods()$sets
    for (nm in names(sets)) txt <- c(txt, paste0("- ", nm, " (", length(sets[[nm]]), "): ", paste(sets[[nm]], collapse = ", ")))
    if (isTRUE(input$multiplicity)) txt <- c(txt, "", "Optional Bonferroni sensitivities are provided for the named current boundary and the full analyzable set. They do not correct coefficient inflation or validate the model/family.")
    audit <- a[["input_provenance"]]
    if (!is.null(audit)) {
      txt <- c(txt, "", "## Annual input sensitivity (separate from the outcome comparison)",
        "The primary estimates above use the supplied annual inputs. The following refits retain only primary rows whose current outcome is labeled extract; supplied predictor and one-year lag values are unchanged.")
      for (f in fs) {
        oriented_restricted <- display_oriented_estimates(orient_landscape(audit$restricted_landscape, display_key()))
        r <- oriented_restricted[oriented_restricted$item == f, , drop = FALSE]
        counts <- audit$summary[audit$summary$item == f, , drop = FALSE]
        txt <- c(txt, paste0("- ", f, ": ", counts$restricted_n, " of ", counts$primary_n,
          " primary rows retained; ", counts$restricted_own_lag_carried_n, " inherited own-lag values remain. ",
          "b = ", fmt(r$b, 5), ", 95% interval [", fmt(r$ci_lo, 5), ", ", fmt(r$ci_hi, 5), "]; p = ", fmt(r$p, 6),
          if (isTRUE(input$multiplicity)) paste0("; p × ", r$bonferroni_universe_k, " = ", fmt(r$p_bonferroni_universe, 4)) else "", "."))
      }
      txt <- c(txt, "These are provenance counts, not effective sample sizes. This input restriction can change the fitted years and results; it does not correct serial dependence or inherited-input uncertainty.")
    }
    anns <- active_annotations()
    txt <- c(txt, "", "## Measurement comparisons (user annotations)")
    if (!nrow(anns)) txt <- c(txt, "No conceptual substitutes have been certified. Empirical neighbors remain unclassified unless a comparison is recorded.")
    else for (i in seq_len(nrow(anns))) txt <- c(txt, paste0("- ", anns$focal[i], " / ", anns$item[i], " — ", anns$relation[i], ": ", anns$reason[i]))
    c(txt, "", reference_report_text(current_reference()), "", "## Interpretation (user declaration)", input$interpretation %or% "", "", "## Selection and source record",
      paste("Rationale:", input$rationale), paste("Timing declaration:", input$timing), paste("Record:", input$record),
      paste(a$provenance, collapse = " "), "", paste("App version:", APP_VERSION),
      "This is a record of the present analysis, not a preregistration or a reconstruction of all past analysis choices.")
  })
  report_html <- reactive({
    txt <- report_text()
    blocks <- lapply(txt, function(s) {
      if (startsWith(s, "# ")) tags$h1(substring(s, 3)) else if (startsWith(s, "## ")) tags$h2(substring(s, 4))
      else if (nzchar(s)) tags$p(s) else NULL
    })
    plain_table <- function(d) {
      d <- display_oriented_estimates(d)
      d$item <- paste0(d$item,ifelse(d$orientation_reversed," [RC]",""))
      use <- intersect(c("item", "high_value_means", "b", "se", "ci_lo", "ci_hi", "t", "p", "n", "p_bonferroni_neighborhood", "p_bonferroni_universe"), names(d))
      d <- d[, use, drop = FALSE]
      tags$table(tags$thead(tags$tr(lapply(names(d), tags$th))), tags$tbody(lapply(seq_len(nrow(d)), function(i)
        tags$tr(lapply(d[i, , drop = FALSE], function(v) tags$td(if (is.numeric(v)) fmt(v, 4) else as.character(v)))))))
    }
    tags$html(tags$head(tags$meta(charset = "utf-8"), tags$title("AMPPS comparison record"),
      tags$style("body{font:16px Georgia,serif;max-width:1100px;margin:50px auto;padding:0 24px;color:#202020;line-height:1.6}h1{font-size:27px}h2{font-size:20px;border-top:1px solid #999;padding-top:18px}p{overflow-wrap:anywhere}table{border-collapse:collapse;font-size:11px;width:100%}th{border-top:1px solid;border-bottom:1px solid}td,th{padding:5px;text-align:left;overflow-wrap:anywhere}tr:last-child td{border-bottom:1px solid}@media print{body{margin:0;max-width:none}h2{break-after:avoid}}")),
      tags$body(blocks, tags$h2("Focal estimates"), plain_table(reports()$focal), tags$h2("Unreported alternatives in this view"), plain_table(reports()$alternatives),
        tags$p("b and t use the selected display direction; RC marks reversal and high_value_means defines the direction. se = standard error; ci_lo/ci_hi = correctly ordered individual 95% interval endpoints; n = fitted rows. Raw fits and the orientation key remain in CSV exports. p is the unchanged two-sided regression p-value. Optional p_bonferroni columns name their family; they are not coefficient corrections.")))
  })
  output$report_preview <- renderUI({
    txt <- report_text(); end <- which(txt == "## Boundary comparison")[1]
    tagList(lapply(txt[seq.int(10, end - 1)], function(s) if (startsWith(s, "## ")) h4(substring(s, 4)) else if (nzchar(s)) p(s)))
  })
  output$provenance_panel <- renderUI({a <- result(); help_box("Sources and software", p(paste(a$provenance, collapse = " ")),
    p(paste(a$network$provenance, collapse = " ")), p(paste("Current runtime:", R.version.string, "· EGAnet", as.character(packageVersion("EGAnet")))),
    p("The app runs on the host serving this page; a localhost address uses your computer. Exports record the runtime and measurement context."))})
  payload <- reactive({
    a <- result()
    list(landscape = a$landscape, eligibility = a$eligibility, report = reports(), metadata = a$metadata,
      orientation = normalize_orientation_key(display_key(),a$measures),
      documented_orientation_key = normalize_orientation_key(a$orientation,a$measures),
      network = a$network, neighborhoods = neighborhoods(), annotations = active_annotations(),
      input_provenance = a[["input_provenance"]],
      concentration_reference = current_reference(),
      settings = list(app_version = APP_VERSION, source = a$label, run = rv$settings, model = c(a$model, list(measures = a$measures)),
        focal = focal(), boundary = input$boundary, multiplicity = isTRUE(input$multiplicity), orientation_mode=input$orientation_mode %or% "documented",
        question = input$question, population = input$population, analysis_unit = input$analysis_unit,
        network_unit = input$network_unit, rationale = input$rationale, selection_timing_declaration = input$timing,
        dated_record = input$record, interpretation = input$interpretation, coding = "Raw inputs/fits retained; documented semantic key applied only to displayed estimates; unverified items retain original coding",
        network_input_separate = !is.null(rv$network_upload) && rv$settings$source == "upload",
        events = rv$events, provenance = a$provenance), reporting_text = report_text())
  })
  output$download_bundle <- downloadHandler(filename = function() paste0("AMPPS_comparison_", Sys.Date(), ".zip"), content = function(file) {
    a <- result()
    inputs <- NULL
    if (rv$settings$source == "demo") inputs <- c(analysis_input.csv = file.path("data", "gss_year.csv"))
    else if (isTRUE(input$include_inputs)) {inputs <- unlist(rv$inputs); names(inputs) <- paste0(names(inputs), "_input.csv")}
    export_bundle(file, payload(), data_files = inputs)
  })
  output$download_report <- downloadHandler(filename = function() "AMPPS_comparison_report.html", content = function(file) writeLines(as.character(report_html()), file, useBytes = TRUE))
  output$download_csv <- downloadHandler(filename = function() "AMPPS_all_estimates.csv", content = function(file) write.csv(spreadsheet_safe_frame(reports()$all), file, row.names = FALSE, na = ""))
  output$download_plot <- downloadHandler(filename = function() "AMPPS_result_landscape.pdf", content = function(file) {
    ph <- if (input$plot_metric == "b") max(5, ceiling(nrow(plot_data()) / 2) * 1.3) else max(5, nrow(plot_data()) * .22 + 1.5)
    ggsave(file, plot = result_ggplot(), device = "pdf", width = 9, height = ph)
  })
}

shinyApp(ui, server)
