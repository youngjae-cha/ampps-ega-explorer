# Self-contained local export. Raw user rows require an explicit data_files argument.
.ampps_export_source <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork=TRUE), error=function(e) "")

spreadsheet_safe_frame <- function(x) {
  x <- as.data.frame(x, check.names=FALSE)
  for (j in seq_along(x)) {
    if (is.factor(x[[j]])) x[[j]] <- as.character(x[[j]])
    if (is.character(x[[j]])) x[[j]] <- sub("^([[:space:]]*[=+@-])", "'\\1", x[[j]])
  }
  names(x) <- sub("^([[:space:]]*[=+@-])", "'\\1", names(x))
  x
}

export_bundle <- function(path, payload, data_files=NULL) {
  if (!requireNamespace("jsonlite", quietly=TRUE)) stop("jsonlite is required for a reproducible export.")
  if (!requireNamespace("zip", quietly=TRUE)) stop("The R package zip is required for a reproducible export.")
  if (!is.list(payload) || is.null(payload$landscape) || is.null(payload$settings)) stop("Export needs landscape and settings.")
  if (length(path) != 1L || !nzchar(path) || !dir.exists(dirname(path))) stop("Choose an existing output directory.")
  target <- file.path(normalizePath(dirname(path), mustWork=TRUE), basename(path))
  archive <- tempfile("ampps_bundle_", fileext=".zip")
  on.exit(unlink(archive), add=TRUE)
  stage <- tempfile("ampps_export_"); dir.create(stage)
  on.exit(unlink(stage, recursive=TRUE), add=TRUE)
  csv <- function(x, name, row.names=FALSE) {
    if (is.null(x)) return(invisible(NULL))
    x <- spreadsheet_safe_frame(x)
    write.csv(x, file.path(stage,name), row.names=row.names, na="")
  }
  csv(payload$landscape, "landscape.csv")
  csv(payload$eligibility, "eligibility_ledger.csv")
  csv(payload$metadata, "item_annotations.csv")
  orientation <- payload[["orientation"]]
  csv(orientation, "orientation_key.csv")
  csv(payload[["documented_orientation_key"]], "documented_orientation_key.csv")
  csv(payload$annotations, "comparison_annotations.csv")
  input_audit <- payload[["input_provenance"]]
  if (!is.null(input_audit)) {
    csv(input_audit$summary, "input_source_counts.csv")
    csv(input_audit$current_landscape, "input_sensitivity_supplied.csv")
    csv(input_audit$restricted_landscape, "input_sensitivity_extract.csv")
    writeLines(input_audit$method_note, file.path(stage,"input_sensitivity_notes.txt"))
  }
  reports <- payload[["report"]]
  if (!is.null(reports)) for (key in intersect(names(reports), c("focal", "alternatives", "all"))) csv(reports[[key]], paste0("report_",key,".csv"))
  settings <- payload$settings
  concentration <- payload[["concentration_reference"]]
  if (!is.null(concentration)) {
    settings$concentration_reference <- concentration
    csv(concentration$statistics, "reference_statistics.csv")
    csv(concentration$result$distribution, "reference_distribution.csv")
    jsonlite::write_json(concentration, file.path(stage, "reference.json"),
      auto_unbox=TRUE, pretty=TRUE, null="null", digits=NA)
  }
  if (!is.null(orientation)) settings$orientation <- orientation
  # Machine-readable settings preserve literal identifiers; spreadsheet-safe CSV labels may be escaped.
  if (is.null(settings$model)) settings$measures <- as.character(payload$landscape$item) else settings$model$measures <- as.character(payload$landscape$item)
  settings$export <- list(timestamp_utc=format(Sys.time(), tz="UTC", usetz=TRUE), app="AMPPS Explorer", raw_inputs_included=!is.null(data_files) && length(data_files)>0, csv_text_formula_escape="Leading =,+,-,@ in text cells/headers receives a single apostrophe in result CSVs; numeric cells and explicitly included input files are unchanged.")
  if (!is.null(payload$network)) {
    net <- payload$network
    matrix_csv <- function(mat, filename) { if (!is.null(mat)) csv(data.frame(item=rownames(mat), mat, check.names=FALSE), filename) }
    matrix_csv(net$adjacency,"network_adjacency.csv")
    matrix_csv(net$correlation,"outcome_correlation.csv")
    matrix_csv(net$original_network,"network_original_weights.csv")
    if (!is.null(net$membership)) csv(data.frame(item=names(net$membership),community=unname(net$membership)),"network_membership.csv")
    csv(net$stability,"network_item_stability.csv")
    if (!is.null(net$coordinates)) csv(data.frame(item=rownames(net$coordinates),net$coordinates),"network_coordinates.csv")
    settings$network_record <- list(method=net$method,provenance=net$provenance,n=net$n,warnings=net$warnings,edge_source=net$edge_source,membership_source=net$membership_source)
    settings$network <- net$settings
    settings$network$measures <- names(net$membership)
    settings$network$row_order <- rownames(net$adjacency)
    settings$network$column_order <- colnames(net$adjacency)
  }
  if (!is.null(payload$neighborhoods)) {
    csv(payload$neighborhoods$summary,"boundary_summary.csv")
    if (!is.null(payload$neighborhoods$distances)) csv(data.frame(item=names(payload$neighborhoods$distances),distance=unname(payload$neighborhoods$distances)),"nearest_focal_distances.csv")
    settings$neighborhood_sets <- payload$neighborhoods$sets
  }
  if (!is.null(payload$provenance)) settings$provenance <- payload$provenance
  if (!is.null(input_audit)) settings$input_sensitivity <- input_audit$settings
  if (!is.null(data_files) && length(data_files)) {
    if (is.null(names(data_files)) || any(!nzchar(names(data_files))) || anyDuplicated(names(data_files))) stop("Explicit inputs must be named: analysis_input.csv and optionally network_input.csv.")
    safe <- basename(names(data_files))
    if (any(safe != names(data_files)) || any(!grepl("^[A-Za-z0-9_.-]+\\.csv$",safe)) || any(grepl("^\\.",safe))) stop("Input archive names must be simple CSV basenames.")
    if (any(!file.exists(data_files))) stop("An explicitly included input file is missing.")
    dir.create(file.path(stage,"inputs"))
    for (i in seq_along(data_files)) if (!file.copy(data_files[[i]],file.path(stage,"inputs",safe[[i]]))) stop("Could not copy explicitly included input.")
    settings$export$input_files <- setNames(file.path("inputs",safe), names(data_files))
  }
  jsonlite::write_json(settings,file.path(stage,"settings.json"),auto_unbox=TRUE,pretty=TRUE,null="null",digits=16,na="null")
  report_text <- if (!is.null(payload$reporting_text)) payload$reporting_text else "See report_focal.csv and report_alternatives.csv. Descriptive positions do not identify selection history or researcher intent."
  writeLines(report_text,file.path(stage,"reporting_text.txt"),useBytes=TRUE)
  blocks <- vapply(report_text,function(line) {
    tag <- if (startsWith(line,"## ")) "h2" else if (startsWith(line,"# ")) "h1" else "p"
    body <- if (tag=="h2") substring(line,4L) else if (tag=="h1") substring(line,3L) else line
    paste0("<",tag,">",htmltools::htmlEscape(body),"</",tag,">")
  },character(1))
  html_table <- function(tab,title) {
    if (is.null(tab) || !nrow(tab)) return("")
    if ("oriented_b" %in% names(tab)) {
      for (field in c("b","t","ci_lo","ci_hi")) tab[[field]] <- tab[[paste0("oriented_",field)]]
      tab$item <- paste0(tab$item,ifelse(tab$orientation_reversed," [RC]",""))
    }
    cols<-intersect(c("item","label","high_value_means","n","b","se","ci_lo","ci_hi","t","p","p_bonferroni_neighborhood","p_bonferroni_universe"),names(tab))
    head<-paste0("<tr>",paste0("<th>",htmltools::htmlEscape(cols),"</th>",collapse=""),"</tr>")
    rows<-vapply(seq_len(nrow(tab)),function(i) {
      cells<-vapply(cols,function(nm) {
        x<-tab[[nm]][i]
        text<-if (is.na(x)) "—" else if (is.numeric(x)) format(x,digits=5,trim=TRUE) else as.character(x)
        paste0("<td>",htmltools::htmlEscape(text),"</td>")
      },character(1))
      paste0("<tr>",paste(cells,collapse=""),"</tr>")
    },character(1))
    paste0("<h2>",htmltools::htmlEscape(title),"</h2><div class='table-scroll'><table><thead>",head,"</thead><tbody>",paste(rows,collapse=""),"</tbody></table></div>")
  }
  report_tables <- if (!is.null(reports)) c(html_table(reports$focal,"Focal estimates"),html_table(reports$alternatives,"Unreported estimates in the current comparison"),
    "<p>Note. b and t use the recorded display direction, with RC marking reversed directions and high_value_means stating their meaning. Raw coefficients, t statistics and interval endpoints remain in landscape.csv and report_all.csv; oriented_* columns give displayed values. SE is unchanged; ci_lo and ci_hi are correctly ordered individual 95% interval limits; p is the unchanged two-sided model p-value. Optional Bonferroni columns use the declared current-neighborhood and full-universe counts. These are sensitivity checks, not coefficient-bias corrections.</p>") else character()
  writeLines(c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>AMPPS comparison record</title>',
    '<style>body{font:16px Georgia,serif;max-width:1100px;margin:45px auto;padding:0 24px;color:#222;line-height:1.6}h1{font-size:27px}h2{font-size:20px;border-top:1px solid #aaa;padding-top:16px}p{overflow-wrap:anywhere}.table-scroll{overflow-x:auto}table{border-collapse:collapse;font-size:12px;width:100%;border-top:1px solid #222;border-bottom:1px solid #222}th{text-align:left;border-bottom:1px solid #222}td,th{padding:6px;vertical-align:top}@media print{body{margin:0;max-width:none}}</style></head><body>',
    blocks,report_tables,'</body></html>'),file.path(stage,"report.html"),useBytes=TRUE)
  writeLines(capture.output(sessionInfo()),file.path(stage,"session_info.txt"))
  source_dir <- if (nzchar(.ampps_export_source)) dirname(.ampps_export_source) else file.path(getwd(),"R")
  for (record in c("renv.lock", "package_versions.csv")) {
    src <- file.path(dirname(source_dir), record)
    if (file.exists(src) && !file.copy(src, file.path(stage, record)))
      stop("Cannot include software version record: ", record)
  }
  if (identical(settings[["network"]]$source,"bundled_demo")) {
    cell_source <- file.path(dirname(source_dir),"data","gss_year_cell_source.csv")
    if (!file.exists(cell_source)) stop("Bundled GSS cell-source provenance file is missing.")
    dir.create(file.path(stage,"provenance"))
    if (!file.copy(cell_source,file.path(stage,"provenance","gss_year_cell_source.csv"))) stop("Cannot bundle GSS cell-source provenance.")
    writeLines("Cell-source labels record the inherited extract/carried distinction. They are not an identified or validated interpolation algorithm; no additional imputation is performed by this app.",file.path(stage,"provenance","README.txt"))
  }
  dir.create(file.path(stage,"R"))
  for (module in c("model.R","ega.R", if (!is.null(input_audit)) "provenance.R")) {
    src <- file.path(source_dir,module)
    if (!file.exists(src)) stop(paste("Cannot export reproducibility module:",module))
    file.copy(src,file.path(stage,"R",module))
  }
  if (!is.null(concentration)) {
    src <- file.path(source_dir, "concentration.R")
    if (!file.copy(src, file.path(stage, "R", "concentration.R")))
      stop("Cannot include the concentration calculation in the export.")
    writeLines(c(
      "# Reproduce the conditional reference from the recorded fixed statistics.",
      "# Run in the unzipped directory: Rscript reproduce_reference.R",
      "source('R/concentration.R')",
      "ref <- jsonlite::read_json('reference.json', simplifyVector=TRUE)",
      "s <- setNames(ref$statistics$statistic, ref$statistics$item)",
      "out <- exact_concentration(s, as.character(unlist(ref$selected)), max_subsets=max(200000, ref$result$n_subsets), alpha=ref$result$alpha)",
      "fields <- c('rank_sum','p_ref','minimum_reference','attainable_rate','n_subsets')",
      "stopifnot(isTRUE(all.equal(unlist(out[fields]), unlist(ref$result[fields]), tolerance=1e-12, check.attributes=FALSE)))",
      "stopifnot(isTRUE(all.equal(out$distribution, ref$result$distribution, tolerance=1e-12, check.attributes=FALSE)))",
      "write.csv(out$distribution, 'reference_distribution_reproduced.csv', row.names=FALSE)",
      "cat('PASS: rank sum, reference p, tied minimum, attainable rate, and full distribution reproduced.\\n')",
      "# The reporting/symmetry condition and independent-specification rationale",
      "# are researcher declarations in reference.json; reproducing arithmetic",
      "# does not verify those declarations."
    ), file.path(stage, "reproduce_reference.R"))
  }
  repro <- c(
    "# Run from this unzipped directory: Rscript reproduce.R [path/to/analysis.csv]",
    "source('R/model.R')",
    "s <- jsonlite::read_json('settings.json', simplifyVector=TRUE)",
    "args <- commandArgs(trailingOnly=TRUE)",
    "input <- if (length(args)) args[[1L]] else 'inputs/analysis_input.csv'",
    "if (!file.exists(input)) stop('Raw rows were not included. Supply the same analysis CSV as the first argument.')",
    "d <- read.csv(input, check.names=FALSE, na.strings=c('', 'NA'))",
    "m <- if (!is.null(s$model)) s$model else s",
    "get <- function(key, fallback) if (is.null(m[[key]])) fallback else m[[key]]",
    "items <- get('measures', NULL)",
    "if (is.null(items)) items <- read.csv('landscape.csv', check.names=FALSE)$item",
    "result <- fit_landscape(d, measures=as.character(unlist(items)), predictor=as.character(unlist(get('predictor', NULL))), covariates=as.character(unlist(get('covariates', character()))), own_lag=get('own_lag', FALSE), lag_suffix=get('lag_suffix', 'Lag'), missing=get('missing', 'common'), se_method=get('se_method', 'classical'))",
    "write.csv(result,'landscape_reproduced.csv',row.names=FALSE)",
    "reference <- read.csv('landscape.csv',check.names=FALSE)",
    "numeric <- intersect(c('n','df','b','se','ci_lo','ci_hi','t','p'),names(reference))",
    "print(all.equal(result[numeric],reference[numeric],tolerance=1e-8,check.attributes=FALSE))",
    "if (!is.null(s$orientation)) { oriented <- orient_landscape(result, s$orientation); write.csv(oriented,'landscape_oriented_reproduced.csv',row.names=FALSE); if (file.exists('report_all.csv')) { rr <- read.csv('report_all.csv',check.names=FALSE); fields <- c('oriented_b','oriented_t','oriented_ci_lo','oriented_ci_hi'); stopifnot(isTRUE(all.equal(oriented[fields],rr[fields],tolerance=1e-8,check.attributes=FALSE))) } }",
    "# Frozen outcome correlation, weighted graph, membership, and boundary sets are in this bundle.",
    "# Recompute an uploaded-data EGA with: Rscript reproduce_network.R path/to/the_same_network.csv",
    "# Saved GSS respondent-level network cannot be recreated from annual analysis rows.",
    "# It is provided as a frozen, provenance-labeled tutorial input instead."
  )
  writeLines(repro,file.path(stage,"reproduce.R"))
  if (!is.null(input_audit)) {
    if (!file.exists(file.path(stage,"provenance","gss_year_cell_source.csv"))) stop("Input sensitivity requires its source-label record in the export.")
    writeLines(c(
      "# From this unzipped GSS bundle: Rscript reproduce_input_sensitivity.R",
      "source('R/model.R'); source('R/provenance.R')",
      "a <- reproduce_input_provenance('inputs/analysis_input.csv', 'provenance/gss_year_cell_source.csv', 'settings.json', 'landscape.csv')",
      "write.csv(a$summary, 'input_source_counts_reproduced.csv', row.names=FALSE)",
      "write.csv(a$restricted_landscape, 'input_sensitivity_extract_reproduced.csv', row.names=FALSE)",
      "ref <- read.csv('input_sensitivity_extract.csv',check.names=FALSE)",
      "fields <- c('n','df','b','se','ci_lo','ci_hi','t','p','p_bonferroni_universe')",
      "stopifnot(isTRUE(all.equal(a$restricted_landscape[fields],ref[fields],tolerance=1e-8,check.attributes=FALSE)))",
      "message('Input-source counts and current-outcome-restricted fits reproduced.')"
    ), file.path(stage,"reproduce_input_sensitivity.R"))
  }
  repro_net <- c(
    "# Run: Rscript reproduce_network.R [path/to/the_same_network.csv]",
    "source('R/ega.R')",
    "s <- jsonlite::read_json('settings.json', simplifyVector=TRUE)",
    "ns <- s$network",
    "if (is.null(ns)) stop('No network settings were included.')",
    "if (identical(ns$source, 'bundled_demo')) { message('The GSS tutorial uses a frozen respondent-level network, not the annual analysis rows. Inspect network_adjacency.csv, outcome_correlation.csv, network_membership.csv and boundary_summary.csv; no new network is estimated.'); quit(status=0L) }",
    "if (!identical(ns$source, 'computed')) stop('Unknown network source; do not silently substitute a method.')",
    "args <- commandArgs(trailingOnly=TRUE)",
    "input <- if (length(args)) args[[1L]] else if (isTRUE(s$network_input_separate)) 'inputs/network_input.csv' else 'inputs/analysis_input.csv'",
    "if (!file.exists(input)) stop('Network rows were not included. Supply exactly the same network CSV as the first argument.')",
    "d <- read.csv(input, check.names=FALSE, na.strings=c('', 'NA'))",
    "net <- compute_ega(d, measures=as.character(unlist(ns$measures)), bootstrap=isTRUE(ns$bootstrap), iter=if (is.null(ns$iter) || ns$iter<1L) 100L else ns$iter, seed=ns$seed, corr=ns$corr)",
    "write.csv(data.frame(item=rownames(net$adjacency),net$adjacency,check.names=FALSE),'network_adjacency_reproduced.csv',row.names=FALSE)",
    "write.csv(data.frame(item=names(net$membership),community=unname(net$membership)),'network_membership_reproduced.csv',row.names=FALSE)",
    "saveRDS(net,'network_reproduced.rds')",
    "reference <- read.csv('network_adjacency.csv',check.names=FALSE)",
    "print(all.equal(unname(net$adjacency),unname(as.matrix(reference[-1L])),tolerance=1e-8,check.attributes=FALSE))",
    "if (!is.null(s$focal)) { nb <- compare_neighborhoods(net,as.character(unlist(s$focal))); write.csv(nb$summary,'boundary_summary_reproduced.csv',row.names=FALSE) }",
    "# Bootstrap reproducibility depends on the recorded software versions; inspect session_info.txt."
  )
  writeLines(repro_net,file.path(stage,"reproduce_network.R"))
  readme <- c(
    "AMPPS Explorer reproducibility bundle",
    "",
    "Landscape estimates stay fixed when empirical neighborhood boundaries change.",
    "Observed |t| ranks describe the reconstructed sample. They are not effect-size ranks or cherry-picking verdicts.",
    "orientation_key.csv and settings.json preserve the active display direction, higher-value meaning, status and source. documented_orientation_key.csv preserves the supplied semantic key even in original-coding view. landscape.csv preserves raw fits; report_all.csv also has oriented_* fields. Only explicit documented keys reverse signs. RC in the readable report means reverse-coded display direction; unverified/nonmonotone items remain original. This is a display transformation, not refitting, standardization or evidence of conceptual equivalence.",
    "Bonferroni columns, if requested, use declared family sizes including failed fits. Individual p-values still need valid model assumptions; coefficient selection bias is not removed.",
    "",
    "Read report.html in a browser for the full interpretation and comparison record; comparison_annotations.csv contains the focal/item/relation/reason annotations.",
    if (!is.null(concentration)) "The concentration reference is recorded in reference.json, reference_statistics.csv, and reference_distribution.csv. Run Rscript reproduce_reference.R to reproduce its exact conditional calculation without raw input rows. The report retains the declared family, reported subset, ranking direction, reporting/symmetry condition, and justification." else "No concentration reference probability is attached to this comparison. Estimates, ranks and measurement notes remain available.",
    "Reproduction: install R and jsonlite, unzip, then run Rscript reproduce.R path/to/the_same_analysis.csv.",
    "If explicitly included, inputs/analysis_input.csv is used automatically. Raw user rows are excluded by default.",
    "The exact graph, original weights, outcome correlations, community membership, and boundaries are preserved for inspection. To rerun a computed EGA: Rscript reproduce_network.R path/to/the_same_network.csv. This requires EGAnet, igraph, the same network rows, and the recorded options. The script uses recorded parameters rather than guessing defaults.",
    "The annual GSS analysis input does not reconstruct its respondent-level network. The tutorial's saved network and annual inputs have distinct provenance.",
    if (!is.null(input_audit)) "GSS input sensitivity: input_source_counts.csv counts actual primary and restricted rows; input_sensitivity_extract.csv refits the current-outcome extract restriction with supplied lags unchanged. Run Rscript reproduce_input_sensitivity.R to reproduce it. This is not effective-N estimation, serial-dependence correction, or a reconstruction of interpolation." else character(),
    "",
    "settings.json contains model settings and recorded provenance. session_info.txt records the actual execution environment; renv.lock and package_versions.csv record the tested application environment. R/ contains the numerical and network source used by this app.",
    "Outcome text exported to result CSVs is escaped against spreadsheet formulas; explicitly opted-in input CSVs remain byte-for-byte copies.",
    "This archive was created by the app's analysis server for download. In a hosted session, uploaded or pasted data are transmitted to that host. User input rows are included in this archive only when explicitly requested; use public or synthetic data on the shared research preview."
  )
  writeLines(readme,file.path(stage,"README.txt"))
  hashes <- tools::md5sum(list.files(stage,recursive=TRUE,full.names=TRUE))
  writeLines(paste(unname(hashes),substring(names(hashes),nchar(stage)+2L)),file.path(stage,"MANIFEST_MD5.txt"))
  previous <- getwd(); on.exit(setwd(previous),add=TRUE); setwd(stage)
  entries <- list.files(".", recursive=TRUE, all.files=TRUE, no..=TRUE)
  zip::zipr(zipfile=archive,files=entries,recurse=FALSE,include_directories=FALSE,
            root=stage,mode="mirror")
  if (!file.exists(archive) || !file.copy(archive,target,overwrite=TRUE)) stop("ZIP creation failed; check the output directory and available disk space.")
  invisible(target)
}
