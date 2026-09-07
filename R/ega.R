# Outcome-only EGA and reproducible comparison boundaries.
# Actual APIs checked against installed EGAnet 2.3.0 and
# https://r-ega.net/reference/{EGA,bootEGA,itemStability}.html.
# No Shiny dependency. No outcome/predictor association enters these functions.

.ega_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE))
    stop(sprintf("Package '%s' is required; no substitute algorithm was run.", package), call. = FALSE)
}

.ega_seed <- function(seed, code) {
  existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (existed) old <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (existed) assign(".Random.seed", old, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  force(code)
}

.ega_matrix <- function(x, label, items = NULL) {
  x <- as.matrix(x)
  if (!is.numeric(x) || length(dim(x)) != 2L || nrow(x) != ncol(x) ||
      nrow(x) < 1L || any(!is.finite(x)))
    stop(label, " must be a finite numeric square matrix.", call. = FALSE)
  if (is.null(rownames(x)) || is.null(colnames(x)) ||
      anyDuplicated(rownames(x)) || anyDuplicated(colnames(x)) ||
      !setequal(rownames(x), colnames(x)))
    stop(label, " must have unique matching row and column names.", call. = FALSE)
  x <- x[rownames(x), rownames(x), drop = FALSE]
  if (max(abs(x - t(x))) > 1e-8)
    stop(label, " is not symmetric.", call. = FALSE)
  if (!is.null(items)) {
    if (!setequal(items, rownames(x)))
      stop(label, " does not cover the requested outcomes.", call. = FALSE)
    x <- x[items, items, drop = FALSE]
  }
  x
}

.ega_graph <- function(adjacency) {
  .ega_require("igraph")
  adjacency <- .ega_matrix(adjacency, "Adjacency")
  diag(adjacency) <- 0
  igraph::graph_from_adjacency_matrix(abs(adjacency), mode = "undirected",
                                    weighted = TRUE, diag = FALSE)
}

.ega_layout <- function(adjacency, seed = 20260907L) {
  graph <- .ega_graph(adjacency)
  initial <- igraph::layout_in_circle(graph)
  coords <- if (igraph::ecount(graph)) .ega_seed(seed,
    igraph::layout_with_fr(graph, coords = initial, weights = igraph::E(graph)$weight,
                           niter = 500L, grid = "nogrid")) else initial
  rownames(coords) <- rownames(adjacency)
  colnames(coords) <- c("x", "y")
  coords
}

.ega_reweight <- function(graph, correlation) {
  graph <- .ega_matrix(graph, "Estimated network", rownames(correlation))
  mask <- graph != 0
  diag(mask) <- FALSE
  # Keep the original structure separately: its edge magnitudes are not lengths.
  adjacency <- correlation * mask
  diag(adjacency) <- 0
  adjacency
}

.ega_walktrap <- function(adjacency) {
  graph <- .ega_graph(adjacency)
  if (!igraph::ecount(graph))
    return(stats::setNames(seq_len(nrow(adjacency)), rownames(adjacency)))
  result <- igraph::membership(igraph::cluster_walktrap(graph,
    weights = igraph::E(graph)$weight))
  stats::setNames(as.integer(result[rownames(adjacency)]), rownames(adjacency))
}

.ega_api <- function(outcome_count, bootstrap = FALSE) {
  # EGAnet 2.3.0 has two small-matrix implementation errors in TMFG:
  # k=5 drops a one-row matrix before rowSums; k=4 incorrectly loops over 5:4.
  # Repair only those dimensions in a private clone of the actual package
  # call chain. We never modify the installed namespace or change algorithms.
  plain <- list(EGA = EGAnet::EGA, bootEGA = EGAnet::bootEGA, compatibility = NULL)
  if ((outcome_count > 5L && !bootstrap) ||
      as.character(utils::packageVersion("EGAnet")) != "2.3.0")
    return(plain)
  namespace <- asNamespace("EGAnet")
  local_api <- new.env(parent = namespace)
  for (name in c("TMFG", "network.estimation", "EGA.estimate", "EGA", "bootEGA",
                 "estimate_typicalStructure")) {
    fn <- get(name, envir = namespace, inherits = FALSE)
    environment(fn) <- local_api
    assign(name, fn, envir = local_api)
  }
  fn <- local_api$TMFG
  statements <- as.list(body(fn))
  dimensions_fixed <- loops_fixed <- cliques_fixed <- 0L
  for (i in seq_along(statements)) {
    statement <- statements[[i]]
    if (!is.call(statement)) next
    if (identical(statement[[1L]], as.name("<-"))) {
      lhs <- statement[[2L]]
      rhs <- statement[[3L]]
      if (is.call(lhs) && identical(lhs[[1L]], as.name("[")) &&
          identical(lhs[[2L]], as.name("gain")) && length(lhs) == 4L &&
          identical(lhs[[3L]], as.name("remaining")) &&
          is.numeric(lhs[[4L]]) && lhs[[4L]] %in% 1:4 &&
          is.call(rhs) && identical(rhs[[1L]], as.name("rowSums"))) {
        subset <- rhs[[2L]]
        if (is.call(subset) && identical(subset[[2L]], as.name("absolute_matrix")) &&
            length(subset) == 4L) {
          subset[["drop"]] <- FALSE
          rhs[[2L]] <- subset
          statement[[3L]] <- rhs
          dimensions_fixed <- dimensions_fixed + 1L
        }
      }
      if (identical(lhs, as.name("cliques"))) {
        statement[[3L]] <- quote(if (nodes == 4L) matrix(inserted[first_four], nrow = 1L) else
          rbind(inserted[first_four], cbind(separators, inserted[seq.int(5L, nodes)])))
        cliques_fixed <- cliques_fixed + 1L
      }
    }
    if (identical(statement[[1L]], as.name("for")) &&
        identical(statement[[2L]], as.name("i")) && identical(statement[[3L]], quote(5:nodes))) {
      statement[[3L]] <- quote(if (nodes > 4L) seq.int(5L, nodes) else integer())
      loops_fixed <- loops_fixed + 1L
    }
    statements[[i]] <- statement
  }
  if (dimensions_fixed != 4L || loops_fixed != 1L || cliques_fixed != 1L)
    stop("Installed EGAnet TMFG source differs from the validated small-matrix compatibility patch; no alternate algorithm was run.", call. = FALSE)
  body(fn) <- as.call(statements)
  local_api$TMFG <- fn
  # A second 2.3.0 defect affects unidimensional bootstrap samples: the LE
  # check records "Leading Eigenvector", whereas community.detection accepts
  # the igraph name "leading_eigen". Normalize this alias, not the method.
  fn <- local_api$estimate_typicalStructure
  statements <- as.list(body(fn))
  aliases_fixed <- 0L
  for (i in seq_along(statements)) {
    statement <- statements[[i]]
    if (is.call(statement) && identical(statement[[1L]], as.name("<-")) &&
        identical(statement[[2L]], as.name("algorithm")) &&
        identical(statement[[3L]], quote(tolower(algorithm_attributes$algorithm)))) {
      statement[[3L]] <- quote(sub("^leading eigenvector$", "leading_eigen",
                                   tolower(algorithm_attributes$algorithm)))
      statements[[i]] <- statement
      aliases_fixed <- aliases_fixed + 1L
    }
  }
  if (aliases_fixed != 1L)
    stop("Installed EGAnet typical-structure source differs from the validated algorithm-alias compatibility patch.", call. = FALSE)
  body(fn) <- as.call(statements)
  local_api$estimate_typicalStructure <- fn
  list(EGA = local_api$EGA, bootEGA = local_api$bootEGA,
       compatibility = paste("EGAnet 2.3.0 private compatibility guards:",
         if (outcome_count <= 5L) "small-matrix TMFG dimensions/empty insertion loop;" else "",
         if (bootstrap) "leading-eigenvector algorithm-name normalization;" else "",
         "algorithms unchanged, installed namespace unchanged"))
}

load_demo_network <- function(data_dir) {
  matrix_file <- function(name) {
    path <- file.path(data_dir, name)
    if (!file.exists(path) || file.info(path)$size == 0L)
      stop("Missing or empty bundled network input: ", name, call. = FALSE)
    as.matrix(utils::read.csv(path, row.names = 1L, check.names = FALSE))
  }
  graph <- .ega_matrix(matrix_file("typical_network.csv"), "Bundled typical network")
  correlation <- .ega_matrix(matrix_file("item_cor_respondent.csv"),
                             "Bundled respondent correlation", rownames(graph))
  mapping <- utils::read.csv(file.path(data_dir, "ega_membership.csv"),
                            stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("item", "item_title", "walktrap_typical") %in% names(mapping)) ||
      anyDuplicated(mapping$item) || !all(rownames(graph) %in% mapping$item))
    stop("Bundled membership must contain one original item/title/membership per node.", call. = FALSE)
  mapping <- mapping[match(rownames(graph), mapping$item), , drop = FALSE]
  items <- mapping$item_title
  if (anyNA(items) || anyDuplicated(items) || any(!nzchar(items)))
    stop("Bundled item names are missing or duplicated.", call. = FALSE)
  membership <- stats::setNames(as.integer(mapping$walktrap_typical), items)
  if (anyNA(membership)) stop("Bundled membership contains missing values.", call. = FALSE)
  dimnames(graph) <- dimnames(correlation) <- list(items, items)
  adjacency <- .ega_reweight(graph, correlation)
  stability_path <- file.path(data_dir, "item_stability.csv")
  if (!file.exists(stability_path) || file.info(stability_path)$size == 0L)
    stop("Bundled item stability is missing or empty.", call. = FALSE)
  stability <- utils::read.csv(stability_path, stringsAsFactors = FALSE)
  if (!all(c("item", "stability") %in% names(stability)) || anyDuplicated(stability$item))
    stop("Bundled stability requires unique item/stability rows.", call. = FALSE)
  values <- stability$stability[match(mapping$item, stability$item)]
  if (anyNA(values) || any(values < 0 | values > 1))
    stop("Bundled stability does not cover all nodes with valid proportions.", call. = FALSE)
  list(adjacency = adjacency, correlation = correlation, membership = membership,
       coordinates = .ega_layout(adjacency),
       stability = data.frame(item = items, stability = values),
       method = "Archived bootEGA structure + respondent zero-order weights; saved Walktrap membership",
       provenance = c("Bundled GSS demonstration: archived outputs, not a newly estimated network.",
         "Edges use archived typical-network presence; weights are respondent-level zero-order correlations.",
         "Community labels are saved original Walktrap labels; item stability is the archived empirical-structure bootstrap statistic.",
         "Network relationships are respondent-level; demonstration regressions are year-level."),
       n = NA_integer_,
       warnings = "The archived respondent correlation has varying item coverage; no single complete-case n is asserted.",
       original_network = graph,
       edge_source = "archived bootEGA typical network presence",
       membership_source = "saved original Walktrap membership (not re-estimated)",
       settings = list(source = "bundled_demo", network_model = "TMFG", algorithm = "walktrap",
         weight_rule = "zero-order correlation on typical-network edges; length = 1/abs(r)",
         missing_policy = "archived respondent correlations; varying item coverage",
         stability_reference = "archived empirical EGA membership", seed = NA_integer_))
}

compute_ega <- function(data, measures, bootstrap = FALSE, iter = 100L,
                        seed = 20260907L, corr = "pearson") {
  .ega_require("EGAnet")
  .ega_require("igraph")
  if (!is.data.frame(data) && !is.matrix(data)) stop("Supply a data frame or matrix.", call. = FALSE)
  measures <- as.character(measures)
  if (length(measures) < 4L || length(measures) > 200L || anyNA(measures) ||
      anyDuplicated(measures) || !all(measures %in% colnames(data)))
    stop("EGA requires 4–200 distinct outcome columns present in the input.", call. = FALSE)
  if (anyDuplicated(colnames(data))) stop("Input column names must be unique.", call. = FALSE)
  if (!is.logical(bootstrap) || length(bootstrap) != 1L || is.na(bootstrap))
    stop("bootstrap must be TRUE or FALSE.", call. = FALSE)
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed) || seed < 0 ||
      seed > .Machine$integer.max || seed != floor(seed)) stop("Use a nonnegative integer seed.", call. = FALSE)
  if (length(corr) != 1L || is.na(corr) || !corr %in% c("pearson", "spearman"))
    stop("The app supports Pearson or Spearman correlations; select one explicitly.", call. = FALSE)
  if (bootstrap && (length(iter) != 1L || !is.numeric(iter) || !is.finite(iter) ||
      iter < 10L || iter > 500L || iter != floor(iter)))
    stop("Bootstrap iterations must be an integer from 10 to 500 (500 recommended for reporting).", call. = FALSE)
  selected <- as.data.frame(data[, measures, drop = FALSE])
  if (nrow(selected) < 10L) stop("Supply at least 10 outcome-data rows.", call. = FALSE)
  if (!all(vapply(selected, is.numeric, logical(1))))
    stop("All EGA outcomes must be numeric; code categorical responses explicitly before upload.", call. = FALSE)
  finite <- apply(as.matrix(selected), 1L, function(x) all(is.finite(x)))
  x <- as.matrix(selected[finite, , drop = FALSE])
  storage.mode(x) <- "double"
  if (nrow(x) < 10L) stop("Fewer than 10 complete finite rows remain across the selected outcomes.", call. = FALSE)
  constant <- vapply(seq_len(ncol(x)), function(j) stats::sd(x[, j]) == 0, logical(1))
  if (any(constant)) stop("Constant outcomes after complete-case filtering: ",
    paste(measures[constant], collapse = ", "), call. = FALSE)
  correlation <- stats::cor(x, method = corr)
  correlation <- .ega_matrix(correlation, "Empirical correlation", measures)
  if (any(abs(correlation[upper.tri(correlation)]) > 1 - 1e-12))
    stop("Perfectly correlated outcome columns were found. Remove duplicates or exact recodings before EGA.", call. = FALSE)
  collected <- character()
  api <- .ega_api(length(measures), bootstrap)
  collect <- function(code) withCallingHandlers(code,
    warning = function(w) { collected <<- c(collected, conditionMessage(w)); invokeRestart("muffleWarning") },
    message = function(m) invokeRestart("muffleMessage"))
  result <- tryCatch(.ega_seed(as.integer(seed), collect(if (bootstrap) {
      # EGAnet 2.3.0's compiled reproducible_seeds path expects double scalars.
      api$bootEGA(x, corr = corr, na.data = "listwise", model = "TMFG",
        algorithm = "walktrap", uni.method = "LE", iter = as.numeric(iter),
        type = "resampling", ncores = 1, typicalStructure = TRUE,
        plot.itemStability = FALSE, plot.typicalStructure = FALSE,
        seed = as.numeric(seed), verbose = FALSE)
    } else {
      api$EGA(x, corr = corr, na.data = "listwise", model = "TMFG",
        algorithm = "walktrap", uni.method = "LE", plot.EGA = FALSE, verbose = FALSE)
    })), error = function(e) stop("EGA did not complete: ", conditionMessage(e),
                                  ". No fallback network was substituted.", call. = FALSE))
  original <- if (bootstrap) result$typicalGraph$graph else result$network
  if (is.null(original)) stop("EGAnet returned no expected network object.", call. = FALSE)
  original <- .ega_matrix(original, "EGAnet network", measures)
  adjacency <- .ega_reweight(original, correlation)
  # Cluster the graph actually displayed and used for distances. This is an
  # explicit post-estimation step, not a claim that EGA's unidimensional check
  # or averaged edge weights yielded these exact memberships.
  membership <- .ega_walktrap(adjacency)
  stability_values <- rep(NA_real_, length(measures))
  if (bootstrap) {
    stability_result <- tryCatch(collect(EGAnet::itemStability(result, IS.plot = FALSE,
      structure = membership)), error = function(e)
        stop("Bootstrap finished but item-stability calculation failed: ", conditionMessage(e), call. = FALSE))
    values <- stability_result$item.stability$empirical.dimensions
    if (is.null(names(values)) || !all(measures %in% names(values)))
      stop("EGAnet item stability could not be aligned to outcome names.", call. = FALSE)
    stability_values <- as.numeric(values[measures])
    if (any(!is.finite(stability_values)) || any(stability_values < 0 | stability_values > 1))
      stop("Bootstrap returned invalid item-stability values; inspect the input and resampling design.", call. = FALSE)
    if (iter < 500L) collected <- c(collected,
      sprintf("%d bootstrap repetitions: a quick stability check; use 500 for a fuller assessment.", iter))
  }
  removed <- nrow(data) - nrow(x)
  if (!is.null(api$compatibility)) collected <- c(collected, api$compatibility)
  if (removed) collected <- c(collected,
    sprintf("Complete-case outcome network omitted %d of %d rows; this is separate from model missingness handling.", removed, nrow(data)))
  if (nrow(x) <= ncol(x)) collected <- c(collected,
    "The complete-case sample is no larger than the outcome count; the map may be especially unstable.")
  if (any(vapply(selected, function(z) length(unique(z[is.finite(z)])) <= 7L, logical(1))))
    collected <- c(collected,
      paste("Low-category outcomes detected:", corr, "correlations were used as requested, not polychoric correlations."))
  list(adjacency = adjacency, correlation = correlation, membership = membership,
    coordinates = .ega_layout(adjacency, as.integer(seed)),
    stability = data.frame(item = measures, stability = stability_values),
    method = if (bootstrap) "Bootstrap typical TMFG structure + zero-order weights + Walktrap" else
      "Empirical TMFG preview + zero-order weights + Walktrap (not bootstrapped)",
    provenance = c(sprintf("Computed by this app with EGAnet %s and igraph %s.",
      utils::packageVersion("EGAnet"), utils::packageVersion("igraph")),
      sprintf("Only %d outcome columns enter the network; %d complete finite rows were used.", length(measures), nrow(x)),
      "Edge presence comes from the estimated TMFG structure; signed weights are empirical zero-order correlations.",
      "Walktrap communities and shortest-path distances use absolute reweighted edges.",
      if (bootstrap) "Item stability compares bootstrap assignments against the displayed reweighted-graph membership; it is not construct validity." else
        "Preview has no item-stability estimate. Run the optional bootstrap before interpreting map stability."),
    n = nrow(x), warnings = unique(collected), original_network = original,
    edge_source = if (bootstrap) "mean bootstrap typical TMFG presence" else "empirical TMFG presence",
    membership_source = "Walktrap on absolute zero-order reweighted displayed graph",
      settings = list(source = "computed", bootstrap = bootstrap, iter = if (bootstrap) as.integer(iter) else 0L,
      seed = as.integer(seed), corr = corr, model = "TMFG", algorithm = "walktrap", uni_method = "LE",
      bootstrap_type = if (bootstrap) "resampling" else "none", ncores = 1L,
      missing_policy = "listwise finite outcomes", complete_n = nrow(x), excluded_rows = removed,
      stability_reference = if (bootstrap) "displayed Walktrap membership" else "not estimated",
      compatibility = if (is.null(api$compatibility)) "none" else api$compatibility,
      weight_rule = "zero-order correlation on estimated edges; length = 1/abs(r)"))
}

compare_neighborhoods <- function(network, focal, percentiles = c(15, 25, 35)) {
  adjacency <- .ega_matrix(network$adjacency, "Network adjacency")
  items <- rownames(adjacency)
  focal <- unique(as.character(focal))
  if (!length(focal) || anyNA(focal) || !all(focal %in% items))
    stop("Select at least one focal outcome present in the network.", call. = FALSE)
  if (!is.numeric(percentiles) || !length(percentiles) || any(!is.finite(percentiles)) ||
      any(percentiles < 0 | percentiles > 100) || anyDuplicated(percentiles))
    stop("Distance percentiles must be distinct numbers from 0 to 100.", call. = FALSE)
  membership <- network$membership
  if (is.null(names(membership)) || !all(items %in% names(membership)))
    stop("Network membership must be named for all outcomes.", call. = FALSE)
  membership <- membership[items]
  focal_communities <- unique(membership[focal])
  focal_communities <- focal_communities[!is.na(focal_communities)]
  community <- items[items %in% focal | (!is.na(membership) & membership %in% focal_communities)]
  graph <- .ega_graph(adjacency)
  lengths <- if (igraph::ecount(graph)) 1 / igraph::E(graph)$weight else numeric()
  pair_distances <- igraph::distances(graph, v = focal, to = items, weights = lengths)
  distances <- apply(pair_distances, 2L, min)
  distances <- stats::setNames(as.numeric(distances), items)
  nonfocal <- setdiff(items, focal)
  finite_pool <- distances[nonfocal][is.finite(distances[nonfocal])]
  unreachable <- sum(!is.finite(distances[nonfocal]))
  sets <- list(community = community)
  summary <- data.frame(boundary = "community", k = length(community), cutoff = NA_real_,
    percentile = NA_real_, pool_n = length(finite_pool), unreachable_excluded = unreachable,
    ties_at_cutoff = NA_integer_)
  for (percentile in percentiles) {
    cutoff <- if (length(finite_pool)) unname(stats::quantile(finite_pool,
      percentile / 100, type = 7L, names = FALSE)) else NA_real_
    # Numerical tolerance makes tied distances reproducible across BLAS/platforms.
    tolerance <- if (is.finite(cutoff)) 1e-10 * max(1, abs(cutoff)) else 0
    selected <- if (is.finite(cutoff)) items[items %in% focal |
      (is.finite(distances) & distances <= cutoff + tolerance)] else items[items %in% focal]
    key <- paste0("ring", format(percentile, trim = TRUE, scientific = FALSE))
    sets[[key]] <- selected
    summary <- rbind(summary, data.frame(boundary = key, k = length(selected), cutoff = cutoff,
      percentile = percentile, pool_n = length(finite_pool), unreachable_excluded = unreachable,
      ties_at_cutoff = if (is.finite(cutoff)) sum(abs(finite_pool - cutoff) <= tolerance) else 0L))
  }
  warnings <- character()
  if (unreachable) warnings <- c(warnings,
    sprintf("%d nonfocal outcomes are unreachable and excluded from the percentile denominator.", unreachable))
  if (!length(nonfocal)) warnings <- c(warnings, "All outcomes are focal; distance rings contain the focal set and no percentile is estimated.")
  if (length(nonfocal) && !length(finite_pool)) warnings <- c(warnings,
    "No nonfocal outcome is reachable; distance rings contain only focal outcomes.")
  if (anyNA(membership[focal])) warnings <- c(warnings,
    "A focal outcome has no community assignment; it is still retained in every comparison set.")
  list(sets = sets, summary = summary, distances = distances, warnings = warnings,
       rule = "Union of focal communities; rings use type-7 percentiles of finite nonfocal nearest-focal distances with all cutoff ties retained.")
}
