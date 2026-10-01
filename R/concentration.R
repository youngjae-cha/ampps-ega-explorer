# Conditional rank-concentration reference, Supplementary Material S1.
# Supply the already specified comparison statistic (for example, signed t in
# the stated direction or absolute t). Larger supplied values rank strongest.
# The reference is exact conditional on the observed statistics under uniform
# selection of a size-m subset. Fixed reported labels also have a valid rank
# reference under a joint distribution exchangeable across candidate labels.
# EGA establishes neither assumption. This does not identify a reporting process
# or estimate the probability that a report arose through selective reporting.
# Base R only; enumeration is exact and stops explicitly at the requested cap.

exact_concentration <- function(statistics, selected, max_subsets = 200000, alpha = .05) {
  if (!is.numeric(statistics) || !is.null(dim(statistics)) ||
      !length(statistics) || any(!is.finite(statistics)))
    stop("Statistics must be a nonempty finite numeric vector; every candidate must have a valid statistic.",
         call. = FALSE)
  candidate_names <- names(statistics)
  if (is.null(candidate_names) || anyNA(candidate_names) ||
      any(!nzchar(trimws(candidate_names))) || anyDuplicated(candidate_names))
    stop("Statistics must have unique, nonempty candidate names.", call. = FALSE)
  if (!is.character(selected) || !is.null(dim(selected)) || !length(selected) ||
      anyNA(selected) || any(!nzchar(trimws(selected))) || anyDuplicated(selected))
    stop("Select at least one candidate using unique, nonempty names.", call. = FALSE)
  if (!all(selected %in% candidate_names))
    stop("Every selected candidate must be present in the specified candidate family.",
         call. = FALSE)
  if (!is.numeric(max_subsets) || length(max_subsets) != 1L ||
      !is.finite(max_subsets) || max_subsets < 1 ||
      max_subsets != floor(max_subsets) || max_subsets > .Machine$integer.max)
    stop("max_subsets must be a positive whole number no larger than the R integer limit.",
         call. = FALSE)

  if (length(alpha) != 1L || !is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1)
    stop("alpha must be a single number between zero and one.", call. = FALSE)

  k <- length(statistics)
  m <- length(selected)
  # choose() can have floating-point roundoff even though the count is integral.
  n_subsets <- round(choose(k, m))
  if (!is.finite(n_subsets) || n_subsets > max_subsets)
    stop(sprintf(paste0("Exact concentration requires C(%s, %s) = %s subsets, ",
                        "exceeding the enumeration limit of %s. ",
                        "This family is retained without a reference p-value. ",
                        "For a prespecified larger family, run exact_concentration() from R/concentration.R ",
                        "with an explicitly increased resource limit."),
                 k, m, format(n_subsets, scientific = FALSE, trim = TRUE),
                 format(max_subsets, scientific = FALSE, trim = TRUE)),
         call. = FALSE)

  ranks <- stats::setNames(rank(-statistics, ties.method = "average"), candidate_names)
  rank_sum <- unname(sum(ranks[selected]))
  # A complement has the same number of subsets and avoids materializing long
  # reported sets when almost every candidate is selected. combn(FUN=...) keeps
  # only the resulting sums, not a matrix of every subset's members.
  if (m == k) {
    subset_sums <- sum(ranks)
  } else if (m <= k - m) {
    subset_sums <- utils::combn(unname(ranks), m, FUN = sum)
  } else {
    subset_sums <- sum(ranks) - utils::combn(unname(ranks), k - m, FUN = sum)
  }
  support <- sort(unique(subset_sums))
  frequency <- tabulate(match(subset_sums, support), nbins = length(support))
  distribution <- data.frame(rank_sum = support, frequency = frequency,
                             probability = frequency / n_subsets)
  # Midrank sums lie on a half-integer lattice. Bound the rounding tolerance
  # below its spacing so a neighboring rank sum cannot enter the lower tail.
  rank_tolerance <- min(.25, 8 * .Machine$double.eps * max(1, abs(rank_sum)))
  p_ref <- sum(frequency[support <= rank_sum + rank_tolerance]) / n_subsets
  cdf <- cumsum(frequency) / n_subsets
  attainable_rate <- sum(frequency[cdf <= alpha + 8 * .Machine$double.eps]) / n_subsets

  list(k = k, m = m, alpha = alpha, rank_sum = rank_sum, p_ref = p_ref,
       minimum_reference = frequency[1L] / n_subsets,
       attainable_rate = attainable_rate, n_subsets = n_subsets,
       ranks = ranks, distribution = distribution,
       calculation = "Exact conditional permutation of reported-status labels using midrank sums")
}
