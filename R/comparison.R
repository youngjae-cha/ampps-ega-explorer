# Descriptive comparison and reference records share the same fitted results.
# A reference family is explicitly supplied; EGA membership never authorizes it.

comparison_statistics <- function(landscape, family, statistic = "abs_t") {
  if (!is.character(family) || !length(family) || anyNA(family) ||
      anyDuplicated(family) || any(!nzchar(family)))
    stop("Specify a nonempty comparison set with unique outcome names.", call. = FALSE)
  if (anyDuplicated(landscape$item) || !all(family %in% landscape$item))
    stop("Every comparison outcome must have a unique fitted result.", call. = FALSE)
  if (length(statistic) != 1L || !statistic %in% c("abs_t", "positive_t", "negative_t"))
    stop("Declare absolute t, positive signed t, or negative signed t.", call. = FALSE)
  x <- landscape[match(family, landscape$item), , drop = FALSE]
  if (any(!is.finite(x$t)) || ("status" %in% names(x) && any(x$status != "ok")))
    stop("Every specified outcome needs a finite, estimable t statistic. Review failed fits; the family has not been reduced.", call. = FALSE)
  value <- switch(statistic, abs_t = abs(x$t), positive_t = x$t, negative_t = -x$t)
  stats::setNames(value, x$item)
}

rank_pattern <- function(landscape, family, selected) {
  s <- comparison_statistics(landscape, family, "abs_t")
  if (!length(selected) || anyDuplicated(selected) || !all(selected %in% family))
    stop("The reported outcomes must be a nonempty subset of the comparison set.", call. = FALSE)
  peers <- setdiff(names(s), selected)
  higher <- peers[vapply(peers, function(p) any(s[p] > s[selected]), logical(1))]
  k <- length(s); m <- length(selected)
  conclusion <- if (length(higher)) sprintf(
    "Under this common model and absolute-t ordering, the reported set differs from the highest-ranked subset of %d outcomes among these %d candidates. Unreported outcomes ranking strictly above at least one reported outcome: %s.",
    m, k, paste(higher, collapse = ", ")) else sprintf(
    "Under this common model and absolute-t ordering, the reported set is one of the highest-ranked subsets of %d outcomes among these %d candidates, allowing ties. This pattern focuses examination of result-based selection, measurement quality, underlying associations, and the comparison set.", m, k)
  list(k = k, m = m, stronger_unreported = higher, top_subset = !length(higher), text = conclusion)
}

reference_condition_text <- function(condition) {
  switch(condition,
    uniform = "Conditional on the observed statistics, every same-sized subset has equal reporting probability independently of those statistics.",
    symmetry = "For fixed reported labels, the joint distribution of the statistics is invariant to permutations of candidate labels.",
    stop("Choose a justified reporting or fixed-label symmetry condition.", call. = FALSE))
}

make_reference_record <- function(landscape, family, selected, statistic, condition,
                                  justification, design_record, independent, claim = "",
                                  orientation = NULL, max_subsets = 200000) {
  if (!isTRUE(independent)) stop("Confirm that the comparison set and statistic were specified independently of the focal predictor results.", call. = FALSE)
  for (x in list(justification, design_record, claim))
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
      stop("Record the claim, statistical justification, and how the set and statistic were specified independently of the results.", call. = FALSE)
  condition_text <- reference_condition_text(condition)
  statistics <- comparison_statistics(landscape, family, statistic)
  calculation <- exact_concentration(statistics, selected, max_subsets = max_subsets)
  list(family = family, selected = selected, statistic = statistic, condition = condition,
       condition_text = condition_text, justification = justification,
       design_record = design_record, independent_declaration = independent,
       claim = claim, orientation = orientation,
       statistics = data.frame(item = names(statistics), statistic = unname(statistics),
         rank = unname(calculation$ranks), reported = names(statistics) %in% selected),
       result = calculation)
}

reference_report_text <- function(record) {
  if (is.null(record)) return(c("## Concentration reference", "No reference probability is attached. The estimates, ranks, measurement comparisons, and claim-relevant contrasts are retained."))
  r <- record$result
  metric <- switch(record$statistic, abs_t="Absolute t (|t|)",
    positive_t="Signed t in the positive displayed direction",
    negative_t="Signed t in the negative displayed direction")
  c("## Concentration reference", paste("Claim:", record$claim),
    paste("Comparison set:", paste(record$family, collapse = ", ")),
    paste("Reported subset:", paste(record$selected, collapse = ", ")),
    paste("Ranking statistic:", metric),
    paste("Reference condition:", record$condition_text),
    paste("Statistical justification (researcher declaration):", record$justification),
    paste("Independent specification (researcher declaration):", record$design_record),
    sprintf("Reported rank sum W = %s; k = %d, m = %d; %s subsets enumerated; reference p = %.8g; smallest attainable reference p = %.8g; attainable rate at alpha = %.2f is %.8g.",
      format(r$rank_sum), r$k, r$m, format(r$n_subsets, scientific = FALSE), r$p_ref, r$minimum_reference, r$alpha, r$attainable_rate),
    "The calculation reallocates reported-status labels over fixed statistics and uses average ranks for ties. The reference p-value quantifies how unusual this concentration is under the stated condition.",
    "Interpret the pattern alongside item content, measurement quality, expected associations, comparison-set composition, selection rationale, and analysis records to examine result-based selection and other explanations. Estimates and intervals supply magnitude, direction, and uncertainty.")
}
