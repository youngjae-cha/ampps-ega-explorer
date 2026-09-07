# Numerical analysis engine. User column names are data, never R formulas.
validate_analysis_data <- function(data) {
  if (!is.data.frame(data) || nrow(data) < 3L) stop("Provide a data frame with at least three rows.")
  nm <- names(data)
  if (is.null(nm) || anyNA(nm) || any(!nzchar(trimws(nm))) || anyDuplicated(nm))
    stop("Column names must be nonempty and unique; preserve their original spelling.")
  if (any(grepl("[\r\n\t]", nm))) stop("Column names cannot contain tabs or newlines.")
  invisible(TRUE)
}

validate_roles <- function(data, measures, predictor, covariates) {
  validate_analysis_data(data)
  if (length(predictor) != 1L || is.na(predictor) || !nzchar(predictor)) stop("Choose exactly one predictor.")
  if (!length(measures) || anyNA(measures) || anyDuplicated(measures)) stop("Choose unique outcome columns.")
  if (anyNA(covariates) || anyDuplicated(covariates) || predictor %in% covariates) stop("Predictor and covariates must be distinct.")
  if (any(measures %in% c(predictor, covariates))) stop("An outcome cannot also be a predictor or covariate.")
  absent <- setdiff(c(measures, predictor, covariates), names(data))
  if (length(absent)) stop(paste("Missing columns:", paste(absent, collapse=", ")))
  invisible(TRUE)
}

finite_rows <- function(data, columns) {
  if (!length(columns)) return(rep(TRUE, nrow(data)))
  Reduce(`&`, lapply(columns, function(nm) is.finite(data[[nm]])))
}

load_demo_data <- function(data_dir) {
  dat <- read.csv(file.path(data_dir, "gss_year.csv"), check.names=FALSE, na.strings=c("", "NA"))
  ref <- read.csv(file.path(data_dir, "landscape_reference.csv"), check.names=FALSE)
  labs <- read.csv(file.path(data_dir, "effect_landscape.csv"), check.names=FALSE)
  items <- names(dat)[!names(dat) %in% c("year", "MobilityLag") & !endsWith(names(dat), "Lag")]
  labels <- setNames(labs$label[match(items, labs$item)], items)
  labels[is.na(labels) | !nzchar(labels)] <- items[is.na(labels) | !nzchar(labels)]
  meta <- data.frame(item=items, label=unname(labels), domain="", respondent_scope="Check the item-specific GSS universe and filters", coding_note="Original inherited coding; no automatic direction alignment", n_extract=ref$n_extract[match(items, ref$measure)], stringsAsFactors=FALSE)
  meta$respondent_scope[meta$item == "Hapmar"] <- "Married respondents; related outcome rather than direct general-happiness substitute"
  meta$respondent_scope[meta$item == "Satjob"] <- "Job/housekeeping satisfaction eligible respondents; inspect applicable filters"
  meta$domain[meta$item %in% c("Happy", "Hapmar", "Health", "Life", "Satjob")] <- "Well-being and related domains"
  meta$domain[meta$item %in% c("Trust", "Fair", "Helpful")] <- "Interpersonal attitudes"
  meta$domain[grepl("^Con", meta$item)] <- "Confidence in institutions"
  list(data=dat, labels=labels, metadata=meta, provenance=c(
    "Built-in GSS tutorial: 47 annual aggregate rows and 44 outcomes, with supplied one-year lag columns.",
    "Annual derived values include inherited non-survey/missing-year values. No raw-to-derived interpolation algorithm is reconstructed by this app; source labels are bundled separately.",
    "The saved network is based on respondent-level outcome correlations; landscape estimates use annual aggregates. These are distinct analytic levels.",
    "The app preserves original coding and displays signed estimates. Directional construct comparisons require a documented codebook/theory orientation key.",
    "Classical OLS intervals reproduce the saved tutorial specification; they do not account for inherited interpolation or unmodeled serial dependence."
  ))
}

audit_universe <- function(data, candidates, predictor, covariates=character(), min_n=10,
                           max_missing=.2, own_lag=FALSE, lag_suffix="Lag") {
  validate_roles(data, candidates, predictor, covariates)
  if (length(min_n) != 1L || !is.finite(min_n) || min_n < 3) stop("Minimum usable N must be at least three.")
  if (length(max_missing) != 1L || !is.finite(max_missing) || max_missing < 0 || max_missing > 1) stop("Missingness threshold must lie between zero and one.")
  shared <- c(predictor, covariates)
  bad_shared <- shared[!vapply(data[shared], is.numeric, logical(1))]
  do.call(rbind, lapply(candidates, function(item) {
    lag <- if (own_lag) paste0(item, lag_suffix) else character()
    cols <- c(shared, item, lag)
    reasons <- character()
    if (length(bad_shared)) reasons <- c(reasons, paste("Nonnumeric shared input:", paste(bad_shared, collapse=", ")))
    if (!is.numeric(data[[item]])) reasons <- c(reasons, "Outcome is not numeric")
    if (length(setdiff(lag, names(data)))) reasons <- c(reasons, "Own-lag column is missing")
    if (length(lag) && lag %in% names(data) && !is.numeric(data[[lag]])) reasons <- c(reasons, "Own-lag column is not numeric")
    if (length(lag) && lag %in% shared) reasons <- c(reasons, "Own-lag duplicates a shared model input")
    observed <- if (is.numeric(data[[item]])) sum(is.finite(data[[item]])) else sum(!is.na(data[[item]]))
    missing_fraction <- 1 - observed/nrow(data)
    usable <- 0L
    if (!length(reasons)) {
      keep <- finite_rows(data, cols)
      usable <- sum(keep)
      if (length(unique(data[[item]][keep])) < 2L) reasons <- c(reasons, "Outcome is constant on usable rows")
      if (usable < min_n) reasons <- c(reasons, sprintf("Usable N %d is below %d", usable, min_n))
      if (usable <= 2L + length(covariates) + as.integer(own_lag)) reasons <- c(reasons, "No residual degrees of freedom")
    }
    if (missing_fraction > max_missing) reasons <- c(reasons, "Outcome missingness exceeds threshold")
    data.frame(item=item, included=!length(reasons), reason=if (length(reasons)) paste(unique(reasons), collapse="; ") else "Eligible under stated data-availability rules", n_observed=observed, missing_fraction=missing_fraction, n_usable=usable, stringsAsFactors=FALSE)
  }))
}

fit_landscape <- function(data, measures, predictor, covariates=character(), own_lag=FALSE,
                          lag_suffix="Lag", missing="common", se_method="classical") {
  validate_roles(data, measures, predictor, covariates)
  missing <- match.arg(missing, c("common", "per_outcome"))
  se_method <- match.arg(se_method, c("classical", "HC3"))
  shared <- c(predictor, covariates)
  if (!all(vapply(data[shared], is.numeric, logical(1)))) stop("All model inputs must be numeric; encode categories explicitly before upload.")
  if (length(unique(data[[predictor]][is.finite(data[[predictor]])])) < 2L) stop("The predictor is constant.")
  item_errors <- setNames(vapply(measures, function(item) {
    if (!is.numeric(data[[item]])) return("Outcome is not numeric")
    lag <- if (own_lag) paste0(item, lag_suffix) else character()
    if (length(lag) && !lag %in% names(data)) return("Own-lag column is missing")
    if (length(lag) && !is.numeric(data[[lag]])) return("Own-lag column is not numeric")
    if (length(lag) && lag %in% shared) return("Own-lag duplicates a shared input")
    ""
  }, character(1)), measures)
  common <- NULL
  if (missing == "common") {
    if (any(nzchar(item_errors))) {
      common <- rep(FALSE, nrow(data))
    } else common <- finite_rows(data, unique(c(shared, measures, if (own_lag) paste0(measures, lag_suffix))))
  }
  output <- do.call(rbind, lapply(measures, function(item) {
    out <- data.frame(item=item,n=0L,df=NA_integer_,b=NA_real_,se=NA_real_,ci_lo=NA_real_,ci_hi=NA_real_,t=NA_real_,p=NA_real_,status="", stringsAsFactors=FALSE)
    fail <- function(msg) { out$status <- msg; out }
    if (nzchar(item_errors[[item]])) return(fail(item_errors[[item]]))
    if (missing == "common" && any(nzchar(item_errors))) return(fail("Common-row model unavailable: another requested outcome has invalid inputs"))
    lag <- if (own_lag) paste0(item, lag_suffix) else character()
    inputs <- c(shared, lag)
    keep <- if (missing == "common") common else finite_rows(data, c(item, inputs))
    out$n <- sum(keep)
    p_dim <- 1L + length(inputs)
    out$df <- out$n - p_dim
    if (out$df <= 0L) return(fail("Insufficient complete observations"))
    y <- data[[item]][keep]
    if (length(unique(y)) < 2L) return(fail("Outcome is constant on fitted rows"))
    X <- cbind(1, as.matrix(data[keep, inputs, drop=FALSE]))
    fit <- lm.fit(X, y)
    if (fit$rank != ncol(X)) return(fail("Singular design: predictor/covariates/lag are linearly dependent"))
    inv <- tryCatch(chol2inv(chol(crossprod(X))), error=function(e) NULL)
    if (is.null(inv)) return(fail("Numerically singular design"))
    if (se_method == "classical") V <- sum(fit$residuals^2)/out$df * inv else {
      h <- rowSums((X %*% inv) * X)
      if (any(1-h < sqrt(.Machine$double.eps))) return(fail("HC3 unavailable: leverage is effectively one"))
      V <- inv %*% crossprod(X, X * as.numeric((fit$residuals/(1-h))^2)) %*% inv
    }
    out$b <- unname(fit$coefficients[2L]); out$se <- sqrt(V[2L,2L])
    if (!is.finite(out$se) || out$se <= 0) return(fail("Residual variance does not support an uncertainty estimate"))
    out$t <- out$b/out$se; out$p <- 2*pt(-abs(out$t), out$df)
    critical <- qt(.975, out$df)
    out$ci_lo <- out$b-critical*out$se; out$ci_hi <- out$b+critical*out$se
    out$status <- "ok"
    out
  }))
  rownames(output) <- NULL
  attr(output,"model_settings") <- list(predictor=predictor,covariates=covariates,own_lag=own_lag,lag_suffix=lag_suffix,missing=missing,se_method=se_method,ci_level=.95,HC3_reference=if (se_method == "HC3") "Student t with residual df; heteroskedasticity-robust, not serial-correlation-robust" else NULL)
  output
}

make_report_tables <- function(landscape, focal, neighborhood, metadata=NULL, multiplicity=FALSE, orientation=NULL) {
  if (!all(c("item", "t", "p", "status") %in% names(landscape)) || anyDuplicated(landscape$item)) stop("Provide a valid landscape with one row per outcome.")
  if (length(setdiff(focal, landscape$item))) stop("Every focal outcome must be retained in the landscape, including failed fits.")
  neighborhood <- unique(neighborhood)
  if (length(setdiff(neighborhood, landscape$item))) stop("Neighborhood contains items outside the fitted universe.")
  out <- landscape
  out$focal <- out$item %in% focal; out$in_neighborhood <- out$item %in% neighborhood
  out$abs_t <- abs(out$t)
  valid <- out$status == "ok" & is.finite(out$abs_t)
  out$observed_abs_t_rank_universe <- NA_real_
  out$observed_abs_t_rank_neighborhood <- NA_real_
  out$observed_abs_t_percentile_universe <- NA_real_
  out$observed_abs_t_rank_universe[valid] <- rank(-out$abs_t[valid], ties.method="min")
  out$observed_abs_t_percentile_universe[valid] <- rank(out$abs_t[valid], ties.method="average")/sum(valid)
  local <- valid & out$in_neighborhood
  out$observed_abs_t_rank_neighborhood[local] <- rank(-out$abs_t[local], ties.method="min")
  out$n_unreported_larger_abs_t_neighborhood <- vapply(seq_len(nrow(out)), function(i) {
    if (!valid[i]) return(NA_integer_)
    sum(local & !out$focal & out$abs_t > out$abs_t[i], na.rm=TRUE)
  }, integer(1))
  if (!is.null(metadata) && "item" %in% names(metadata)) {
    for (nm in setdiff(names(metadata), names(out))) out[[nm]] <- metadata[[nm]][match(out$item, metadata$item)]
  }
  if (!is.null(orientation)) {
    if (is.null(names(orientation)) || any(!orientation %in% c(-1,1))) stop("Orientation must be a named vector of -1 or +1 defined from theory and coding.")
    out$orientation_multiplier <- unname(orientation[out$item])
    out$oriented_b <- out$b*out$orientation_multiplier
    out$oriented_t <- out$t*out$orientation_multiplier
    out$oriented_ci_lo <- pmin(out$ci_lo*out$orientation_multiplier, out$ci_hi*out$orientation_multiplier)
    out$oriented_ci_hi <- pmax(out$ci_lo*out$orientation_multiplier, out$ci_hi*out$orientation_multiplier)
  }
  if (multiplicity) {
    out$bonferroni_universe_k <- nrow(landscape)
    out$p_bonferroni_universe <- pmin(1, out$p*nrow(landscape))
    out$bonferroni_neighborhood_k <- length(neighborhood)
    out$p_bonferroni_neighborhood <- ifelse(out$in_neighborhood, pmin(1, out$p*length(neighborhood)), NA_real_)
  }
  list(focal=out[match(focal,out$item), ,drop=FALSE], alternatives=out[out$in_neighborhood & !out$focal, ,drop=FALSE], all=out)
}
