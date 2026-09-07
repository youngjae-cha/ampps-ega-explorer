# Run from app folder: Rscript tests/test_ega.R
source(file.path("R", "ega.R"))
expect_error <- function(code, pattern) {
  message <- tryCatch({ force(code); NULL }, error = function(e) conditionMessage(e))
  stopifnot(!is.null(message), grepl(pattern, message))
}

demo <- load_demo_network("data")
stopifnot(nrow(demo$adjacency) == 44L,
          sum(demo$adjacency[upper.tri(demo$adjacency)] != 0) == 262L,
          all(diag(demo$adjacency) == 0),
          identical(names(demo$membership), rownames(demo$adjacency)),
          all(is.finite(demo$coordinates)))
boundaries <- compare_neighborhoods(demo, c("Happy", "Trust", "Fair"))
stopifnot(identical(as.integer(boundaries$summary$k), c(11L, 10L, 14L, 18L)))
stopifnot(max(abs(boundaries$summary$cutoff[-1L] -
                 c(4.90995417498, 7.44158875061, 9.69391250016))) < 1e-8)
stopifnot(identical(sort(setdiff(boundaries$sets$community, boundaries$sets$ring15)), "News"))
stopifnot(identical(sort(setdiff(boundaries$sets$ring25, boundaries$sets$community)),
                    sort(c("Conbus", "Conjudge", "Consci"))))
stopifnot(identical(sort(setdiff(boundaries$sets$ring35, boundaries$sets$ring25)),
                    sort(c("Reliten", "Attend", "Conmedic", "Confed"))))
stopifnot(all(c("Health", "Life") %in% Reduce(intersect, boundaries$sets)))
stopifnot(all(demo$stability$stability[demo$stability$item %in%
            boundaries$sets$community] >= .982))
multi <- compare_neighborhoods(demo, c("Happy", "Conbus"))
stopifnot(all(c("Happy", "Conbus") %in% multi$sets$community),
          length(unique(demo$membership[multi$sets$community])) == 2L)
allfocal <- compare_neighborhoods(demo, rownames(demo$adjacency))
stopifnot(all(allfocal$summary$k == 44L), all(is.na(allfocal$summary$cutoff)))

# Tied shortest paths and isolated components never silently disappear.
a <- matrix(0, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
a["A", c("B", "C")] <- a[c("B", "C"), "A"] <- .5
a["D", "E"] <- a["E", "D"] <- .5
toy <- list(adjacency = a, membership = setNames(c(1L,1L,1L,2L,2L), LETTERS[1:5]))
ties <- compare_neighborhoods(toy, "A")
stopifnot(all(ties$summary$k == 3L), all(ties$summary$unreachable_excluded == 2L),
          all(ties$summary$ties_at_cutoff[-1L] == 2L),
          identical(sort(ties$sets$ring15), LETTERS[1:3]))
a[,] <- 0
empty <- compare_neighborhoods(list(adjacency = a,
   membership = setNames(1:5, LETTERS[1:5])), "A")
stopifnot(all(empty$summary$k == 1L), all(is.na(empty$summary$cutoff)))
expect_error(compare_neighborhoods(demo, character()), "at least one")
expect_error(compare_neighborhoods(demo, "not-present"), "present")
expect_error(compare_neighborhoods(demo, "Happy", c(15, 15)), "distinct")

# Real EGAnet execution on data with two correlated-outcome blocks. We verify
# computation/reproducibility, not that a particular recovered partition is truth.
set.seed(20260907)
n <- 180L
latent <- matrix(rnorm(n * 2L), n, 2L)
raw <- as.data.frame(sapply(1:12, function(j)
  .8 * latent[, if (j <= 6) 1L else 2L] + .6 * rnorm(n)))
names(raw) <- paste0("Outcome", 1:12)
raw$unused_predictor <- rnorm(n)
measures <- names(raw)[1:12]
before <- .Random.seed
preview <- compute_ega(raw, measures, seed = 123L)
stopifnot(identical(before, .Random.seed), preview$n == n,
          all(is.na(preview$stability$stability)),
          sum(preview$adjacency[upper.tri(preview$adjacency)] != 0) == 3L * 12L - 6L,
          all(is.finite(preview$coordinates)),
          max(abs(preview$correlation - stats::cor(raw[, measures]))) < 1e-12)
changed <- raw
changed$unused_predictor <- changed$unused_predictor * 100
preview2 <- compute_ega(changed, measures, seed = 123L)
stopifnot(identical(preview$adjacency, preview2$adjacency),
          identical(preview$membership, preview2$membership),
          identical(preview$coordinates, preview2$coordinates))
boot <- compute_ega(raw, measures, bootstrap = TRUE, iter = 10L, seed = 123L)
boot2 <- compute_ega(raw, measures, bootstrap = TRUE, iter = 10L, seed = 123L)
stopifnot(identical(boot$adjacency, boot2$adjacency),
          identical(boot$membership, boot2$membership),
          identical(boot$stability, boot2$stability),
          all(boot$stability$stability >= 0 & boot$stability$stability <= 1),
          boot$settings$ncores == 1L, boot$settings$iter == 10L)
boot20 <- compute_ega(raw, measures, bootstrap = TRUE, iter = 20L, seed = 321L)
stopifnot(all(is.finite(boot20$stability$stability)), boot20$settings$iter == 20L)
missing <- raw
missing[1:3, "Outcome1"] <- NA_real_
missing[4, "Outcome2"] <- Inf
filtered <- compute_ega(missing, measures, seed = 123L, corr = "spearman")
stopifnot(filtered$n == n - 4L, filtered$settings$excluded_rows == 4L,
          length(filtered$warnings) > 0L)
bad <- raw
bad$Outcome1 <- 1
expect_error(compute_ega(bad, measures), "Constant outcomes")
expect_error(compute_ega(raw, measures[1:3]), "4–200")
expect_error(compute_ega(raw[1:3, ], measures), "at least 10")
expect_error(compute_ega(raw, measures, corr = "auto"), "Pearson or Spearman")
expect_error(compute_ega(raw, measures, bootstrap = TRUE, iter = 2), "10 to 500")
bad <- raw
bad$Outcome2 <- bad$Outcome1
expect_error(compute_ega(bad, measures), "Perfectly correlated")

# Regression: EGAnet 2.3.0's TMFG drops a one-row matrix for k=5 and runs
# 5:4 for k=4. Its typical-structure bootstrap also misnames the LE algorithm.
# The private compatibility guards preserve the installed package namespace
# and produce the same native TMFG graph whenever native TMFG already works.
original_tmfg_body <- body(EGAnet::TMFG)
original_typical_body <- body(getFromNamespace("estimate_typicalStructure", "EGAnet"))
set.seed(5097)
small <- data.frame(X = rnorm(100), Y1 = rnorm(100), Y2 = rnorm(100),
                    Y3 = rnorm(100), Y4 = rnorm(100), Y5 = rnorm(100))
small$Y1 <- .25 * small$X + small$Y1
small_result <- compute_ega(small, paste0("Y", 1:5), seed = 5097L)
stopifnot(nrow(small_result$adjacency) == 5L, all(is.finite(small_result$adjacency)))
for (k in 4:6) {
  set.seed(12)
  independent <- as.data.frame(matrix(rnorm(100 * k), 100, k))
  preview_small <- compute_ega(independent, names(independent), seed = 5097L)
  bootstrap_small <- compute_ega(independent, names(independent),
                                 bootstrap = TRUE, iter = 10L, seed = 5097L)
  stopifnot(nrow(preview_small$adjacency) == k,
            sum(preview_small$adjacency[upper.tri(preview_small$adjacency)] != 0) == 3 * k - 6,
            all(is.finite(bootstrap_small$stability$stability)),
            bootstrap_small$n == 100L)
}
set.seed(111)
single_factor <- rnorm(120)
single_factor_data <- as.data.frame(sapply(1:12, function(j)
  .95 * single_factor + .05 * rnorm(120)))
single_factor_boot <- compute_ega(single_factor_data, names(single_factor_data),
                                  bootstrap = TRUE, iter = 10L, seed = 22L)
stopifnot(all(is.finite(single_factor_boot$stability$stability)))
if (as.character(utils::packageVersion("EGAnet")) == "2.3.0") {
  patched_tmfg <- get("TMFG", envir = environment(.ega_api(5L)$EGA))
  for (k in c(6L, 8L, 12L)) {
    set.seed(k)
    cor_input <- stats::cor(matrix(rnorm(100 * k), 100, k))
    dimnames(cor_input) <- list(paste0("Y", seq_len(k)), paste0("Y", seq_len(k)))
    stopifnot(identical(EGAnet::TMFG(cor_input, n = 100, corr = "pearson"),
                        patched_tmfg(cor_input, n = 100, corr = "pearson")))
  }
  stopifnot(grepl("private compatibility", small_result$settings$compatibility))
}
stopifnot(identical(body(EGAnet::TMFG), original_tmfg_body),
          identical(body(getFromNamespace("estimate_typicalStructure", "EGAnet")), original_typical_body))
cat("PASS: demo 44 nodes / 262 edges; community and rings 11/10/14/18; exact cutoffs/memberships; ties, disconnected, all-focal; live TMFG+Walktrap; bootstrap10/20; reproducibility; outcome-only, missingness and invalid-input checks.\n")
cat("PASS: k4/5/6 independent-data preview and bootstrap; upstream small-matrix and LE-alias regression; native TMFG equivalence at k6/8/12; installed namespace unchanged.\n")
