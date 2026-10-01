# Check recorded direct dependencies without installing or changing libraries.
runtime_mismatches <- function(app_dir) {
  expected <- read.csv(file.path(app_dir,"package_versions.csv"),stringsAsFactors=FALSE)
  installed <- vapply(expected$package,function(p) {
    if (!requireNamespace(p,quietly=TRUE)) return("missing")
    as.character(utils::packageVersion(p))
  },character(1))
  expected$installed <- unname(installed)
  expected[expected$version != expected$installed,,drop=FALSE]
}
