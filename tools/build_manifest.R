# Run from the repository root after local tests pass.
# An explicit allowlist keeps credentials, QA outputs, and private files out.
options(repos = c(CRAN = "https://cloud.r-project.org"))
app_files <- c("app.R", "package_versions.csv",
  list.files("R", full.names = TRUE, pattern = "\\.R$"),
  list.files("www", full.names = TRUE, recursive = TRUE),
  list.files("data", full.names = TRUE, pattern = "\\.(csv|md)$"),
  file.path("examples", c("upload_example.csv", "metadata_example.csv")))
stopifnot(all(file.exists(app_files)), !anyDuplicated(app_files))
rsconnect::writeManifest(appDir = getwd(), appFiles = app_files,
  appPrimaryDoc = "app.R", appMode = "shiny", quarto = FALSE)
manifest <- jsonlite::read_json("manifest.json", simplifyVector = FALSE)
stopifnot(all(app_files %in% names(manifest$files)))
cat("Manifest prepared for", length(app_files), "allowlisted app files.\n")
