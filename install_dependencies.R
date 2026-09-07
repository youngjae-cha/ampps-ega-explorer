# Run explicitly on a new machine. The app itself never installs packages.
packages <- c("shiny", "ggplot2", "plotly", "DT", "igraph", "EGAnet", "jsonlite", "htmltools", "zip")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
for (p in packages) cat(p, as.character(packageVersion(p)), "\n")
