# Run once in a new session
cran_packages <- c(
  "remotes", "runjags", "dplyr", "tidyr", "ggplot2", "coda",
  "bayesplot", "ggridges", "stringr", "ggrepel", "tibble", "igraph"
)

missing_packages <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

if (!requireNamespace("ERforResearch", quietly = TRUE)) {
  remotes::install_github("snsf-data/ERforResearch")
}

message("Required R packages are installed.")