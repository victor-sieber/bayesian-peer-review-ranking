# Run once in a new session
cran_packages <- c(
  "remotes", "runjags", "dplyr", "tidyr", "ggplot2", "coda",
  "bayesplot", "ggridges", "stringr", "ggrepel", "tibble"
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

# Check if the modified JAGS txt file exists
MODEL_PATH <- file.path(
  "analysis",
  "model",
  "modified_jags_model.txt"
)

if (!file.exists(MODEL_PATH)) {
  warning(
    "Modified JAGS model not found at: ",
    MODEL_PATH
  )
} else {
  message("Modified JAGS model found at: ", MODEL_PATH)
}