# Model V0: Heyard et al. reference specification
# Direct continuous-model implementation used as the reference model.
#
# Run from the evaluation-data project root with:
# source("analysis/01_fit_v0_heyard_reference.R")

# 1. Make Homebrew JAGS visible to RStudio
jags_path <- "/opt/homebrew/bin/jags"

if (!file.exists(jags_path)) {
  stop("JAGS was not found at ", jags_path, ". Check with `which jags` in Terminal.")
}

Sys.setenv(PATH = paste(dirname(jags_path), Sys.getenv("PATH"), sep = ":"))
runjags::runjags.options(jagspath = jags_path)

# 2. Check packages
required_packages <- c("ERforResearch", "runjags", "dplyr", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Run analysis/00_install_heyard_dependencies.R first."
  )
}

# 3. Paths

data_path <- "data/evaluation_anonymized.csv"

model_path <- file.path(
  "analysis",
  "model",
  "model_v0_heyard_reference.txt"
)

results_dir <- file.path(
  "results",
  "model_v0_heyard_reference"
)

figures_dir <- file.path(
  "figures",
  "model_v0_heyard_reference"
)

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path)
}

if (!file.exists(model_path)) {
  stop("V0 JAGS model not found: ", model_path)
}

message("Using JAGS model: ", model_path)
message("Results directory: ", results_dir)
message("Figures directory: ", figures_dir)

# 4. Read and validate data
reviews <- read.csv(
  data_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

expected_columns <- c(
  "proposal_id",
  "reviewer_id",
  "scientific_quality",
  "community_building",
  "scaling_potential",
  "scientific_rigor",
  "overall_grade"
)

missing_columns <- setdiff(expected_columns, names(reviews))

if (length(missing_columns) > 0) {
  stop("Missing columns: ", paste(missing_columns, collapse = ", "))
}

reviews <- reviews[, expected_columns]

reviews$proposal_id <- trimws(as.character(reviews$proposal_id))
reviews$reviewer_id <- trimws(as.character(reviews$reviewer_id))

score_columns <- c(
  "scientific_quality",
  "community_building",
  "scaling_potential",
  "scientific_rigor",
  "overall_grade"
)

reviews[score_columns] <- lapply(
  reviews[score_columns],
  function(x) as.numeric(as.character(x))
)

if (anyNA(reviews[, c("proposal_id", "reviewer_id", score_columns)])) {
  stop("Missing or non-numeric values were found.")
}

if (any(!reviews$overall_grade %in% 1:5)) {
  stop("overall_grade must contain only values from 1 to 5.")
}

if (any(duplicated(reviews[, c("proposal_id", "reviewer_id")]))) {
  stop("At least one proposal-reviewer pair occurs more than once.")
}

proposal_review_counts <- table(reviews$proposal_id)

if (any(proposal_review_counts != 2)) {
  stop(
    "Each proposal should have exactly two reviews. Problematic proposals: ",
    paste(names(proposal_review_counts)[proposal_review_counts != 2], collapse = ", ")
  )
}

message("Rows imported: ", nrow(reviews))
message("Proposals: ", length(unique(reviews$proposal_id)))
message("Reviewers: ", length(unique(reviews$reviewer_id)))

# 5. Reconstruct original CRS qualification
original_benchmark <- reviews |>
  dplyr::group_by(proposal_id) |>
  dplyr::summarise(
    n_reviews = dplyr::n(),
    mean_overall_grade = mean(overall_grade),
    minimum_grade = min(overall_grade),
    maximum_grade = max(overall_grade),
    qualifies_original =
      n_reviews == 2 &
      maximum_grade == 5 &
      minimum_grade >= 4,
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(mean_overall_grade), proposal_id)

write.csv(
  original_benchmark,
  file.path(results_dir, "original_qualification_benchmark.csv"),
  row.names = FALSE
)

message("Originally qualified proposals: ", sum(original_benchmark$qualifies_original))

# 6. Fit Model V0: Heyard reference model
set.seed(20260727)

message("Starting Model V0 (Heyard reference). This may take several minutes.")

mcmc_fit <- ERforResearch::get_mcmc_samples(
  data = reviews,
  id_proposal = "proposal_id",
  id_assessor = "reviewer_id",
  grade_variable = "overall_grade",
  
  path_to_jags_model = model_path,
  
  ordinal_scale = FALSE,
  heterogeneous_residuals = FALSE,
  
  n_chains = 4,
  n_iter = 50000,
  n_burnin = 10000,
  n_adapt = 10000,
  
  # Same as n_iter: prevents repeated automatic extensions
  max_iter = 50000,
  
  seed = 20260727,
  
  # More practical for this first sparse-data fit
  rhat_threshold = 1.1,
  
  inits_type = "random",
  runjags_method = "parallel",
  quiet = TRUE
)

message("Bayesian sampling finished.")

saveRDS(
  mcmc_fit,
  file.path(results_dir, "mcmc_fit_continuous.rds")
)

write.csv(
  mcmc_fit$summary,
  file.path(results_dir, "mcmc_summary_continuous.csv"),
  row.names = TRUE
)

# 7. Calculate expected ranks
er_results <- ERforResearch::get_er_from_jags(
  data = reviews,
  id_proposal = "proposal_id",
  id_assessor = "reviewer_id",
  grade_variable = "overall_grade",
  ordinal_scale = FALSE,
  heterogeneous_residuals = FALSE,
  mcmc_samples = mcmc_fit,
  rank_pm = TRUE
)

ranking_table <- er_results$rankings |>
  dplyr::left_join(
    original_benchmark |>
      dplyr::select(proposal_id, qualifies_original),
    by = c("id_proposal" = "proposal_id")
  ) |>
  dplyr::arrange(er)

write.csv(
  ranking_table,
  file.path(results_dir, "ranking_continuous.csv"),
  row.names = FALSE
)

saveRDS(
  er_results,
  file.path(results_dir, "expected_rank_results.rds")
)

# 8. Create comparison figure
ranking_plot <- ERforResearch::plotting_er_results(
  er_results = er_results,
  id_proposal = "id_proposal",
  how_many_fundable = NULL,
  title = "CRS Seed Grant 2026 — V0 Heyard reference",
  ordering_increasing = TRUE,
  draw_funding_line = FALSE,
  result_show = TRUE,
  easy_numbering = FALSE
)

ggplot2::ggsave(
  filename = file.path(figures_dir, "ranking_comparison.png"),
  plot = ranking_plot,
  width = 10,
  height = 8,
  dpi = 300
)

# 9. Save reproducibility information
capture.output(
  sessionInfo(),
  file = file.path(results_dir, "session_info.txt")
)

message("Model V0 finished.")
message("Ranking table: ", file.path(results_dir, "ranking_continuous.csv"))
message("Figure: ", file.path(figures_dir, "ranking_comparison.png"))

print(
  ranking_table |>
    dplyr::select(
      id_proposal,
      avg_grade,
      rank,
      rank_pm,
      er,
      pcer,
      qualifies_original
    ) |>
    head(10)
)