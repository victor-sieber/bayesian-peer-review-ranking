# Model V1: CRS cross-classified Gaussian model
# Main inference model for the CRS Seed Grant 2026 data.
#
# Run from the evaluation-data project root with:
# source("analysis/02_fit_v1_cross_classified.R")

# 1. Check packages

required_packages <- c(
  "ERforResearch",
  "runjags",
  "dplyr",
  "ggplot2"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ",
    paste(
      missing_packages,
      collapse = ", "
    ),
    ". Run analysis/00_install_heyard_dependencies.R first."
  )
}


# 2. Locate JAGS

jags_path <- Sys.which("jags")

if (!nzchar(jags_path)) {
  homebrew_jags <- "/opt/homebrew/bin/jags"

  if (file.exists(homebrew_jags)) {
    jags_path <- homebrew_jags
  }
}

if (!nzchar(jags_path)) {
  stop(
    "JAGS executable not found. Install JAGS and ensure `jags` is available on PATH."
  )
}

runjags::runjags.options(
  jagspath = jags_path
)

message(
  "Using JAGS executable: ",
  jags_path
)

# 3. Paths

data_path <- "data/evaluation_anonymized.csv"

model_path <- file.path(
  "analysis",
  "model",
  "model_v1_cross_classified.txt"
)

results_dir <- file.path(
  "results",
  "model_v1_cross_classified"
)

figures_dir <- file.path(
  "figures",
  "model_v1_cross_classified"
)

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path)
}

if (!file.exists(model_path)) {
  stop("V1 JAGS model not found: ", model_path)
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

# 6. Fit Model V1: cross-classified Gaussian model

# Model V1 has one persistent reviewer-level random effect
# assessor_intercept[l] for each reviewer.
# The separate Heyard parameter nu[l] is not present.
variables_to_sample <- c(
  "proposal_intercept",
  "tau_proposal",
  "tau_assessor",
  "rank_theta",
  "assessor_intercept",
  "sigma"
)

n_chains <- 4L
n_proposals <- length(unique(reviews$proposal_id))
n_reviewers <- length(unique(reviews$reviewer_id))

# Custom initial values are supplied because V1 differs from
# the package's Heyard reference model. In particular, V1 has
# one reviewer-level assessor_intercept per reviewer and no nu[l].
set.seed(20260727)

rng_names <- c(
  "base::Wichmann-Hill",
  "base::Marsaglia-Multicarry",
  "base::Super-Duper",
  "base::Mersenne-Twister"
)

rng_seeds <- sample.int(
  1000000,
  size = n_chains
)

initial_values_v1 <- lapply(
  seq_len(n_chains),
  function(chain) {
    list(
      proposal_intercept = runif(
        n_proposals,
        min = -2,
        max = 2
      ),

      assessor_intercept = runif(
        n_reviewers,
        min = -2,
        max = 2
      ),

      sigma = runif(
        1,
        min = 0.000001,
        max = 2
      ),

      tau_proposal = runif(
        1,
        min = 0.000001,
        max = 2
      ),

      tau_assessor = runif(
        1,
        min = 0.000001,
        max = 2
      ),

      .RNG.name = rng_names[chain],
      .RNG.seed = rng_seeds[chain]
    )
  }
)

message("Starting Model V1 (cross-classified Gaussian). This may take several minutes.")

mcmc_fit <- ERforResearch::get_mcmc_samples(
  data = reviews,
  id_proposal = "proposal_id",
  id_assessor = "reviewer_id",
  grade_variable = "overall_grade",

  # Explicitly use the V1 cross-classified JAGS specification.
  path_to_jags_model = model_path,

  ordinal_scale = FALSE,
  heterogeneous_residuals = FALSE,

  n_chains = n_chains,
  n_iter = 50000,
  n_burnin = 10000,
  n_adapt = 10000,

  # Same as n_iter: prevents repeated automatic extensions.
  max_iter = 50000,

  seed = 20260727,

  # Convergence threshold used for this baseline analysis.
  rhat_threshold = 1.1,

  runjags_method = "parallel",
  quiet = TRUE,

  # nu is deliberately absent from both of these.
  names_variables_to_sample = variables_to_sample,
  initial_values = initial_values_v1
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
  title = "CRS Seed Grant 2026 — V1 cross-classified",
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

message("Model V1 finished.")
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