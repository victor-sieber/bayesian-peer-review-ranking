# Fit V1 to the equal-weight average of the four supporting criteria
# and compare the resulting Bayesian ranking with the baseline V1 model
# fitted to the holistic overall grade.
#
# Run from the evaluation-data project root with:
# source("analysis/06_fit_compare_v1_criterion_average.R")

# 1. Check packages

required_packages <- c(
  "ERforResearch",
  "runjags",
  "dplyr",
  "ggplot2",
  "ggrepel",
  "tibble"
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

baseline_ranking_path <- file.path(
  "results",
  "model_v1_cross_classified",
  "ranking_continuous.csv"
)

baseline_fit_path <- file.path(
  "results",
  "model_v1_cross_classified",
  "mcmc_fit_continuous.rds"
)

model_results_dir <- file.path(
  "results",
  "model_v1_criterion_average"
)

model_figures_dir <- file.path(
  "figures",
  "model_v1_criterion_average"
)

comparison_results_dir <- file.path(
  "results",
  "criterion_model_comparison"
)

comparison_figures_dir <- file.path(
  "figures",
  "criterion_model_comparison"
)

for (directory in c(
  model_results_dir,
  model_figures_dir,
  comparison_results_dir,
  comparison_figures_dir
)) {
  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

input_files <- c(
  data_path,
  model_path,
  baseline_ranking_path,
  baseline_fit_path
)

missing_files <- input_files[
  !file.exists(input_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing required input files: ",
    paste(missing_files, collapse = ", ")
  )
}

# 4. Read and validate data

reviews <- read.csv(
  data_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

criterion_columns <- c(
  "scientific_quality",
  "community_building",
  "scaling_potential",
  "scientific_rigor"
)

expected_columns <- c(
  "proposal_id",
  "reviewer_id",
  criterion_columns,
  "overall_grade"
)

missing_columns <- setdiff(
  expected_columns,
  names(reviews)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

reviews <- reviews[, expected_columns]

reviews$proposal_id <- trimws(
  as.character(reviews$proposal_id)
)

reviews$reviewer_id <- trimws(
  as.character(reviews$reviewer_id)
)

score_columns <- c(
  criterion_columns,
  "overall_grade"
)

reviews[score_columns] <- lapply(
  reviews[score_columns],
  function(x) as.numeric(as.character(x))
)

if (
  anyNA(
    reviews[, c("proposal_id", "reviewer_id", score_columns)]
  )
) {
  stop("Missing or non-numeric values were found.")
}

if (
  any(
    !unlist(reviews[score_columns], use.names = FALSE) %in% 1:5
  )
) {
  stop("All criterion scores and overall grades must lie on the integer 1-5 scale.")
}

if (
  any(
    duplicated(
      reviews[, c("proposal_id", "reviewer_id")]
    )
  )
) {
  stop("At least one proposal-reviewer pair occurs more than once.")
}

proposal_review_counts <- table(
  reviews$proposal_id
)

if (any(proposal_review_counts != 2)) {
  stop(
    "Each proposal should have exactly two reviews. Problematic proposals: ",
    paste(
      names(proposal_review_counts)[proposal_review_counts != 2],
      collapse = ", "
    )
  )
}

reviews$criterion_average <- rowMeans(
  reviews[, criterion_columns]
)

message("Rows imported: ", nrow(reviews))
message("Criterion-average range: ", paste(range(reviews$criterion_average), collapse = " to "))

# 5. Reconstruct original CRS qualification

original_benchmark <- reviews |>
  dplyr::group_by(proposal_id) |>
  dplyr::summarise(
    n_reviews = dplyr::n(),
    mean_overall_grade = mean(overall_grade),
    mean_criterion_average = mean(criterion_average),
    minimum_grade = min(overall_grade),
    maximum_grade = max(overall_grade),
    qualifies_original =
      n_reviews == 2 &
      maximum_grade == 5 &
      minimum_grade >= 4,
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(mean_overall_grade),
    proposal_id
  )

write.csv(
  original_benchmark,
  file.path(
    model_results_dir,
    "original_qualification_benchmark.csv"
  ),
  row.names = FALSE
)

# 6. Fit V1 to the equal-weight criterion average

variables_to_sample <- c(
  "proposal_intercept",
  "tau_proposal",
  "tau_assessor",
  "rank_theta",
  "assessor_intercept",
  "sigma"
)

n_chains <- 4L
n_proposals <- length(
  unique(reviews$proposal_id)
)

n_reviewers <- length(
  unique(reviews$reviewer_id)
)

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

message("Starting V1 fit using the equal-weight criterion average.")

criterion_fit <- ERforResearch::get_mcmc_samples(
  data = reviews,
  id_proposal = "proposal_id",
  id_assessor = "reviewer_id",
  grade_variable = "criterion_average",
  path_to_jags_model = model_path,
  ordinal_scale = FALSE,
  heterogeneous_residuals = FALSE,
  n_chains = n_chains,
  n_iter = 50000,
  n_burnin = 10000,
  n_adapt = 10000,
  max_iter = 50000,
  seed = 20260727,
  rhat_threshold = 1.1,
  runjags_method = "parallel",
  quiet = TRUE,
  names_variables_to_sample = variables_to_sample,
  initial_values = initial_values_v1
)

message("Criterion-average Bayesian sampling finished.")

saveRDS(
  criterion_fit,
  file.path(
    model_results_dir,
    "mcmc_fit_continuous.rds"
  )
)

write.csv(
  criterion_fit$summary,
  file.path(
    model_results_dir,
    "mcmc_summary_continuous.csv"
  ),
  row.names = TRUE
)

# 7. Calculate expected ranks for the criterion-average outcome

criterion_er_results <- ERforResearch::get_er_from_jags(
  data = reviews,
  id_proposal = "proposal_id",
  id_assessor = "reviewer_id",
  grade_variable = "criterion_average",
  ordinal_scale = FALSE,
  heterogeneous_residuals = FALSE,
  mcmc_samples = criterion_fit,
  rank_pm = TRUE
)

criterion_ranking <- criterion_er_results$rankings |>
  dplyr::left_join(
    original_benchmark |>
      dplyr::select(
        proposal_id,
        qualifies_original
      ),
    by = c(
      "id_proposal" = "proposal_id"
    )
  ) |>
  dplyr::arrange(er)

write.csv(
  criterion_ranking,
  file.path(
    model_results_dir,
    "ranking_continuous.csv"
  ),
  row.names = FALSE
)

saveRDS(
  criterion_er_results,
  file.path(
    model_results_dir,
    "expected_rank_results.rds"
  )
)

criterion_ranking_plot <- ERforResearch::plotting_er_results(
  er_results = criterion_er_results,
  id_proposal = "id_proposal",
  how_many_fundable = NULL,
  title = "CRS Seed Grant 2026 — V1 criterion average",
  ordering_increasing = TRUE,
  draw_funding_line = FALSE,
  result_show = TRUE,
  easy_numbering = FALSE
)

ggplot2::ggsave(
  filename = file.path(
    model_figures_dir,
    "ranking_comparison.png"
  ),
  plot = criterion_ranking_plot,
  width = 10,
  height = 8,
  dpi = 300
)

# 8. Load baseline V1 and reconstruct proposal mapping

baseline_ranking <- read.csv(
  baseline_ranking_path,
  stringsAsFactors = FALSE
)

baseline_fit <- readRDS(
  baseline_fit_path
)

proposal_map <- reviews |>
  dplyr::distinct(proposal_id) |>
  dplyr::mutate(
    num_proposal = dplyr::row_number()
  )

if (nrow(proposal_map) != 42) {
  stop("Expected 42 proposals.")
}

baseline_draws <- as.matrix(
  baseline_fit$samples
)

criterion_draws <- as.matrix(
  criterion_fit$samples
)

proposal_columns <- paste0(
  "proposal_intercept[",
  proposal_map$num_proposal,
  "]"
)

rank_columns <- paste0(
  "rank_theta[",
  proposal_map$num_proposal,
  "]"
)

if (
  !all(proposal_columns %in% colnames(baseline_draws)) ||
  !all(proposal_columns %in% colnames(criterion_draws))
) {
  stop("Proposal-intercept columns could not be found in the MCMC samples.")
}

if (
  !all(rank_columns %in% colnames(baseline_draws)) ||
  !all(rank_columns %in% colnames(criterion_draws))
) {
  stop("Rank-theta columns could not be found in the MCMC samples.")
}

# 9. Ranking comparison

baseline_for_comparison <- baseline_ranking |>
  dplyr::select(
    id_proposal,
    avg_grade,
    rank,
    rank_pm,
    er,
    pcer,
    qualifies_original
  ) |>
  dplyr::rename(
    mean_overall_grade = avg_grade,
    raw_rank_overall = rank,
    rank_pm_overall = rank_pm,
    er_overall = er,
    pcer_overall = pcer
  )

criterion_for_comparison <- criterion_ranking |>
  dplyr::select(
    id_proposal,
    avg_grade,
    rank,
    rank_pm,
    er,
    pcer
  ) |>
  dplyr::rename(
    mean_criterion_average = avg_grade,
    raw_rank_criterion = rank,
    rank_pm_criterion = rank_pm,
    er_criterion = er,
    pcer_criterion = pcer
  )

ranking_comparison <- baseline_for_comparison |>
  dplyr::inner_join(
    criterion_for_comparison,
    by = "id_proposal"
  ) |>
  dplyr::mutate(
    delta_rank_pm =
      rank_pm_criterion - rank_pm_overall,
    abs_delta_rank_pm = abs(delta_rank_pm),
    delta_er = er_criterion - er_overall,
    abs_delta_er = abs(delta_er),
    rank_pm_changed = delta_rank_pm != 0
  ) |>
  dplyr::arrange(
    dplyr::desc(abs_delta_er),
    dplyr::desc(abs_delta_rank_pm),
    id_proposal
  )

if (nrow(ranking_comparison) != 42) {
  stop("Overall-grade and criterion-average ranking merge did not produce 42 proposals.")
}

write.csv(
  ranking_comparison,
  file.path(
    comparison_results_dir,
    "ranking_comparison.csv"
  ),
  row.names = FALSE
)

original_qualifier_comparison <- ranking_comparison |>
  dplyr::filter(qualifies_original) |>
  dplyr::arrange(er_criterion)

write.csv(
  original_qualifier_comparison,
  file.path(
    comparison_results_dir,
    "original_qualifier_comparison.csv"
  ),
  row.names = FALSE
)

# 10. Posterior rank uncertainty

summarise_rank_uncertainty <- function(
    draws,
    model_name
) {
  ranks <- draws[
    ,
    rank_columns,
    drop = FALSE
  ]

  tibble::tibble(
    proposal_id = proposal_map$proposal_id,
    model = model_name,
    expected_rank_from_draws = colMeans(ranks),
    rank_median = apply(
      ranks,
      2,
      median
    ),
    rank_q05 = apply(
      ranks,
      2,
      quantile,
      probs = 0.05
    ),
    rank_q95 = apply(
      ranks,
      2,
      quantile,
      probs = 0.95
    )
  ) |>
    dplyr::mutate(
      rank_interval_90_width =
        rank_q95 - rank_q05
    )
}

rank_uncertainty <- dplyr::bind_rows(
  summarise_rank_uncertainty(
    baseline_draws,
    "V1 overall grade"
  ),
  summarise_rank_uncertainty(
    criterion_draws,
    "V1 criterion average"
  )
)

write.csv(
  rank_uncertainty,
  file.path(
    comparison_results_dir,
    "rank_uncertainty_comparison.csv"
  ),
  row.names = FALSE
)

baseline_widths <- rank_uncertainty |>
  dplyr::filter(
    model == "V1 overall grade"
  ) |>
  dplyr::pull(
    rank_interval_90_width
  )

criterion_widths <- rank_uncertainty |>
  dplyr::filter(
    model == "V1 criterion average"
  ) |>
  dplyr::pull(
    rank_interval_90_width
  )

# 11. Observed outcome scales

outcome_scale_summary <- tibble::tibble(
  outcome = c(
    "Holistic overall grade",
    "Equal-weight criterion average"
  ),
  observed_mean = c(
    mean(reviews$overall_grade),
    mean(reviews$criterion_average)
  ),
  observed_sd = c(
    stats::sd(reviews$overall_grade),
    stats::sd(reviews$criterion_average)
  ),
  observed_minimum = c(
    min(reviews$overall_grade),
    min(reviews$criterion_average)
  ),
  observed_maximum = c(
    max(reviews$overall_grade),
    max(reviews$criterion_average)
  )
)

write.csv(
  outcome_scale_summary,
  file.path(
    comparison_results_dir,
    "outcome_scale_summary.csv"
  ),
  row.names = FALSE
)

# 12. Scale parameters and variance shares

summarise_scale_parameter <- function(
    draws,
    model_name,
    parameter_name
) {
  x <- draws[, parameter_name]

  tibble::tibble(
    model = model_name,
    parameter = parameter_name,
    posterior_mean = mean(x),
    posterior_median = median(x),
    q025 = stats::quantile(
      x,
      probs = 0.025,
      names = FALSE
    ),
    q975 = stats::quantile(
      x,
      probs = 0.975,
      names = FALSE
    )
  )
}

scale_parameter_comparison <- c(
  lapply(
    c("tau_proposal", "tau_assessor", "sigma"),
    function(parameter_name) {
      summarise_scale_parameter(
        baseline_draws,
        "V1 overall grade",
        parameter_name
      )
    }
  ),
  lapply(
    c("tau_proposal", "tau_assessor", "sigma"),
    function(parameter_name) {
      summarise_scale_parameter(
        criterion_draws,
        "V1 criterion average",
        parameter_name
      )
    }
  )
) |>
  dplyr::bind_rows()

write.csv(
  scale_parameter_comparison,
  file.path(
    comparison_results_dir,
    "scale_parameter_comparison.csv"
  ),
  row.names = FALSE
)

summarise_variance_shares <- function(
    draws,
    model_name
) {
  tau_proposal <- draws[, "tau_proposal"]
  tau_assessor <- draws[, "tau_assessor"]
  sigma <- draws[, "sigma"]

  total_variance <-
    tau_proposal^2 +
    tau_assessor^2 +
    sigma^2

  share_data <- tibble::tibble(
    proposal = tau_proposal^2 / total_variance,
    reviewer = tau_assessor^2 / total_variance,
    residual = sigma^2 / total_variance
  )

  lapply(
    names(share_data),
    function(component_name) {
      x <- share_data[[component_name]]

      tibble::tibble(
        model = model_name,
        component = component_name,
        posterior_mean = mean(x),
        posterior_median = median(x),
        q025 = stats::quantile(
          x,
          probs = 0.025,
          names = FALSE
        ),
        q975 = stats::quantile(
          x,
          probs = 0.975,
          names = FALSE
        )
      )
    }
  ) |>
    dplyr::bind_rows()
}

variance_share_comparison <- dplyr::bind_rows(
  summarise_variance_shares(
    baseline_draws,
    "V1 overall grade"
  ),
  summarise_variance_shares(
    criterion_draws,
    "V1 criterion average"
  )
)

write.csv(
  variance_share_comparison,
  file.path(
    comparison_results_dir,
    "variance_share_comparison.csv"
  ),
  row.names = FALSE
)

# 13. Convergence comparison

convergence_row <- function(
    fit,
    model_name
) {
  psrf <- fit$summary[, "psrf"]
  ess <- fit$summary[, "SSeff"]

  tibble::tibble(
    model = model_name,
    max_psrf = max(
      psrf,
      na.rm = TRUE
    ),
    worst_psrf_parameter = rownames(fit$summary)[
      which.max(psrf)
    ],
    min_effective_sample_size = min(
      ess,
      na.rm = TRUE
    ),
    worst_ess_parameter = rownames(fit$summary)[
      which.min(ess)
    ]
  )
}

convergence_comparison <- dplyr::bind_rows(
  convergence_row(
    baseline_fit,
    "V1 overall grade"
  ),
  convergence_row(
    criterion_fit,
    "V1 criterion average"
  )
)

write.csv(
  convergence_comparison,
  file.path(
    comparison_results_dir,
    "convergence_comparison.csv"
  ),
  row.names = FALSE
)

# 14. Main comparison summary

criterion_top_four <- ranking_comparison |>
  dplyr::arrange(rank_pm_criterion) |>
  dplyr::slice_head(n = 4)

comparison_summary <- tibble::tibble(
  metric = c(
    "Spearman correlation of posterior-mean ranks",
    "Pearson correlation of expected ranks",
    "Number of proposals changing posterior-mean rank",
    "Mean absolute posterior-mean rank change",
    "Maximum absolute posterior-mean rank change",
    "Mean absolute expected-rank change",
    "Maximum absolute expected-rank change",
    "Mean 90% rank interval width overall-grade V1",
    "Mean 90% rank interval width criterion-average V1",
    "Median 90% rank interval width overall-grade V1",
    "Median 90% rank interval width criterion-average V1",
    "Number of original qualifiers in criterion-average posterior-mean top four"
  ),
  value = c(
    stats::cor(
      ranking_comparison$rank_pm_overall,
      ranking_comparison$rank_pm_criterion,
      method = "spearman"
    ),
    stats::cor(
      ranking_comparison$er_overall,
      ranking_comparison$er_criterion,
      method = "pearson"
    ),
    sum(ranking_comparison$rank_pm_changed),
    mean(ranking_comparison$abs_delta_rank_pm),
    max(ranking_comparison$abs_delta_rank_pm),
    mean(ranking_comparison$abs_delta_er),
    max(ranking_comparison$abs_delta_er),
    mean(baseline_widths),
    mean(criterion_widths),
    median(baseline_widths),
    median(criterion_widths),
    sum(criterion_top_four$qualifies_original)
  )
)

write.csv(
  comparison_summary,
  file.path(
    comparison_results_dir,
    "comparison_summary.csv"
  ),
  row.names = FALSE
)

# 15. Expected-rank comparison figure

label_proposals <- ranking_comparison |>
  dplyr::arrange(
    dplyr::desc(abs_delta_er),
    id_proposal
  ) |>
  dplyr::slice_head(n = 5) |>
  dplyr::pull(id_proposal)

label_proposals <- union(
  label_proposals,
  ranking_comparison |>
    dplyr::filter(qualifies_original) |>
    dplyr::pull(id_proposal)
)

expected_rank_plot <- ggplot2::ggplot(
  ranking_comparison,
  ggplot2::aes(
    x = er_overall,
    y = er_criterion
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  ggrepel::geom_text_repel(
    data = ranking_comparison |>
      dplyr::filter(
        id_proposal %in% label_proposals
      ),
    ggplot2::aes(label = id_proposal),
    seed = 20260727,
    max.overlaps = Inf,
    box.padding = 0.3,
    point.padding = 0.2
  ) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    title = "Expected ranks: overall grade vs criterion average",
    subtitle = "Original qualifiers and largest expected-rank changes labelled",
    x = "V1 overall-grade expected rank",
    y = "V1 criterion-average expected rank"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(
    comparison_figures_dir,
    "expected_rank_overall_vs_criterion_average.png"
  ),
  plot = expected_rank_plot,
  width = 8,
  height = 7,
  dpi = 300
)

# 16. Reproducibility information

capture.output(
  sessionInfo(),
  file = file.path(
    model_results_dir,
    "session_info.txt"
  )
)

message("Criterion-average V1 fit and comparison finished.")
message("Model results: ", model_results_dir)
message("Comparison results: ", comparison_results_dir)
message("Comparison figure: ", comparison_figures_dir)

print(comparison_summary)
print(outcome_scale_summary)
print(convergence_comparison)
print(scale_parameter_comparison)
print(variance_share_comparison)

print(
  ranking_comparison |>
    dplyr::select(
      id_proposal,
      qualifies_original,
      rank_pm_overall,
      rank_pm_criterion,
      er_overall,
      er_criterion,
      delta_er
    ) |>
    head(10)
)