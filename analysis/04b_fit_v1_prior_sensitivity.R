# V1 prior sensitivity analysis
#
# Re-fits the CRS cross-classified Gaussian model using alternative
# half-normal priors for the scale parameters.
#
# The likelihood, data, initialization, MCMC settings and ranking
# calculations are identical to analysis/02_fit_v1_cross_classified.R.
#
# Run from the evaluation-data project root with:
# source("analysis/04b_fit_v1_prior_sensitivity.R")


# 1. Make Homebrew JAGS visible to RStudio

jags_path <- "/opt/homebrew/bin/jags"

if (!file.exists(jags_path)) {
  stop(
    "JAGS was not found at ",
    jags_path,
    ". Check with `which jags` in Terminal."
  )
}

Sys.setenv(
  PATH = paste(
    dirname(jags_path),
    Sys.getenv("PATH"),
    sep = ":"
  )
)

runjags::runjags.options(
  jagspath = jags_path
)


# 2. Check packages

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


# 3. Paths

data_path <- "data/evaluation_anonymized.csv"

model_path <- file.path(
  "analysis",
  "model",
  "model_v1_halfnormal_sensitivity.txt"
)

results_dir <- file.path(
  "results",
  "model_v1_prior_sensitivity"
)

figures_dir <- file.path(
  "figures",
  "model_v1_prior_sensitivity"
)

baseline_fit_path <- file.path(
  "results",
  "model_v1_cross_classified",
  "mcmc_fit_continuous.rds"
)

baseline_ranking_path <- file.path(
  "results",
  "model_v1_cross_classified",
  "ranking_continuous.csv"
)

dir.create(
  results_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figures_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(data_path)) {
  stop(
    "Data file not found: ",
    data_path
  )
}

if (!file.exists(model_path)) {
  stop(
    "V1 sensitivity JAGS model not found: ",
    model_path
  )
}

if (!file.exists(baseline_fit_path)) {
  stop(
    "Baseline V1 fit not found: ",
    baseline_fit_path
  )
}

if (!file.exists(baseline_ranking_path)) {
  stop(
    "Baseline V1 ranking not found: ",
    baseline_ranking_path
  )
}

message(
  "Using JAGS model: ",
  model_path
)

message(
  "Results directory: ",
  results_dir
)

message(
  "Figures directory: ",
  figures_dir
)


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

missing_columns <- setdiff(
  expected_columns,
  names(reviews)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}

reviews <- reviews[
  ,
  expected_columns
]

reviews$proposal_id <- trimws(
  as.character(
    reviews$proposal_id
  )
)

reviews$reviewer_id <- trimws(
  as.character(
    reviews$reviewer_id
  )
)

score_columns <- c(
  "scientific_quality",
  "community_building",
  "scaling_potential",
  "scientific_rigor",
  "overall_grade"
)

reviews[score_columns] <- lapply(
  reviews[score_columns],
  function(x) {
    as.numeric(
      as.character(x)
    )
  }
)

if (
  anyNA(
    reviews[
      ,
      c(
        "proposal_id",
        "reviewer_id",
        score_columns
      )
    ]
  )
) {
  stop(
    "Missing or non-numeric values were found."
  )
}

if (
  any(
    !reviews$overall_grade %in% 1:5
  )
) {
  stop(
    "overall_grade must contain only values from 1 to 5."
  )
}

if (
  any(
    duplicated(
      reviews[
        ,
        c(
          "proposal_id",
          "reviewer_id"
        )
      ]
    )
  )
) {
  stop(
    "At least one proposal-reviewer pair occurs more than once."
  )
}

proposal_review_counts <- table(
  reviews$proposal_id
)

if (
  any(
    proposal_review_counts != 2
  )
) {
  stop(
    "Each proposal should have exactly two reviews. Problematic proposals: ",
    paste(
      names(
        proposal_review_counts
      )[
        proposal_review_counts != 2
      ],
      collapse = ", "
    )
  )
}

message(
  "Rows imported: ",
  nrow(reviews)
)

message(
  "Proposals: ",
  length(
    unique(
      reviews$proposal_id
    )
  )
)

message(
  "Reviewers: ",
  length(
    unique(
      reviews$reviewer_id
    )
  )
)


# 5. Reconstruct original CRS qualification

original_benchmark <- reviews |>
  dplyr::group_by(
    proposal_id
  ) |>
  dplyr::summarise(
    n_reviews = dplyr::n(),
    
    mean_overall_grade = mean(
      overall_grade
    ),
    
    minimum_grade = min(
      overall_grade
    ),
    
    maximum_grade = max(
      overall_grade
    ),
    
    qualifies_original =
      n_reviews == 2 &
      maximum_grade == 5 &
      minimum_grade >= 4,
    
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(
      mean_overall_grade
    ),
    proposal_id
  )

write.csv(
  original_benchmark,
  file.path(
    results_dir,
    "original_qualification_benchmark.csv"
  ),
  row.names = FALSE
)

message(
  "Originally qualified proposals: ",
  sum(
    original_benchmark$qualifies_original
  )
)


# 6. Fit Model V1 with alternative priors

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
  unique(
    reviews$proposal_id
  )
)

n_reviewers <- length(
  unique(
    reviews$reviewer_id
  )
)

set.seed(
  20260727
)

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
  seq_len(
    n_chains
  ),
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
      
      .RNG.name = rng_names[
        chain
      ],
      
      .RNG.seed = rng_seeds[
        chain
      ]
    )
  }
)

message(
  "Starting Model V1 prior sensitivity fit. This may take several minutes."
)

mcmc_fit <- ERforResearch::get_mcmc_samples(
  data = reviews,
  
  id_proposal = "proposal_id",
  
  id_assessor = "reviewer_id",
  
  grade_variable = "overall_grade",
  
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
  
  names_variables_to_sample =
    variables_to_sample,
  
  initial_values =
    initial_values_v1
)

message(
  "Bayesian sampling finished."
)

saveRDS(
  mcmc_fit,
  file.path(
    results_dir,
    "mcmc_fit_continuous.rds"
  )
)

write.csv(
  mcmc_fit$summary,
  file.path(
    results_dir,
    "mcmc_summary_continuous.csv"
  ),
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
      dplyr::select(
        proposal_id,
        qualifies_original
      ),
    by = c(
      "id_proposal" =
        "proposal_id"
    )
  ) |>
  dplyr::arrange(
    er
  )

write.csv(
  ranking_table,
  file.path(
    results_dir,
    "ranking_continuous.csv"
  ),
  row.names = FALSE
)

saveRDS(
  er_results,
  file.path(
    results_dir,
    "expected_rank_results.rds"
  )
)


# 8. Create sensitivity-model ranking figure

ranking_plot <- ERforResearch::plotting_er_results(
  er_results = er_results,
  
  id_proposal = "id_proposal",
  
  how_many_fundable = NULL,
  
  title =
    "CRS Seed Grant 2026 — V1 half-normal prior sensitivity",
  
  ordering_increasing = TRUE,
  
  draw_funding_line = FALSE,
  
  result_show = TRUE,
  
  easy_numbering = FALSE
)

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "ranking_comparison.png"
  ),
  plot = ranking_plot,
  width = 10,
  height = 8,
  dpi = 300
)


# 9. Load baseline V1 for comparison

baseline_fit <- readRDS(
  baseline_fit_path
)

baseline_ranking <- read.csv(
  baseline_ranking_path,
  stringsAsFactors = FALSE
)

baseline_draws <- as.matrix(
  baseline_fit$samples
)

sensitivity_draws <- as.matrix(
  mcmc_fit$samples
)


# 10. Compare expected ranks and point ranks

ranking_comparison <- baseline_ranking |>
  dplyr::select(
    id_proposal,
    rank_pm,
    er,
    qualifies_original
  ) |>
  dplyr::rename(
    rank_pm_baseline = rank_pm,
    er_baseline = er
  ) |>
  dplyr::inner_join(
    ranking_table |>
      dplyr::select(
        id_proposal,
        rank_pm,
        er
      ) |>
      dplyr::rename(
        rank_pm_halfnormal = rank_pm,
        er_halfnormal = er
      ),
    by = "id_proposal"
  ) |>
  dplyr::mutate(
    delta_rank_pm =
      rank_pm_halfnormal -
      rank_pm_baseline,
    
    abs_delta_rank_pm =
      abs(
        delta_rank_pm
      ),
    
    delta_er =
      er_halfnormal -
      er_baseline,
    
    abs_delta_er =
      abs(
        delta_er
      )
  ) |>
  dplyr::arrange(
    dplyr::desc(
      abs_delta_er
    )
  )

write.csv(
  ranking_comparison,
  file.path(
    results_dir,
    "ranking_prior_comparison.csv"
  ),
  row.names = FALSE
)


# 11. Compare rank uncertainty

proposal_ids <- unique(
  reviews$proposal_id
)

rank_columns <- paste0(
  "rank_theta[",
  seq_along(
    proposal_ids
  ),
  "]"
)

if (
  !all(
    rank_columns %in%
    colnames(
      baseline_draws
    )
  )
) {
  stop(
    "Baseline posterior rank columns were not found."
  )
}

if (
  !all(
    rank_columns %in%
    colnames(
      sensitivity_draws
    )
  )
) {
  stop(
    "Sensitivity posterior rank columns were not found."
  )
}

summarise_rank_draws <- function(
    draws,
    model
) {
  
  rank_draws <- draws[
    ,
    rank_columns,
    drop = FALSE
  ]
  
  data.frame(
    proposal_id = proposal_ids,
    
    model = model,
    
    expected_rank =
      colMeans(
        rank_draws
      ),
    
    q05 =
      apply(
        rank_draws,
        2,
        stats::quantile,
        probs = 0.05
      ),
    
    q95 =
      apply(
        rank_draws,
        2,
        stats::quantile,
        probs = 0.95
      ),
    
    row.names = NULL
  ) |>
    dplyr::mutate(
      interval_width =
        q95 - q05
    )
}

rank_uncertainty_comparison <-
  dplyr::bind_rows(
    summarise_rank_draws(
      baseline_draws,
      "Baseline Uniform(0,2)"
    ),
    
    summarise_rank_draws(
      sensitivity_draws,
      "Half-normal(0,1)"
    )
  )

write.csv(
  rank_uncertainty_comparison,
  file.path(
    results_dir,
    "rank_uncertainty_comparison.csv"
  ),
  row.names = FALSE
)


# 12. Compare scale parameters

summarise_parameter <- function(
    draws,
    parameter,
    model
) {
  
  values <- draws[
    ,
    parameter
  ]
  
  data.frame(
    model = model,
    
    parameter = parameter,
    
    posterior_mean =
      mean(
        values
      ),
    
    posterior_median =
      median(
        values
      ),
    
    q025 =
      unname(
        stats::quantile(
          values,
          0.025
        )
      ),
    
    q975 =
      unname(
        stats::quantile(
          values,
          0.975
        )
      ),
    
    row.names = NULL
  )
}

scale_parameters <- c(
  "tau_proposal",
  "tau_assessor",
  "sigma"
)

scale_parameter_comparison <-
  dplyr::bind_rows(
    lapply(
      scale_parameters,
      function(parameter) {
        
        dplyr::bind_rows(
          summarise_parameter(
            baseline_draws,
            parameter,
            "Baseline Uniform(0,2)"
          ),
          
          summarise_parameter(
            sensitivity_draws,
            parameter,
            "Half-normal(0,1)"
          )
        )
      }
    )
  )

write.csv(
  scale_parameter_comparison,
  file.path(
    results_dir,
    "scale_parameter_comparison.csv"
  ),
  row.names = FALSE
)


# 13. Compare variance shares

summarise_variance_shares <- function(
    draws,
    model
) {
  
  proposal_variance <-
    draws[
      ,
      "tau_proposal"
    ]^2
  
  reviewer_variance <-
    draws[
      ,
      "tau_assessor"
    ]^2
  
  residual_variance <-
    draws[
      ,
      "sigma"
    ]^2
  
  total_variance <-
    proposal_variance +
    reviewer_variance +
    residual_variance
  
  proposal_share <-
    proposal_variance /
    total_variance
  
  reviewer_share <-
    reviewer_variance /
    total_variance
  
  residual_share <-
    residual_variance /
    total_variance
  
  shares <- list(
    Proposal = proposal_share,
    Reviewer = reviewer_share,
    Residual = residual_share
  )
  
  dplyr::bind_rows(
    lapply(
      names(shares),
      function(component) {
        
        values <- shares[[component]]
        
        data.frame(
          model = model,
          
          component = component,
          
          posterior_mean =
            mean(
              values
            ),
          
          posterior_median =
            median(
              values
            ),
          
          q025 =
            unname(
              stats::quantile(
                values,
                0.025
              )
            ),
          
          q975 =
            unname(
              stats::quantile(
                values,
                0.975
              )
            ),
          
          row.names = NULL
        )
      }
    )
  )
}

variance_share_comparison <-
  dplyr::bind_rows(
    summarise_variance_shares(
      baseline_draws,
      "Baseline Uniform(0,2)"
    ),
    
    summarise_variance_shares(
      sensitivity_draws,
      "Half-normal(0,1)"
    )
  ) |>
  dplyr::mutate(
    component = factor(
      component,
      levels = c(
        "Proposal",
        "Reviewer",
        "Residual"
      )
    )
  )

write.csv(
  variance_share_comparison,
  file.path(
    results_dir,
    "variance_share_comparison.csv"
  ),
  row.names = FALSE
)


# 14. Compare convergence

summarise_convergence <- function(
    fit,
    model
) {
  
  psrf <- fit$summary[
    ,
    "psrf"
  ]
  
  ess <- fit$summary[
    ,
    "SSeff"
  ]
  
  data.frame(
    model = model,
    
    maximum_psrf =
      max(
        psrf,
        na.rm = TRUE
      ),
    
    minimum_ess =
      min(
        ess,
        na.rm = TRUE
      ),
    
    weakest_ess_parameter =
      rownames(
        fit$summary
      )[
        which.min(
          ess
        )
      ],
    
    row.names = NULL
  )
}

convergence_comparison <-
  dplyr::bind_rows(
    summarise_convergence(
      baseline_fit,
      "Baseline Uniform(0,2)"
    ),
    
    summarise_convergence(
      mcmc_fit,
      "Half-normal(0,1)"
    )
  )

write.csv(
  convergence_comparison,
  file.path(
    results_dir,
    "convergence_comparison.csv"
  ),
  row.names = FALSE
)


# 15. Pairwise comparison of P023 and P032

p023_index <- which(
  proposal_ids == "P023"
)

p032_index <- which(
  proposal_ids == "P032"
)

if (
  length(
    p023_index
  ) != 1 ||
  length(
    p032_index
  ) != 1
) {
  stop(
    "Could not uniquely identify P023 and P032."
  )
}

p023_column <- paste0(
  "proposal_intercept[",
  p023_index,
  "]"
)

p032_column <- paste0(
  "proposal_intercept[",
  p032_index,
  "]"
)

pairwise_comparison <- data.frame(
  model = c(
    "Baseline Uniform(0,2)",
    "Half-normal(0,1)"
  ),
  
  probability_P032_gt_P023 = c(
    mean(
      baseline_draws[
        ,
        p032_column
      ] >
        baseline_draws[
          ,
          p023_column
        ]
    ),
    
    mean(
      sensitivity_draws[
        ,
        p032_column
      ] >
        sensitivity_draws[
          ,
          p023_column
        ]
    )
  )
)

write.csv(
  pairwise_comparison,
  file.path(
    results_dir,
    "p023_p032_prior_comparison.csv"
  ),
  row.names = FALSE
)


# 16. Overall sensitivity summary

baseline_widths <-
  rank_uncertainty_comparison |>
  dplyr::filter(
    model ==
      "Baseline Uniform(0,2)"
  ) |>
  dplyr::pull(
    interval_width
  )

sensitivity_widths <-
  rank_uncertainty_comparison |>
  dplyr::filter(
    model ==
      "Half-normal(0,1)"
  ) |>
  dplyr::pull(
    interval_width
  )

sensitivity_summary <- data.frame(
  metric = c(
    "Spearman posterior-mean rank correlation",
    "Pearson expected-rank correlation",
    "Proposals changing posterior-mean rank",
    "Mean absolute posterior-mean rank change",
    "Maximum absolute posterior-mean rank change",
    "Mean absolute expected-rank change",
    "Maximum absolute expected-rank change",
    "Mean baseline 90% rank interval width",
    "Mean half-normal 90% rank interval width",
    "Median baseline 90% rank interval width",
    "Median half-normal 90% rank interval width"
  ),
  
  value = c(
    stats::cor(
      ranking_comparison$rank_pm_baseline,
      ranking_comparison$rank_pm_halfnormal,
      method = "spearman"
    ),
    
    stats::cor(
      ranking_comparison$er_baseline,
      ranking_comparison$er_halfnormal,
      method = "pearson"
    ),
    
    sum(
      ranking_comparison$delta_rank_pm != 0
    ),
    
    mean(
      ranking_comparison$abs_delta_rank_pm
    ),
    
    max(
      ranking_comparison$abs_delta_rank_pm
    ),
    
    mean(
      ranking_comparison$abs_delta_er
    ),
    
    max(
      ranking_comparison$abs_delta_er
    ),
    
    mean(
      baseline_widths
    ),
    
    mean(
      sensitivity_widths
    ),
    
    median(
      baseline_widths
    ),
    
    median(
      sensitivity_widths
    )
  )
)

write.csv(
  sensitivity_summary,
  file.path(
    results_dir,
    "sensitivity_summary.csv"
  ),
  row.names = FALSE
)


# 17. Expected-rank sensitivity figure

expected_rank_plot <-
  ggplot2::ggplot(
    ranking_comparison,
    ggplot2::aes(
      x = er_baseline,
      y = er_halfnormal
    )
  ) +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_text(
    data =
      ranking_comparison |>
      dplyr::filter(
        qualifies_original
      ),
    ggplot2::aes(
      label = id_proposal
    ),
    vjust = -0.7,
    size = 3,
    check_overlap = TRUE
  ) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    title =
      "Expected ranks under alternative V1 priors",
    
    subtitle =
      "Baseline Uniform(0,2) versus half-normal(0,1)",
    
    x =
      "Baseline V1 expected rank",
    
    y =
      "Half-normal V1 expected rank"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "expected_rank_prior_sensitivity.png"
  ),
  plot = expected_rank_plot,
  width = 8,
  height = 7,
  dpi = 300
)


# 18. Variance-share sensitivity figure

variance_share_plot <-
  ggplot2::ggplot(
    variance_share_comparison,
    ggplot2::aes(
      x = component,
      y = posterior_median,
      group = model,
      shape = model
    )
  ) +
  ggplot2::geom_point(
    position =
      ggplot2::position_dodge(
        width = 0.35
      ),
    size = 3
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = q025,
      ymax = q975
    ),
    position =
      ggplot2::position_dodge(
        width = 0.35
      ),
    width = 0.12
  ) +
  ggplot2::coord_cartesian(
    ylim = c(
      0,
      1
    )
  ) +
  ggplot2::labs(
    title =
      "V1 variance shares under alternative priors",
    
    subtitle =
      "Points show posterior medians; intervals show 95% credible intervals",
    
    x = NULL,
    
    y =
      "Share of total model variance",
    
    shape =
      "Prior specification"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "variance_share_prior_sensitivity.png"
  ),
  plot = variance_share_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# 19. Save reproducibility information

capture.output(
  sessionInfo(),
  file = file.path(
    results_dir,
    "session_info.txt"
  )
)


# 20. Print results

message("")
message(
  "V1 prior sensitivity analysis finished."
)

message("")
message(
  "Convergence comparison:"
)

print(
  convergence_comparison
)

message("")
message(
  "Scale-parameter comparison:"
)

print(
  scale_parameter_comparison
)

message("")
message(
  "Variance-share comparison:"
)

print(
  variance_share_comparison
)

message("")
message(
  "Ranking sensitivity summary:"
)

print(
  sensitivity_summary
)

message("")
message(
  "P023 versus P032:"
)

print(
  pairwise_comparison
)

message("")
message(
  "Largest expected-rank changes:"
)

print(
  ranking_comparison |>
    dplyr::select(
      id_proposal,
      er_baseline,
      er_halfnormal,
      delta_er,
      rank_pm_baseline,
      rank_pm_halfnormal,
      delta_rank_pm
    ) |>
    head(
      10
    )
)

message("")
message(
  "Results directory: ",
  results_dir
)

message(
  "Figures directory: ",
  figures_dir
)