# Validate Model V1: CRS cross-classified Gaussian model
#
# This script uses the previously fitted V1 posterior.
# It does not refit the baseline model.
#
# Run from the evaluation-data project root with:
# source("analysis/04_validate_v1.R")


# 1. Packages

required_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "tibble",
  "coda"
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
    paste(missing_packages, collapse = ", ")
  )
}


# 2. Paths

data_path <- file.path(
  "data",
  "evaluation_anonymized.csv"
)

v1_fit_path <- file.path(
  "results",
  "model_v1_cross_classified",
  "mcmc_fit_continuous.rds"
)

results_dir <- file.path(
  "results",
  "model_v1_validation"
)

figures_dir <- file.path(
  "figures",
  "validation"
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

input_files <- c(
  data_path,
  v1_fit_path
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


# 3. Load data and fitted model

reviews <- read.csv(
  data_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

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

reviews$overall_grade <- as.numeric(
  as.character(
    reviews$overall_grade
  )
)

v1_fit <- readRDS(
  v1_fit_path
)

v1_draws <- as.matrix(
  v1_fit$samples
)

proposal_review_counts <- table(
  reviews$proposal_id
)

if (any(proposal_review_counts != 2)) {
  stop(
    "Each proposal must have exactly two reviews for the current PPC disagreement calculation."
  )
}

message(
  "Loaded V1 posterior. Baseline model is not being refitted."
)

message(
  "Posterior draws available: ",
  nrow(v1_draws)
)


# 4. Check required parameters

required_parameters <- c(
  "tau_proposal",
  "tau_assessor",
  "sigma"
)

missing_parameters <- setdiff(
  required_parameters,
  colnames(v1_draws)
)

if (length(missing_parameters) > 0) {
  stop(
    "Missing posterior parameters: ",
    paste(missing_parameters, collapse = ", ")
  )
}


# 5. Computational diagnostics

diagnostic_summary <- tibble::tibble(
  parameter = rownames(v1_fit$summary),
  psrf = v1_fit$summary[, "psrf"],
  effective_sample_size = v1_fit$summary[, "SSeff"]
)

write.csv(
  diagnostic_summary,
  file.path(
    results_dir,
    "computational_diagnostics.csv"
  ),
  row.names = FALSE
)

scale_diagnostics <- diagnostic_summary |>
  dplyr::filter(
    parameter %in% required_parameters
  )

write.csv(
  scale_diagnostics,
  file.path(
    results_dir,
    "scale_parameter_diagnostics.csv"
  ),
  row.names = FALSE
)

print(
  scale_diagnostics
)


# 6. Trace plots

trace_matrix <- as.matrix(
  v1_fit$samples
)

n_chains <- 4L

if (nrow(trace_matrix) %% n_chains != 0) {
  stop(
    "Posterior draws cannot be divided evenly into four chains."
  )
}

draws_per_chain <- nrow(
  trace_matrix
) / n_chains

trace_data <- dplyr::bind_rows(
  lapply(
    seq_len(n_chains),
    function(chain_id) {
      
      start_row <- (
        (chain_id - 1) *
          draws_per_chain +
          1
      )
      
      end_row <- (
        chain_id *
          draws_per_chain
      )
      
      chain_matrix <- trace_matrix[
        start_row:end_row,
        required_parameters,
        drop = FALSE
      ]
      
      chain_table <- tibble::as_tibble(
        chain_matrix
      )
      
      chain_table$iteration <- seq_len(
        nrow(chain_table)
      )
      
      chain_table$chain <- factor(
        paste0(
          "Chain ",
          chain_id
        )
      )
      
      chain_table
    }
  )
)

trace_window <- 5000L
plot_every <- 5L

trace_data_plot <- trace_data |>
  dplyr::filter(
    iteration > draws_per_chain - trace_window,
    iteration %% plot_every == 0
  )

trace_long <- trace_data_plot |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(
      required_parameters
    ),
    names_to = "parameter",
    values_to = "value"
  ) |>
  dplyr::mutate(
    parameter = dplyr::recode(
      parameter,
      tau_proposal = "tau[theta]",
      tau_assessor = "tau[b]",
      sigma = "sigma"
    )
  )

trace_plot <- ggplot2::ggplot(
  trace_long,
  ggplot2::aes(
    x = iteration,
    y = value,
    group = chain,
    colour = chain
  )
) +
  ggplot2::geom_line(
    alpha = 0.65,
    linewidth = 0.35
  ) +
  ggplot2::facet_wrap(
    ~ parameter,
    ncol = 1,
    scales = "free_y",
    labeller = ggplot2::label_parsed
  ) +
  ggplot2::labs(
    title = "V1 MCMC trace plots",
    subtitle = "Four chains; final 5,000 stored iterations shown for readability",
    x = "Iteration within chain",
    y = NULL,
    colour = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "v1_trace_plots.png"
  ),
  plot = trace_plot,
  width = 9,
  height = 8,
  dpi = 300
)


# 7. Posterior variance decomposition

tau_proposal <- v1_draws[
  ,
  "tau_proposal"
]

tau_assessor <- v1_draws[
  ,
  "tau_assessor"
]

sigma <- v1_draws[
  ,
  "sigma"
]

proposal_variance <- tau_proposal^2
reviewer_variance <- tau_assessor^2
residual_variance <- sigma^2

total_variance <- (
  proposal_variance +
    reviewer_variance +
    residual_variance
)

variance_share_draws <- tibble::tibble(
  proposal = (
    proposal_variance /
      total_variance
  ),
  reviewer = (
    reviewer_variance /
      total_variance
  ),
  residual = (
    residual_variance /
      total_variance
  )
)

variance_share_long <- variance_share_draws |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "component",
    values_to = "variance_share"
  ) |>
  dplyr::mutate(
    component = dplyr::recode(
      component,
      proposal = "Proposal",
      reviewer = "Reviewer",
      residual = "Residual"
    ),
    component = factor(
      component,
      levels = c(
        "Proposal",
        "Reviewer",
        "Residual"
      )
    )
  )

variance_share_summary <- variance_share_long |>
  dplyr::group_by(
    component
  ) |>
  dplyr::summarise(
    posterior_mean = mean(
      variance_share
    ),
    posterior_median = median(
      variance_share
    ),
    q025 = quantile(
      variance_share,
      0.025
    ),
    q975 = quantile(
      variance_share,
      0.975
    ),
    .groups = "drop"
  )

write.csv(
  variance_share_summary,
  file.path(
    results_dir,
    "variance_share_summary.csv"
  ),
  row.names = FALSE
)

print(
  variance_share_summary
)

variance_share_plot <- ggplot2::ggplot(
  variance_share_summary,
  ggplot2::aes(
    x = component,
    y = posterior_median
  )
) +
  ggplot2::geom_point(
    size = 3
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = q025,
      ymax = q975
    ),
    width = 0.15
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 1)
  ) +
  ggplot2::labs(
    title = "Posterior variance shares under V1",
    subtitle = "Points show posterior medians; intervals show 95% credible intervals",
    x = NULL,
    y = "Share of total model variance"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "v1_variance_shares.png"
  ),
  plot = variance_share_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# 8. Prepare posterior predictive simulation

proposal_map <- reviews |>
  dplyr::distinct(
    proposal_id
  ) |>
  dplyr::mutate(
    proposal_index = dplyr::row_number()
  )

reviewer_map <- reviews |>
  dplyr::distinct(
    reviewer_id
  ) |>
  dplyr::mutate(
    reviewer_index = dplyr::row_number()
  )

proposal_index <- match(
  reviews$proposal_id,
  proposal_map$proposal_id
)

reviewer_index <- match(
  reviews$reviewer_id,
  reviewer_map$reviewer_id
)

proposal_columns <- paste0(
  "proposal_intercept[",
  proposal_map$proposal_index,
  "]"
)

reviewer_columns <- paste0(
  "assessor_intercept[",
  reviewer_map$reviewer_index,
  "]"
)

if (
  !all(
    proposal_columns %in%
    colnames(v1_draws)
  )
) {
  stop(
    "Proposal-effect columns were not found."
  )
}

if (
  !all(
    reviewer_columns %in%
    colnames(v1_draws)
  )
) {
  stop(
    "Reviewer-effect columns were not found."
  )
}


# 9. Select posterior draws for PPC

set.seed(
  20260811
)

n_ppc_draws <- min(
  5000,
  nrow(v1_draws)
)

ppc_indices <- sample(
  seq_len(
    nrow(v1_draws)
  ),
  size = n_ppc_draws,
  replace = FALSE
)

ppc_draws <- v1_draws[
  ppc_indices,
  ,
  drop = FALSE
]

overall_mean <- mean(
  reviews$overall_grade
)

proposal_effects_ppc <- ppc_draws[
  ,
  proposal_columns,
  drop = FALSE
]

reviewer_effects_ppc <- ppc_draws[
  ,
  reviewer_columns,
  drop = FALSE
]


# 10. Generate replicated grades

mu_rep <- matrix(
  NA_real_,
  nrow = n_ppc_draws,
  ncol = nrow(reviews)
)

for (r in seq_len(nrow(reviews))) {
  
  mu_rep[, r] <- (
    overall_mean +
      proposal_effects_ppc[
        ,
        proposal_index[r]
      ] +
      reviewer_effects_ppc[
        ,
        reviewer_index[r]
      ]
  )
}

y_rep <- matrix(
  stats::rnorm(
    n_ppc_draws * nrow(reviews),
    mean = as.vector(mu_rep),
    sd = rep(
      ppc_draws[, "sigma"],
      times = nrow(reviews)
    )
  ),
  nrow = n_ppc_draws,
  ncol = nrow(reviews)
)


# 11. Posterior predictive summaries

observed_mean <- mean(
  reviews$overall_grade
)

observed_sd <- stats::sd(
  reviews$overall_grade
)

observed_disagreement <- reviews |>
  dplyr::group_by(
    proposal_id
  ) |>
  dplyr::summarise(
    absolute_difference =
      abs(
        overall_grade[1] -
          overall_grade[2]
      ),
    .groups = "drop"
  ) |>
  dplyr::summarise(
    mean_absolute_difference =
      mean(
        absolute_difference
      )
  ) |>
  dplyr::pull(
    mean_absolute_difference
  )

replicated_mean <- rowMeans(
  y_rep
)

replicated_sd <- apply(
  y_rep,
  1,
  stats::sd
)

proposal_rows <- split(
  seq_len(
    nrow(reviews)
  ),
  reviews$proposal_id
)

replicated_disagreement <- vapply(
  seq_len(
    n_ppc_draws
  ),
  function(s) {
    
    proposal_differences <- vapply(
      proposal_rows,
      function(rows) {
        abs(
          y_rep[s, rows[1]] -
            y_rep[s, rows[2]]
        )
      },
      numeric(1)
    )
    
    mean(
      proposal_differences
    )
  },
  numeric(1)
)

replicated_outside_range <- rowMeans(
  y_rep < 1 |
    y_rep > 5
)

summarise_replicated <- function(x) {
  c(
    posterior_mean = mean(x),
    posterior_median = median(x),
    q025 = unname(
      quantile(
        x,
        0.025
      )
    ),
    q975 = unname(
      quantile(
        x,
        0.975
      )
    )
  )
}

mean_summary <- summarise_replicated(
  replicated_mean
)

sd_summary <- summarise_replicated(
  replicated_sd
)

disagreement_summary <- summarise_replicated(
  replicated_disagreement
)

outside_range_summary <- summarise_replicated(
  replicated_outside_range
)

ppc_summary <- tibble::tibble(
  statistic = c(
    "Mean overall grade",
    "Standard deviation",
    "Mean within-proposal absolute disagreement",
    "Proportion outside 1-5"
  ),
  observed = c(
    observed_mean,
    observed_sd,
    observed_disagreement,
    0
  ),
  replicated_mean = c(
    mean_summary["posterior_mean"],
    sd_summary["posterior_mean"],
    disagreement_summary["posterior_mean"],
    outside_range_summary["posterior_mean"]
  ),
  replicated_median = c(
    mean_summary["posterior_median"],
    sd_summary["posterior_median"],
    disagreement_summary["posterior_median"],
    outside_range_summary["posterior_median"]
  ),
  replicated_q025 = c(
    mean_summary["q025"],
    sd_summary["q025"],
    disagreement_summary["q025"],
    outside_range_summary["q025"]
  ),
  replicated_q975 = c(
    mean_summary["q975"],
    sd_summary["q975"],
    disagreement_summary["q975"],
    outside_range_summary["q975"]
  )
)

write.csv(
  ppc_summary,
  file.path(
    results_dir,
    "posterior_predictive_summary.csv"
  ),
  row.names = FALSE
)

print(
  ppc_summary
)


# 12. Posterior predictive figure

ppc_plot_data <- dplyr::bind_rows(
  tibble::tibble(
    statistic = "Mean overall grade",
    replicated = replicated_mean,
    observed = observed_mean
  ),
  tibble::tibble(
    statistic = "Standard deviation",
    replicated = replicated_sd,
    observed = observed_sd
  ),
  tibble::tibble(
    statistic =
      "Mean within-proposal absolute disagreement",
    replicated = replicated_disagreement,
    observed = observed_disagreement
  ),
  tibble::tibble(
    statistic = "Proportion outside 1-5",
    replicated = replicated_outside_range,
    observed = 0
  )
)

ppc_plot <- ggplot2::ggplot(
  ppc_plot_data,
  ggplot2::aes(
    x = replicated
  )
) +
  ggplot2::geom_histogram(
    bins = 35
  ) +
  ggplot2::geom_vline(
    ggplot2::aes(
      xintercept = observed
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  ggplot2::facet_wrap(
    ~ statistic,
    scales = "free",
    ncol = 2
  ) +
  ggplot2::labs(
    title = "Posterior predictive checks for V1",
    subtitle = "Histograms show replicated statistics; dashed lines show observed values",
    x = NULL,
    y = "Number of posterior predictive replicates"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "v1_posterior_predictive_checks.png"
  ),
  plot = ppc_plot,
  width = 10,
  height = 7,
  dpi = 300
)


# 13. Save reproducibility information

capture.output(
  sessionInfo(),
  file = file.path(
    results_dir,
    "session_info.txt"
  )
)

message("")
message("V1 baseline validation finished.")
message("")
message(
  "Results: ",
  results_dir
)
message(
  "Figures: ",
  figures_dir
)