# Compare Model V0 (Heyard reference) with
# Model V1 (CRS cross-classified Gaussian model).
#
# This script does NOT fit either model.
# It only reads previously saved V0 and V1 outputs.
#
# Run from the evaluation-data project root with:
# source("analysis/03_compare_v0_v1.R")


# 1. Packages

required_packages <- c(
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
    paste(missing_packages, collapse = ", ")
  )
}


# 2. Paths

data_path <- "data/evaluation_anonymized.csv"

v0_ranking_path <- file.path(
  "results",
  "model_v0_heyard_reference",
  "ranking_continuous.csv"
)

v1_ranking_path <- file.path(
  "results",
  "model_v1_cross_classified",
  "ranking_continuous.csv"
)

v0_fit_path <- file.path(
  "results",
  "model_v0_heyard_reference",
  "mcmc_fit_continuous.rds"
)

v1_fit_path <- file.path(
  "results",
  "model_v1_cross_classified",
  "mcmc_fit_continuous.rds"
)

results_dir <- file.path(
  "results",
  "model_comparison"
)

figures_dir <- file.path(
  "figures",
  "model_comparison"
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
  v0_ranking_path,
  v1_ranking_path,
  v0_fit_path,
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


# 3. Read inputs

reviews <- read.csv(
  data_path,
  stringsAsFactors = FALSE
)

v0_ranking <- read.csv(
  v0_ranking_path,
  stringsAsFactors = FALSE
)

v1_ranking <- read.csv(
  v1_ranking_path,
  stringsAsFactors = FALSE
)

v0_fit <- readRDS(v0_fit_path)
v1_fit <- readRDS(v1_fit_path)

message(
  "Loaded V0 and V1 results. No model is being refitted."
)


# 4. Reconstruct proposal index mapping

proposal_map <- reviews |>
  dplyr::distinct(proposal_id) |>
  dplyr::mutate(
    num_proposal = dplyr::row_number()
  )

if (nrow(proposal_map) != 42) {
  stop("Expected 42 proposals.")
}


# 5. Main ranking comparison

v0_for_comparison <- v0_ranking |>
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
    raw_mean = avg_grade,
    raw_rank = rank,
    rank_pm_v0 = rank_pm,
    er_v0 = er,
    pcer_v0 = pcer
  )

v1_for_comparison <- v1_ranking |>
  dplyr::select(
    id_proposal,
    rank_pm,
    er,
    pcer
  ) |>
  dplyr::rename(
    rank_pm_v1 = rank_pm,
    er_v1 = er,
    pcer_v1 = pcer
  )

ranking_comparison <- v0_for_comparison |>
  dplyr::inner_join(
    v1_for_comparison,
    by = "id_proposal"
  ) |>
  dplyr::mutate(
    delta_rank_pm = rank_pm_v1 - rank_pm_v0,
    delta_er = er_v1 - er_v0,
    abs_delta_rank_pm = abs(delta_rank_pm),
    abs_delta_er = abs(delta_er),
    rank_pm_changed = delta_rank_pm != 0
  ) |>
  dplyr::arrange(
    dplyr::desc(abs_delta_rank_pm),
    dplyr::desc(abs_delta_er)
  )

if (nrow(ranking_comparison) != 42) {
  stop(
    "V0/V1 ranking merge did not produce 42 proposals."
  )
}

write.csv(
  ranking_comparison,
  file.path(
    results_dir,
    "ranking_comparison.csv"
  ),
  row.names = FALSE
)


# 6. Extract posterior draws

v0_draws <- as.matrix(v0_fit$samples)
v1_draws <- as.matrix(v1_fit$samples)

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
  !all(proposal_columns %in% colnames(v0_draws)) ||
  !all(proposal_columns %in% colnames(v1_draws))
) {
  stop(
    "Proposal-intercept columns could not be found in MCMC samples."
  )
}

if (
  !all(rank_columns %in% colnames(v0_draws)) ||
  !all(rank_columns %in% colnames(v1_draws))
) {
  stop(
    "Rank-theta columns could not be found in MCMC samples."
  )
}


# 7. Posterior rank uncertainty

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
    
    expected_rank_from_draws =
      colMeans(ranks),
    
    rank_median =
      apply(
        ranks,
        2,
        median
      ),
    
    rank_q05 =
      apply(
        ranks,
        2,
        quantile,
        probs = 0.05
      ),
    
    rank_q95 =
      apply(
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
    v0_draws,
    "V0 Heyard reference"
  ),
  summarise_rank_uncertainty(
    v1_draws,
    "V1 cross-classified"
  )
)

write.csv(
  rank_uncertainty,
  file.path(
    results_dir,
    "rank_uncertainty_comparison.csv"
  ),
  row.names = FALSE
)


# 8. Model comparison summary

v0_rank_widths <- rank_uncertainty |>
  dplyr::filter(
    model == "V0 Heyard reference"
  ) |>
  dplyr::pull(
    rank_interval_90_width
  )

v1_rank_widths <- rank_uncertainty |>
  dplyr::filter(
    model == "V1 cross-classified"
  ) |>
  dplyr::pull(
    rank_interval_90_width
  )

model_comparison_summary <- tibble::tibble(
  metric = c(
    "Spearman correlation of posterior-mean ranks",
    "Pearson correlation of expected ranks",
    "Number of proposals changing posterior-mean rank",
    "Mean absolute posterior-mean rank change",
    "Maximum absolute posterior-mean rank change",
    "Mean 90% rank interval width V0",
    "Mean 90% rank interval width V1",
    "Median 90% rank interval width V0",
    "Median 90% rank interval width V1"
  ),
  
  value = c(
    cor(
      ranking_comparison$rank_pm_v0,
      ranking_comparison$rank_pm_v1,
      method = "spearman"
    ),
    
    cor(
      ranking_comparison$er_v0,
      ranking_comparison$er_v1,
      method = "pearson"
    ),
    
    sum(
      ranking_comparison$rank_pm_changed
    ),
    
    mean(
      abs(
        ranking_comparison$delta_rank_pm
      )
    ),
    
    max(
      abs(
        ranking_comparison$delta_rank_pm
      )
    ),
    
    mean(v0_rank_widths),
    
    mean(v1_rank_widths),
    
    median(v0_rank_widths),
    
    median(v1_rank_widths)
  )
)

write.csv(
  model_comparison_summary,
  file.path(
    results_dir,
    "model_comparison_summary.csv"
  ),
  row.names = FALSE
)


# 9. Convergence comparison

convergence_row <- function(
    fit,
    model_name
) {
  
  psrf <- fit$summary[, "psrf"]
  ess <- fit$summary[, "SSeff"]
  
  tibble::tibble(
    model = model_name,
    
    max_psrf =
      max(
        psrf,
        na.rm = TRUE
      ),
    
    worst_psrf_parameter =
      rownames(fit$summary)[
        which.max(psrf)
      ],
    
    min_effective_sample_size =
      min(
        ess,
        na.rm = TRUE
      ),
    
    worst_ess_parameter =
      rownames(fit$summary)[
        which.min(ess)
      ]
  )
}

convergence_comparison <- dplyr::bind_rows(
  convergence_row(
    v0_fit,
    "V0 Heyard reference"
  ),
  convergence_row(
    v1_fit,
    "V1 cross-classified"
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


# 10. P023 versus P032 pairwise probability

p023_index <- proposal_map |>
  dplyr::filter(
    proposal_id == "P023"
  ) |>
  dplyr::pull(
    num_proposal
  )

p032_index <- proposal_map |>
  dplyr::filter(
    proposal_id == "P032"
  ) |>
  dplyr::pull(
    num_proposal
  )

if (
  length(p023_index) != 1 ||
  length(p032_index) != 1
) {
  stop(
    "Could not uniquely identify P023 and P032."
  )
}

p023_col <- paste0(
  "proposal_intercept[",
  p023_index,
  "]"
)

p032_col <- paste0(
  "proposal_intercept[",
  p032_index,
  "]"
)

probability_comparison <- tibble::tibble(
  model = c(
    "V0 Heyard reference",
    "V1 cross-classified"
  ),
  
  p_theta_P032_gt_P023 = c(
    mean(
      v0_draws[, p032_col] >
        v0_draws[, p023_col]
    ),
    
    mean(
      v1_draws[, p032_col] >
        v1_draws[, p023_col]
    )
  )
)

write.csv(
  probability_comparison,
  file.path(
    results_dir,
    "p023_p032_pairwise_probability.csv"
  ),
  row.names = FALSE
)


# 11. Figure: expected ranks V0 vs V1

er_plot <- ggplot2::ggplot(
  ranking_comparison,
  ggplot2::aes(
    x = er_v0,
    y = er_v1
  )
) +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_point() +
  ggrepel::geom_text_repel(
    data = ranking_comparison |>
      dplyr::filter(
        qualifies_original |
          abs_delta_er >=
          quantile(
            abs_delta_er,
            0.90
          )
      ),
    ggplot2::aes(
      label = id_proposal
    ),
    size = 3,
    max.overlaps = Inf
  ) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    title = "Expected ranks: V0 versus V1",
    subtitle =
      "Original qualifiers and proposals with the largest expected-rank changes are labelled",
    x = "V0 Heyard reference expected rank",
    y = "V1 cross-classified expected rank"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "expected_rank_v0_vs_v1.png"
  ),
  plot = er_plot,
  width = 8,
  height = 7,
  dpi = 300
)


# 12. Console summary

message("")
message("Model comparison finished.")
message("")

message("Overall V0-V1 comparison:")
print(model_comparison_summary)

message("")
message("Convergence:")
print(convergence_comparison)

message("")
message("P023 versus P032:")
print(probability_comparison)

message("")
message(
  "Results saved to: ",
  results_dir
)

message(
  "Figure saved to: ",
  figures_dir
)