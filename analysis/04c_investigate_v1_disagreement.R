# Investigate reviewer disagreement under Model V1
#
# Uses the previously fitted baseline V1 posterior.
# No model refitting is performed.
#
# Run from the evaluation-data project root with:
# source("analysis/04c_investigate_v1_disagreement.R")


# 1. Packages

required_packages <- c(
  "dplyr",
  "ggplot2",
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
    )
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
  "model_v1_disagreement"
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


# 3. Load data and posterior

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

if (
  any(
    proposal_review_counts != 2
  )
) {
  stop(
    "Each proposal must have exactly two reviews."
  )
}

message(
  "Loaded V1 posterior. No model refitting is performed."
)

message(
  "Posterior draws available: ",
  nrow(v1_draws)
)


# 4. Map proposals and reviewers

proposal_map <- reviews |>
  dplyr::distinct(
    proposal_id
  ) |>
  dplyr::mutate(
    proposal_index =
      dplyr::row_number()
  )

reviewer_map <- reviews |>
  dplyr::distinct(
    reviewer_id
  ) |>
  dplyr::mutate(
    reviewer_index =
      dplyr::row_number()
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

required_columns <- c(
  proposal_columns,
  reviewer_columns,
  "sigma"
)

missing_columns <- setdiff(
  required_columns,
  colnames(v1_draws)
)

if (
  length(
    missing_columns
  ) > 0
) {
  stop(
    "Missing posterior columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}


# 5. Reproduce the posterior predictive simulation

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

mu_rep <- matrix(
  NA_real_,
  nrow = n_ppc_draws,
  ncol = nrow(reviews)
)

for (
  r in seq_len(
    nrow(reviews)
  )
) {
  
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
    n_ppc_draws *
      nrow(reviews),
    mean = as.vector(
      mu_rep
    ),
    sd = rep(
      ppc_draws[
        ,
        "sigma"
      ],
      times = nrow(reviews)
    )
  ),
  nrow = n_ppc_draws,
  ncol = nrow(reviews)
)


# 6. Observed proposal-level disagreement

proposal_rows <- split(
  seq_len(
    nrow(reviews)
  ),
  reviews$proposal_id
)

proposal_ids <- names(
  proposal_rows
)

observed_differences <- vapply(
  proposal_rows,
  function(rows) {
    
    abs(
      reviews$overall_grade[
        rows[1]
      ] -
        reviews$overall_grade[
          rows[2]
        ]
    )
  },
  numeric(1)
)

observed_mean_disagreement <- mean(
  observed_differences
)

message("")
message(
  "Observed mean absolute disagreement: ",
  round(
    observed_mean_disagreement,
    6
  )
)


# 7. Replicated proposal-level disagreement

replicated_differences <- vapply(
  proposal_rows,
  function(rows) {
    
    abs(
      y_rep[
        ,
        rows[1]
      ] -
        y_rep[
          ,
          rows[2]
        ]
    )
  },
  numeric(
    n_ppc_draws
  )
)

colnames(
  replicated_differences
) <- proposal_ids

replicated_mean_disagreement <- rowMeans(
  replicated_differences
)


# 8. Is the observed mean disagreement unusual?

ppc_upper_probability <- mean(
  replicated_mean_disagreement >=
    observed_mean_disagreement
)

ppc_lower_probability <- mean(
  replicated_mean_disagreement <=
    observed_mean_disagreement
)

ppc_two_sided <- min(
  1,
  2 *
    min(
      ppc_upper_probability,
      ppc_lower_probability
    )
)

observed_percentile <- mean(
  replicated_mean_disagreement <=
    observed_mean_disagreement
)

standardized_difference <- (
  observed_mean_disagreement -
    mean(
      replicated_mean_disagreement
    )
) /
  stats::sd(
    replicated_mean_disagreement
  )

mean_disagreement_check <- tibble::tibble(
  observed =
    observed_mean_disagreement,
  
  replicated_mean =
    mean(
      replicated_mean_disagreement
    ),
  
  replicated_median =
    median(
      replicated_mean_disagreement
    ),
  
  replicated_sd =
    stats::sd(
      replicated_mean_disagreement
    ),
  
  replicated_q025 =
    unname(
      quantile(
        replicated_mean_disagreement,
        0.025
      )
    ),
  
  replicated_q975 =
    unname(
      quantile(
        replicated_mean_disagreement,
        0.975
      )
    ),
  
  probability_rep_ge_observed =
    ppc_upper_probability,
  
  probability_rep_le_observed =
    ppc_lower_probability,
  
  two_sided_ppc_probability =
    ppc_two_sided,
  
  observed_percentile =
    observed_percentile,
  
  standardized_difference =
    standardized_difference
)

write.csv(
  mean_disagreement_check,
  file.path(
    results_dir,
    "mean_disagreement_check.csv"
  ),
  row.names = FALSE
)


# 9. Analytic expected disagreement under V1

folded_normal_mean <- function(
    mu,
    sd
) {
  
  abs_mu <- abs(
    mu
  )
  
  (
    sd *
      sqrt(
        2 / pi
      ) *
      exp(
        -(abs_mu^2) /
          (
            2 *
              sd^2
          )
      )
  ) +
    (
      abs_mu *
        (
          2 *
            stats::pnorm(
              abs_mu /
                sd
            ) -
            1
        )
    )
}

analytic_disagreement <- matrix(
  NA_real_,
  nrow = n_ppc_draws,
  ncol = length(
    proposal_rows
  )
)

colnames(
  analytic_disagreement
) <- proposal_ids

for (
  i in seq_along(
    proposal_rows
  )
) {
  
  rows <- proposal_rows[[i]]
  
  reviewer_1 <- reviewer_index[
    rows[1]
  ]
  
  reviewer_2 <- reviewer_index[
    rows[2]
  ]
  
  delta_b <- (
    reviewer_effects_ppc[
      ,
      reviewer_1
    ] -
      reviewer_effects_ppc[
        ,
        reviewer_2
      ]
  )
  
  disagreement_sd <- (
    sqrt(
      2
    ) *
      ppc_draws[
        ,
        "sigma"
      ]
  )
  
  analytic_disagreement[
    ,
    i
  ] <- folded_normal_mean(
    mu = delta_b,
    sd = disagreement_sd
  )
}

analytic_mean_disagreement <- rowMeans(
  analytic_disagreement
)

residual_only_mean_disagreement <- (
  2 *
    ppc_draws[
      ,
      "sigma"
    ] /
    sqrt(
      pi
    )
)

reviewer_increment <- (
  analytic_mean_disagreement -
    residual_only_mean_disagreement
)


# 10. Summarise the mechanism

summarise_distribution <- function(
    x,
    quantity
) {
  
  tibble::tibble(
    quantity = quantity,
    
    posterior_mean =
      mean(
        x
      ),
    
    posterior_median =
      median(
        x
      ),
    
    q025 =
      unname(
        quantile(
          x,
          0.025
        )
      ),
    
    q975 =
      unname(
        quantile(
          x,
          0.975
        )
      )
  )
}

mechanism_summary <- dplyr::bind_rows(
  summarise_distribution(
    replicated_mean_disagreement,
    "Simulated posterior predictive mean disagreement"
  ),
  
  summarise_distribution(
    analytic_mean_disagreement,
    "Analytic V1 expected mean disagreement"
  ),
  
  summarise_distribution(
    residual_only_mean_disagreement,
    "Expected mean disagreement with reviewer effects set equal"
  ),
  
  summarise_distribution(
    reviewer_increment,
    "Increase from fitted reviewer-effect differences"
  )
)

mechanism_summary$observed_reference <-
  observed_mean_disagreement

write.csv(
  mechanism_summary,
  file.path(
    results_dir,
    "disagreement_mechanism_summary.csv"
  ),
  row.names = FALSE
)

simulation_minus_analytic <- (
  mean(
    replicated_mean_disagreement
  ) -
    mean(
      analytic_mean_disagreement
    )
)


# 11. Check disagreement distribution and upper tail

observed_statistics <- c(
  mean =
    mean(
      observed_differences
    ),
  
  median =
    median(
      observed_differences
    ),
  
  q90 =
    unname(
      quantile(
        observed_differences,
        0.90
      )
    ),
  
  maximum =
    max(
      observed_differences
    ),
  
  proportion_ge_2 =
    mean(
      observed_differences >= 2
    ),
  
  proportion_ge_3 =
    mean(
      observed_differences >= 3
    )
)

replicated_statistics <- list(
  mean =
    rowMeans(
      replicated_differences
    ),
  
  median =
    apply(
      replicated_differences,
      1,
      stats::median
    ),
  
  q90 =
    apply(
      replicated_differences,
      1,
      stats::quantile,
      probs = 0.90
    ),
  
  maximum =
    apply(
      replicated_differences,
      1,
      max
    ),
  
  proportion_ge_2 =
    rowMeans(
      replicated_differences >= 2
    ),
  
  proportion_ge_3 =
    rowMeans(
      replicated_differences >= 3
    )
)

statistic_labels <- c(
  mean =
    "Mean absolute disagreement",
  
  median =
    "Median absolute disagreement",
  
  q90 =
    "90th percentile of disagreement",
  
  maximum =
    "Maximum disagreement",
  
  proportion_ge_2 =
    "Proportion with disagreement >= 2",
  
  proportion_ge_3 =
    "Proportion with disagreement >= 3"
)

tail_ppc_summary <- dplyr::bind_rows(
  lapply(
    names(
      replicated_statistics
    ),
    function(statistic) {
      
      replicated_values <-
        replicated_statistics[[statistic]]
      
      observed_value <-
        observed_statistics[
          statistic
        ]
      
      tibble::tibble(
        statistic =
          statistic_labels[
            statistic
          ],
        
        observed =
          unname(
            observed_value
          ),
        
        replicated_mean =
          mean(
            replicated_values
          ),
        
        replicated_median =
          median(
            replicated_values
          ),
        
        replicated_q025 =
          unname(
            quantile(
              replicated_values,
              0.025
            )
          ),
        
        replicated_q975 =
          unname(
            quantile(
              replicated_values,
              0.975
            )
          ),
        
        probability_rep_ge_observed =
          mean(
            replicated_values >=
              observed_value
          )
      )
    }
  )
)

write.csv(
  tail_ppc_summary,
  file.path(
    results_dir,
    "disagreement_distribution_ppc.csv"
  ),
  row.names = FALSE
)


# 12. Repeat disagreement PPC on the observed 1-5 score grid

# The baseline V1 model generates continuous Gaussian scores,
# while the observed CRS grades are integers from 1 to 5.
# For this diagnostic only, map replicated scores back onto
# the observed score grid by rounding and clipping to 1-5.

y_rep_discrete <- round(
  y_rep
)

y_rep_discrete[
  y_rep_discrete < 1
] <- 1

y_rep_discrete[
  y_rep_discrete > 5
] <- 5

replicated_differences_discrete <- vapply(
  proposal_rows,
  function(rows) {
    
    abs(
      y_rep_discrete[
        ,
        rows[1]
      ] -
        y_rep_discrete[
          ,
          rows[2]
        ]
    )
  },
  numeric(
    n_ppc_draws
  )
)

colnames(
  replicated_differences_discrete
) <- proposal_ids

replicated_discrete_statistics <- list(
  mean =
    rowMeans(
      replicated_differences_discrete
    ),
  
  median =
    apply(
      replicated_differences_discrete,
      1,
      stats::median
    ),
  
  q90 =
    apply(
      replicated_differences_discrete,
      1,
      stats::quantile,
      probs = 0.90
    ),
  
  maximum =
    apply(
      replicated_differences_discrete,
      1,
      max
    ),
  
  proportion_ge_2 =
    rowMeans(
      replicated_differences_discrete >= 2
    ),
  
  proportion_ge_3 =
    rowMeans(
      replicated_differences_discrete >= 3
    )
)

discrete_ppc_summary <- dplyr::bind_rows(
  lapply(
    names(
      replicated_discrete_statistics
    ),
    function(statistic) {
      
      replicated_values <-
        replicated_discrete_statistics[[statistic]]
      
      observed_value <-
        observed_statistics[
          statistic
        ]
      
      tibble::tibble(
        statistic =
          statistic_labels[
            statistic
          ],
        
        observed =
          unname(
            observed_value
          ),
        
        replicated_mean =
          mean(
            replicated_values
          ),
        
        replicated_median =
          median(
            replicated_values
          ),
        
        replicated_q025 =
          unname(
            quantile(
              replicated_values,
              0.025
            )
          ),
        
        replicated_q975 =
          unname(
            quantile(
              replicated_values,
              0.975
            )
          ),
        
        probability_rep_ge_observed =
          mean(
            replicated_values >=
              observed_value
          )
      )
    }
  )
)

write.csv(
  discrete_ppc_summary,
  file.path(
    results_dir,
    "disagreement_distribution_discrete_ppc.csv"
  ),
  row.names = FALSE
)

observed_disagreement_frequencies <-
  as.data.frame(
    table(
      observed_differences
    )
  )

names(
  observed_disagreement_frequencies
) <- c(
  "absolute_disagreement",
  "number_of_proposals"
)

write.csv(
  observed_disagreement_frequencies,
  file.path(
    results_dir,
    "observed_disagreement_frequencies.csv"
  ),
  row.names = FALSE
)


# 13. Proposal-level disagreement

proposal_disagreement <- dplyr::bind_rows(
  lapply(
    seq_along(
      proposal_rows
    ),
    function(i) {
      
      proposal <- proposal_ids[
        i
      ]
      
      rows <- proposal_rows[[i]]
      
      replicated_values <-
        replicated_differences[
          ,
          i
        ]
      
      analytic_values <-
        analytic_disagreement[
          ,
          i
        ]
      
      tibble::tibble(
        proposal_id =
          proposal,
        
        reviewer_1 =
          reviews$reviewer_id[
            rows[1]
          ],
        
        reviewer_2 =
          reviews$reviewer_id[
            rows[2]
          ],
        
        grade_1 =
          reviews$overall_grade[
            rows[1]
          ],
        
        grade_2 =
          reviews$overall_grade[
            rows[2]
          ],
        
        observed_abs_disagreement =
          observed_differences[
            proposal
          ],
        
        predictive_mean =
          mean(
            replicated_values
          ),
        
        predictive_median =
          median(
            replicated_values
          ),
        
        predictive_q025 =
          unname(
            quantile(
              replicated_values,
              0.025
            )
          ),
        
        predictive_q975 =
          unname(
            quantile(
              replicated_values,
              0.975
            )
          ),
        
        analytic_expected_disagreement =
          mean(
            analytic_values
          ),
        
        probability_rep_ge_observed =
          mean(
            replicated_values >=
              observed_differences[
                proposal
              ]
          )
      )
    }
  )
) |>
  dplyr::arrange(
    dplyr::desc(
      observed_abs_disagreement
    ),
    proposal_id
  )

write.csv(
  proposal_disagreement,
  file.path(
    results_dir,
    "proposal_disagreement_summary.csv"
  ),
  row.names = FALSE
)


# 14. Figure for disagreement PPC

ppc_plot_data <- dplyr::bind_rows(
  lapply(
    names(replicated_statistics),
    function(stat_name) {
      
      values <- replicated_statistics[[stat_name]]
      
      data.frame(
        statistic =
          unname(
            statistic_labels[[stat_name]]
          ),
        
        replicated_value =
          as.numeric(
            values
          )
      )
    }
  )
)

observed_plot_data <- data.frame(
  statistic =
    unname(
      statistic_labels[
        names(replicated_statistics)
      ]
    ),
  
  observed_value =
    as.numeric(
      observed_statistics[
        names(replicated_statistics)
      ]
    )
)

disagreement_ppc_plot <-
  ggplot2::ggplot(
    ppc_plot_data,
    ggplot2::aes(
      x = replicated_value
    )
  ) +
  ggplot2::geom_histogram(
    bins = 35
  ) +
  ggplot2::geom_vline(
    data = observed_plot_data,
    ggplot2::aes(
      xintercept = observed_value
    ),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  ggplot2::facet_wrap(
    ~ statistic,
    scales = "free",
    ncol = 2
  ) +
  ggplot2::labs(
    title =
      "Posterior predictive checks of reviewer disagreement",
    
    subtitle =
      "Histograms show replicated statistics; dashed lines show observed values",
    
    x = NULL,
    
    y =
      "Number of posterior predictive replicates"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "v1_disagreement_ppc.png"
  ),
  plot = disagreement_ppc_plot,
  width = 10,
  height = 9,
  dpi = 300
)


# 15. Print important results

message("")
message(
  "Mean disagreement check:"
)

print(
  mean_disagreement_check
)

message("")
message(
  "Mechanism summary:"
)

print(
  mechanism_summary
)

message("")
message(
  "Simulation mean minus analytic expectation: ",
  round(
    simulation_minus_analytic,
    6
  )
)

message("")
message(
  "Disagreement distribution PPC:"
)

print(
  tail_ppc_summary
)

message("")
message(
  "Disagreement PPC after mapping replicated scores to the 1-5 grid:"
)

print(
  discrete_ppc_summary
)

message("")
message(
  "Observed disagreement frequencies:"
)

print(
  observed_disagreement_frequencies
)

message("")
message(
  "Proposals with largest observed disagreement:"
)

print(
  proposal_disagreement |>
    dplyr::select(
      proposal_id,
      reviewer_1,
      reviewer_2,
      grade_1,
      grade_2,
      observed_abs_disagreement,
      predictive_mean,
      probability_rep_ge_observed
    ) |>
    head(
      10
    )
)

message("")
message(
  "Results saved to: ",
  results_dir
)

message(
  "Figure saved to: ",
  file.path(
    figures_dir,
    "v1_disagreement_ppc.png"
  )
)