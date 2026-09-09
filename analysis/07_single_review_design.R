# Evaluate all hypothetical single additional reviews under V1.
#
# The analysis uses posterior draws of the V1 scale parameters and evaluates
# A-, D- and E-optimal reductions in proposal-contrast uncertainty.
#
# No Bayesian model is refitted.
#
# Run from the project root with:
# source("analysis/07_single_review_design.R")


# Packages

required_packages <- c(
  "dplyr",
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


# Paths

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
  "review_design"
)

dir.create(
  results_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(data_path)) {
  stop(
    "Data file not found: ",
    data_path
  )
}

if (!file.exists(v1_fit_path)) {
  stop(
    "V1 fit not found: ",
    v1_fit_path
  )
}


# Load observed assignment structure

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

if (
  any(
    duplicated(
      reviews[, c(
        "proposal_id",
        "reviewer_id"
      )]
    )
  )
) {
  stop(
    "At least one reviewer-proposal pair occurs more than once."
  )
}


# Define proposal and reviewer order

proposal_ids <- sort(
  unique(
    reviews$proposal_id
  )
)

reviewer_ids <- sort(
  unique(
    reviews$reviewer_id
  )
)

n_proposals <- length(
  proposal_ids
)

n_reviewers <- length(
  reviewer_ids
)

n_vertices <-
  n_proposals +
  n_reviewers

if (n_proposals != 42) {
  stop(
    "Expected 42 proposals, found ",
    n_proposals,
    "."
  )
}

if (n_reviewers != 9) {
  stop(
    "Expected 9 reviewers, found ",
    n_reviewers,
    "."
  )
}

message(
  "Proposals: ",
  n_proposals
)

message(
  "Reviewers: ",
  n_reviewers
)

message(
  "Observed reviews: ",
  nrow(reviews)
)


# Build the observed incidence matrix

X <- matrix(
  0,
  nrow = nrow(reviews),
  ncol = n_vertices
)

for (r in seq_len(nrow(reviews))) {
  
  proposal_position <- match(
    reviews$proposal_id[r],
    proposal_ids
  )
  
  reviewer_position <-
    n_proposals +
    match(
      reviews$reviewer_id[r],
      reviewer_ids
    )
  
  X[r, proposal_position] <- 1
  
  X[r, reviewer_position] <- -1
}

L_G <- crossprod(
  X
)


# Verify Laplacian structure

if (
  max(
    abs(
      rowSums(L_G)
    )
  ) > 1e-10
) {
  stop(
    "Constructed Laplacian does not have zero row sums."
  )
}


# Construct all currently unobserved candidate edges

all_pairs <- expand.grid(
  proposal_id = proposal_ids,
  reviewer_id = reviewer_ids,
  stringsAsFactors = FALSE
)

observed_pairs <- reviews |>
  dplyr::distinct(
    proposal_id,
    reviewer_id
  )

candidate_edges <- all_pairs |>
  dplyr::anti_join(
    observed_pairs,
    by = c(
      "proposal_id",
      "reviewer_id"
    )
  ) |>
  dplyr::arrange(
    proposal_id,
    reviewer_id
  ) |>
  dplyr::mutate(
    candidate_id =
      dplyr::row_number()
  )

if (nrow(candidate_edges) != 294) {
  stop(
    "Expected 294 unobserved candidate edges, found ",
    nrow(candidate_edges),
    "."
  )
}

message(
  "Candidate additional reviews: ",
  nrow(candidate_edges)
)


# Build incidence vectors for candidate edges

candidate_X <- matrix(
  0,
  nrow = nrow(candidate_edges),
  ncol = n_vertices
)

for (e in seq_len(nrow(candidate_edges))) {
  
  proposal_position <- match(
    candidate_edges$proposal_id[e],
    proposal_ids
  )
  
  reviewer_position <-
    n_proposals +
    match(
      candidate_edges$reviewer_id[e],
      reviewer_ids
    )
  
  candidate_X[e, proposal_position] <- 1
  
  candidate_X[e, reviewer_position] <- -1
}


# Load V1 posterior draws

v1_fit <- readRDS(
  v1_fit_path
)

v1_draws <- as.matrix(
  v1_fit$samples
)

required_posterior_columns <- c(
  "tau_proposal",
  "tau_assessor",
  "sigma"
)

missing_posterior_columns <- setdiff(
  required_posterior_columns,
  colnames(v1_draws)
)

if (length(missing_posterior_columns) > 0) {
  stop(
    "Missing posterior columns: ",
    paste(
      missing_posterior_columns,
      collapse = ", "
    )
  )
}

message(
  "Posterior draws available: ",
  nrow(v1_draws)
)


# Select posterior draws for the review-design calculation
#
# Use 5,000 approximately evenly spaced posterior draws.
# A previous 1,000-draw run was used to assess Monte Carlo stability.
# Its derived outputs are stored under results/review_design_1000/ for comparison.

n_design_draws <- min(
  5000L,
  nrow(v1_draws)
)

draw_indices <- unique(
  round(
    seq(
      1,
      nrow(v1_draws),
      length.out = n_design_draws
    )
  )
)

design_draws <- v1_draws[
  draw_indices,
  required_posterior_columns,
  drop = FALSE
]

n_design_draws <- nrow(
  design_draws
)

message(
  "Posterior draws used for review design: ",
  n_design_draws
)


# Construct an orthonormal basis for proposal contrasts
#
# Columns of C span the subspace orthogonal to the all-ones vector.

C <- qr.Q(
  qr(
    stats::contr.helmert(
      n_proposals
    )
  )
)

if (
  nrow(C) != n_proposals ||
  ncol(C) != n_proposals - 1
) {
  stop(
    "Contrast basis has unexpected dimensions."
  )
}

if (
  max(
    abs(
      colSums(C)
    )
  ) > 1e-10
) {
  stop(
    "Contrast basis is not orthogonal to the common-shift direction."
  )
}


# Storage for posterior-draw utility reductions

n_candidates <- nrow(
  candidate_edges
)

delta_A <- matrix(
  NA_real_,
  nrow = n_design_draws,
  ncol = n_candidates
)

delta_D <- matrix(
  NA_real_,
  nrow = n_design_draws,
  ncol = n_candidates
)

delta_E <- matrix(
  NA_real_,
  nrow = n_design_draws,
  ncol = n_candidates
)


# Evaluate candidate edges over posterior draws

for (s in seq_len(n_design_draws)) {
  
  tau_theta <- design_draws[
    s,
    "tau_proposal"
  ]
  
  tau_b <- design_draws[
    s,
    "tau_assessor"
  ]
  
  sigma <- design_draws[
    s,
    "sigma"
  ]
  
  # Prior precision P_0^(s)
  
  P0 <- diag(
    c(
      rep(
        1 / tau_theta^2,
        n_proposals
      ),
      rep(
        1 / tau_b^2,
        n_reviewers
      )
    )
  )
  
  # Conditional posterior precision and covariance
  
  Q <-
    P0 +
    L_G / sigma^2
  
  V <- solve(
    Q
  )
  
  V_theta <- V[
    seq_len(n_proposals),
    seq_len(n_proposals),
    drop = FALSE
  ]
  
  # Proposal covariance on the contrast subspace
  
  S <-
    crossprod(
      C,
      V_theta %*% C
    )
  
  S <- (
    S +
      t(S)
  ) / 2
  
  # Baseline E loss
  
  baseline_E <- max(
    eigen(
      S,
      symmetric = TRUE,
      only.values = TRUE
    )$values
  )
  
  # Inverse needed for efficient D-optimality update
  
  S_inverse <- solve(
    S
  )
  
  for (e in seq_len(n_candidates)) {
    
    x_e <- candidate_X[
      e,
      ,
      drop = FALSE
    ]
    
    x_e <- as.numeric(
      x_e
    )
    
    # V x_e
    
    u <- as.numeric(
      V %*% x_e
    )
    
    denominator <-
      sigma^2 +
      sum(
        x_e * u
      )
    
    if (
      !is.finite(denominator) ||
      denominator <= 0
    ) {
      stop(
        "Invalid Sherman-Morrison denominator."
      )
    }
    
    # Proposal part of V x_e
    
    u_theta <- u[
      seq_len(n_proposals)
    ]
    
    # Express proposal update on contrast subspace
    
    z <- as.numeric(
      crossprod(
        C,
        u_theta
      )
    )
    
    # A-optimality
    #
    # Average pairwise proposal-contrast variance:
    #
    # L_A = 2/(n-1) * tr(C' V_theta C)
    #
    # Hence its reduction after the rank-one update is:
    
    delta_A[s, e] <-
      (
        2 /
          (n_proposals - 1)
      ) *
      sum(
        z^2
      ) /
      denominator
    
    # D-optimality
    #
    # By the matrix determinant lemma,
    #
    # det(S_e)
    # =
    # det(S) *
    # (1 - z' S^{-1} z / denominator)
    
    determinant_fraction <-
      as.numeric(
        crossprod(
          z,
          S_inverse %*% z
        )
      ) /
      denominator
    
    # Numerical protection only
    
    determinant_fraction <- min(
      max(
        determinant_fraction,
        0
      ),
      1 - 1e-12
    )
    
    delta_D[s, e] <-
      -log1p(
        -determinant_fraction
      )
    
    # E-optimality
    #
    # Largest posterior variance direction on the contrast subspace.
    
    S_e <-
      S -
      tcrossprod(
        z
      ) /
      denominator
    
    S_e <- (
      S_e +
        t(S_e)
    ) / 2
    
    updated_E <- max(
      eigen(
        S_e,
        symmetric = TRUE,
        only.values = TRUE
      )$values
    )
    
    delta_E[s, e] <-
      baseline_E -
      updated_E
  }
  
  if (
    s == 1 ||
    s %% 50 == 0 ||
    s == n_design_draws
  ) {
    message(
      "Completed posterior draw ",
      s,
      " of ",
      n_design_draws
    )
  }
}


# Summarise posterior-averaged utility

summarise_criterion <- function(
    utility_matrix,
    prefix
) {
  
  tibble::tibble(
    mean = colMeans(
      utility_matrix
    ),
    median = apply(
      utility_matrix,
      2,
      median
    ),
    q025 = apply(
      utility_matrix,
      2,
      stats::quantile,
      probs = 0.025
    ),
    q975 = apply(
      utility_matrix,
      2,
      stats::quantile,
      probs = 0.975
    )
  ) |>
    stats::setNames(
      paste0(
        prefix,
        c(
          "_mean",
          "_median",
          "_q025",
          "_q975"
        )
      )
    )
}

A_summary <- summarise_criterion(
  delta_A,
  "A"
)

D_summary <- summarise_criterion(
  delta_D,
  "D"
)

E_summary <- summarise_criterion(
  delta_E,
  "E"
)

candidate_results <- dplyr::bind_cols(
  candidate_edges,
  A_summary,
  D_summary,
  E_summary
) |>
  dplyr::mutate(
    A_rank = rank(
      -A_mean,
      ties.method = "min"
    ),
    D_rank = rank(
      -D_mean,
      ties.method = "min"
    ),
    E_rank = rank(
      -E_mean,
      ties.method = "min"
    )
  ) |>
  dplyr::arrange(
    A_rank,
    D_rank,
    E_rank
  )


# Save complete candidate results

write.csv(
  candidate_results,
  file.path(
    results_dir,
    "single_review_candidate_results.csv"
  ),
  row.names = FALSE
)


# Save top candidates under each criterion

top_A <- candidate_results |>
  dplyr::arrange(
    A_rank
  ) |>
  dplyr::slice_head(
    n = 15
  )

top_D <- candidate_results |>
  dplyr::arrange(
    D_rank
  ) |>
  dplyr::slice_head(
    n = 15
  )

top_E <- candidate_results |>
  dplyr::arrange(
    E_rank
  ) |>
  dplyr::slice_head(
    n = 15
  )

write.csv(
  top_A,
  file.path(
    results_dir,
    "top_candidates_A.csv"
  ),
  row.names = FALSE
)

write.csv(
  top_D,
  file.path(
    results_dir,
    "top_candidates_D.csv"
  ),
  row.names = FALSE
)

write.csv(
  top_E,
  file.path(
    results_dir,
    "top_candidates_E.csv"
  ),
  row.names = FALSE
)


# Compare rankings across criteria

criterion_rank_correlations <- data.frame(
  comparison = c(
    "A vs D",
    "A vs E",
    "D vs E"
  ),
  spearman = c(
    cor(
      candidate_results$A_rank,
      candidate_results$D_rank,
      method = "spearman"
    ),
    cor(
      candidate_results$A_rank,
      candidate_results$E_rank,
      method = "spearman"
    ),
    cor(
      candidate_results$D_rank,
      candidate_results$E_rank,
      method = "spearman"
    )
  )
)

write.csv(
  criterion_rank_correlations,
  file.path(
    results_dir,
    "criterion_rank_correlations.csv"
  ),
  row.names = FALSE
)


# Posterior-median plug-in sensitivity analysis

tau_theta_median <- median(
  v1_draws[, "tau_proposal"]
)

tau_b_median <- median(
  v1_draws[, "tau_assessor"]
)

sigma_median <- median(
  v1_draws[, "sigma"]
)

message(
  "\nPosterior median scales:"
)

print(
  c(
    tau_theta = tau_theta_median,
    tau_b = tau_b_median,
    sigma = sigma_median
  )
)

write.csv(
  data.frame(
    parameter = c(
      "tau_proposal",
      "tau_assessor",
      "sigma"
    ),
    posterior_median = c(
      tau_theta_median,
      tau_b_median,
      sigma_median
    )
  ),
  file.path(
    results_dir,
    "posterior_median_scales.csv"
  ),
  row.names = FALSE
)

# Construct P0, Q and V using posterior medians

P0_median <- diag(
  c(
    rep(
      1 / tau_theta_median^2,
      n_proposals
    ),
    rep(
      1 / tau_b_median^2,
      n_reviewers
    )
  )
)

Q_median <-
  P0_median +
  L_G / sigma_median^2

V_median <- solve(
  Q_median
)

V_theta_median <- V_median[
  seq_len(n_proposals),
  seq_len(n_proposals),
  drop = FALSE
]


# Covariance on proposal-contrast subspace

S_median <- crossprod(
  C,
  V_theta_median %*% C
)

S_median <- (
  S_median +
    t(S_median)
) / 2

S_inverse_median <- solve(
  S_median
)

baseline_E_median <- max(
  eigen(
    S_median,
    symmetric = TRUE,
    only.values = TRUE
  )$values
)


# Storage

plugin_A <- numeric(
  n_candidates
)

plugin_D <- numeric(
  n_candidates
)

plugin_E <- numeric(
  n_candidates
)


# Evaluate all candidate edges once at posterior medians

for (e in seq_len(n_candidates)) {
  
  x_e <- as.numeric(
    candidate_X[
      e,
      ,
      drop = FALSE
    ]
  )
  
  u <- as.numeric(
    V_median %*% x_e
  )
  
  denominator <-
    sigma_median^2 +
    sum(
      x_e * u
    )
  
  if (
    !is.finite(denominator) ||
    denominator <= 0
  ) {
    stop(
      "Invalid Sherman-Morrison denominator in plug-in analysis."
    )
  }
  
  u_theta <- u[
    seq_len(n_proposals)
  ]
  
  z <- as.numeric(
    crossprod(
      C,
      u_theta
    )
  )
  
  
  # A-optimality reduction
  
  plugin_A[e] <-
    (
      2 /
        (n_proposals - 1)
    ) *
    sum(
      z^2
    ) /
    denominator
  
  
  # D-optimality reduction
  
  determinant_fraction <-
    as.numeric(
      crossprod(
        z,
        S_inverse_median %*% z
      )
    ) /
    denominator
  
  determinant_fraction <- min(
    max(
      determinant_fraction,
      0
    ),
    1 - 1e-12
  )
  
  plugin_D[e] <-
    -log1p(
      -determinant_fraction
    )
  
  
  # E-optimality reduction
  
  S_e <-
    S_median -
    tcrossprod(
      z
    ) /
    denominator
  
  S_e <- (
    S_e +
      t(S_e)
  ) / 2
  
  updated_E <- max(
    eigen(
      S_e,
      symmetric = TRUE,
      only.values = TRUE
    )$values
  )
  
  plugin_E[e] <-
    baseline_E_median -
    updated_E
}


# Create plug-in ranking table

plugin_results <- candidate_edges |>
  dplyr::mutate(
    A_plugin = plugin_A,
    D_plugin = plugin_D,
    E_plugin = plugin_E,
    
    A_plugin_rank = rank(
      -A_plugin,
      ties.method = "min"
    ),
    
    D_plugin_rank = rank(
      -D_plugin,
      ties.method = "min"
    ),
    
    E_plugin_rank = rank(
      -E_plugin,
      ties.method = "min"
    )
  )


# Compare with posterior-averaged rankings

plugin_comparison <- candidate_results |>
  dplyr::select(
    candidate_id,
    proposal_id,
    reviewer_id,
    A_mean,
    D_mean,
    E_mean,
    A_rank,
    D_rank,
    E_rank
  ) |>
  dplyr::left_join(
    plugin_results,
    by = c(
      "candidate_id",
      "proposal_id",
      "reviewer_id"
    )
  )


# Spearman rank correlations

plugin_rank_correlations <- data.frame(
  criterion = c(
    "A",
    "D",
    "E"
  ),
  
  spearman = c(
    cor(
      plugin_comparison$A_rank,
      plugin_comparison$A_plugin_rank,
      method = "spearman"
    ),
    
    cor(
      plugin_comparison$D_rank,
      plugin_comparison$D_plugin_rank,
      method = "spearman"
    ),
    
    cor(
      plugin_comparison$E_rank,
      plugin_comparison$E_plugin_rank,
      method = "spearman"
    )
  )
)


# Save outputs

write.csv(
  plugin_comparison,
  file.path(
    results_dir,
    "posterior_average_vs_plugin.csv"
  ),
  row.names = FALSE
)

write.csv(
  plugin_rank_correlations,
  file.path(
    results_dir,
    "plugin_rank_correlations.csv"
  ),
  row.names = FALSE
)


# Print sensitivity results

message(
  "\nPosterior-average vs posterior-median plug-in rank correlations:"
)

print(
  plugin_rank_correlations
)

message(
  "\nTop five plug-in candidates under A-optimality:"
)

print(
  plugin_results |>
    dplyr::arrange(
      A_plugin_rank
    ) |>
    dplyr::select(
      proposal_id,
      reviewer_id,
      A_plugin,
      A_plugin_rank
    ) |>
    dplyr::slice_head(
      n = 5
    )
)

message(
  "\nTop five plug-in candidates under D-optimality:"
)

print(
  plugin_results |>
    dplyr::arrange(
      D_plugin_rank
    ) |>
    dplyr::select(
      proposal_id,
      reviewer_id,
      D_plugin,
      D_plugin_rank
    ) |>
    dplyr::slice_head(
      n = 5
    )
)

message(
  "\nTop five plug-in candidates under E-optimality:"
)

print(
  plugin_results |>
    dplyr::arrange(
      E_plugin_rank
    ) |>
    dplyr::select(
      proposal_id,
      reviewer_id,
      E_plugin,
      E_plugin_rank
    ) |>
    dplyr::slice_head(
      n = 5
    )
)

# Print main results

message(
  "\nPosterior-averaged review-design analysis finished."
)

message(
  "\nTop five candidates under A-optimality:"
)

print(
  top_A |>
    dplyr::select(
      proposal_id,
      reviewer_id,
      A_mean,
      A_rank,
      D_rank,
      E_rank
    ) |>
    dplyr::slice_head(
      n = 5
    )
)

message(
  "\nTop five candidates under D-optimality:"
)

print(
  top_D |>
    dplyr::select(
      proposal_id,
      reviewer_id,
      D_mean,
      A_rank,
      D_rank,
      E_rank
    ) |>
    dplyr::slice_head(
      n = 5
    )
)

message(
  "\nTop five candidates under E-optimality:"
)

print(
  top_E |>
    dplyr::select(
      proposal_id,
      reviewer_id,
      E_mean,
      A_rank,
      D_rank,
      E_rank
    ) |>
    dplyr::slice_head(
      n = 5
    )
)

message(
  "\nRank correlations:"
)

print(
  criterion_rank_correlations
)

message(
  "\nResults written to: ",
  results_dir
)