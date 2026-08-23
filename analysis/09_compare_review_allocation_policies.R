# Compare limited additional-review allocation policies under V1.
#
# Practical extension of the single-review design analysis in
# analysis/07_single_review_design.R.
#
# Policies:
#   1. Random: choose an unobserved reviewer-proposal edge at random.
#   2. Disagreement-targeted: prioritize proposals with the largest original
#      absolute reviewer disagreement; within the currently highest-
#      disagreement group, choose the most A-informative available edge.
#   3. Sequential A-optimal: at each step choose the available edge with the
#      largest posterior-averaged reduction in average pairwise proposal-
#      contrast variance, update the covariance, and repeat.
#
# For comparability, each proposal can receive at most one additional review.
# The analysis considers budgets of 1,...,6 additional reviews.
#
# This script does not read the evaluation dataset and does not refit V1.
# It uses only derived outputs plus the saved V1 posterior fit.
#
# Run from the project root with:
# source("analysis/09_compare_review_allocation_policies.R")


# Packages

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


# Settings

max_budget <- 6L

# Use the same 5,000-draw resolution as the final single-review design analysis
# so that the first sequential step is directly comparable with Chapter 6.

n_design_draws_requested <- 5000L

# One random allocation path is paired with each selected posterior draw.
# This balances posterior-draw representation in the random benchmark.

n_random_paths <- 5000L

random_seed <- 20260823L


# Paths

candidate_results_path <- file.path(
  "results",
  "review_design",
  "single_review_candidate_results.csv"
)

disagreement_path <- file.path(
  "results",
  "model_v1_disagreement",
  "proposal_disagreement_summary.csv"
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

figures_dir <- file.path(
  "figures",
  "review_design"
)

report_figures_dir <- file.path(
  "report",
  "figures",
  "review_design"
)

for (directory in c(
  results_dir,
  figures_dir,
  report_figures_dir
)) {
  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

input_files <- c(
  candidate_results_path,
  disagreement_path,
  v1_fit_path
)

missing_files <- input_files[
  !file.exists(input_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing required input files: ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}


# Load derived single-review candidate results

candidate_edges <- read.csv(
  candidate_results_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_candidate_columns <- c(
  "proposal_id",
  "reviewer_id"
)

missing_candidate_columns <- setdiff(
  required_candidate_columns,
  names(candidate_edges)
)

if (length(missing_candidate_columns) > 0) {
  stop(
    "Missing candidate-result columns: ",
    paste(
      missing_candidate_columns,
      collapse = ", "
    )
  )
}

candidate_edges$proposal_id <- trimws(
  as.character(
    candidate_edges$proposal_id
  )
)

candidate_edges$reviewer_id <- trimws(
  as.character(
    candidate_edges$reviewer_id
  )
)

candidate_edges <- candidate_edges |>
  dplyr::distinct(
    proposal_id,
    reviewer_id
  ) |>
  dplyr::arrange(
    proposal_id,
    reviewer_id
  ) |>
  dplyr::mutate(
    candidate_id = dplyr::row_number()
  )

if (nrow(candidate_edges) != 294L) {
  stop(
    "Expected 294 unobserved candidate assignments, found ",
    nrow(candidate_edges),
    "."
  )
}

proposal_ids <- sort(
  unique(
    candidate_edges$proposal_id
  )
)

reviewer_ids <- sort(
  unique(
    candidate_edges$reviewer_id
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

if (n_proposals != 42L) {
  stop(
    "Expected 42 proposals, found ",
    n_proposals,
    "."
  )
}

if (n_reviewers != 9L) {
  stop(
    "Expected 9 reviewers, found ",
    n_reviewers,
    "."
  )
}


# Reconstruct the observed assignment graph from the complement of the
# 294 unobserved candidate pairs.
#
# This avoids reading the evaluation dataset. Since every proposal has exactly
# two observed reviews, each proposal has seven candidate reviewers.

all_pairs <- expand.grid(
  proposal_id = proposal_ids,
  reviewer_id = reviewer_ids,
  stringsAsFactors = FALSE
)

observed_pairs <- all_pairs |>
  dplyr::anti_join(
    candidate_edges |>
      dplyr::select(
        proposal_id,
        reviewer_id
      ),
    by = c(
      "proposal_id",
      "reviewer_id"
    )
  ) |>
  dplyr::arrange(
    proposal_id,
    reviewer_id
  )

if (nrow(observed_pairs) != 84L) {
  stop(
    "Expected 84 observed assignments after reconstruction, found ",
    nrow(observed_pairs),
    "."
  )
}

candidate_counts_by_proposal <- table(
  candidate_edges$proposal_id
)

if (
  length(candidate_counts_by_proposal) != 42L ||
  any(
    candidate_counts_by_proposal != 7L
  )
) {
  stop(
    "Each proposal should have exactly seven unobserved reviewer candidates."
  )
}


# Load proposal-level observed disagreement

proposal_disagreement <- read.csv(
  disagreement_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_disagreement_columns <- c(
  "proposal_id",
  "observed_abs_disagreement"
)

missing_disagreement_columns <- setdiff(
  required_disagreement_columns,
  names(proposal_disagreement)
)

if (length(missing_disagreement_columns) > 0) {
  stop(
    "Missing disagreement columns: ",
    paste(
      missing_disagreement_columns,
      collapse = ", "
    )
  )
}

proposal_disagreement$proposal_id <- trimws(
  as.character(
    proposal_disagreement$proposal_id
  )
)

proposal_disagreement$observed_abs_disagreement <- as.numeric(
  proposal_disagreement$observed_abs_disagreement
)

proposal_disagreement <- proposal_disagreement |>
  dplyr::select(
    proposal_id,
    observed_abs_disagreement
  ) |>
  dplyr::distinct()

if (nrow(proposal_disagreement) != 42L) {
  stop(
    "Expected one disagreement value for each of 42 proposals."
  )
}

if (
  !setequal(
    proposal_disagreement$proposal_id,
    proposal_ids
  )
) {
  stop(
    "Proposal IDs in the disagreement summary do not match the review-design IDs."
  )
}

if (
  any(
    !is.finite(
      proposal_disagreement$observed_abs_disagreement
    )
  )
) {
  stop(
    "Non-finite observed disagreement values were found."
  )
}

candidate_edges <- candidate_edges |>
  dplyr::left_join(
    proposal_disagreement,
    by = "proposal_id"
  )


# Build observed incidence matrix and graph Laplacian

X_observed <- matrix(
  0,
  nrow = nrow(observed_pairs),
  ncol = n_vertices
)

for (r in seq_len(nrow(observed_pairs))) {
  proposal_position <- match(
    observed_pairs$proposal_id[r],
    proposal_ids
  )
  
  reviewer_position <-
    n_proposals +
    match(
      observed_pairs$reviewer_id[r],
      reviewer_ids
    )
  
  X_observed[r, proposal_position] <- 1
  X_observed[r, reviewer_position] <- -1
}

L_G <- crossprod(
  X_observed
)

if (
  max(
    abs(
      rowSums(
        L_G
      )
    )
  ) > 1e-10
) {
  stop(
    "Reconstructed graph Laplacian does not have zero row sums."
  )
}


# Candidate endpoint positions

candidate_proposal_position <- match(
  candidate_edges$proposal_id,
  proposal_ids
)

candidate_reviewer_position <-
  n_proposals +
  match(
    candidate_edges$reviewer_id,
    reviewer_ids
  )

if (
  anyNA(
    candidate_proposal_position
  ) ||
  anyNA(
    candidate_reviewer_position
  )
) {
  stop(
    "Could not map all candidate edges to graph vertices."
  )
}


# Load V1 posterior scale draws

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

n_design_draws <- min(
  n_design_draws_requested,
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

sigma_draws <- design_draws[
  ,
  "sigma"
]

message(
  "Posterior draws available: ",
  nrow(v1_draws)
)

message(
  "Posterior draws used for policy comparison: ",
  n_design_draws
)

message(
  "Random policy paths: ",
  n_random_paths
)

message(
  "Maximum additional-review budget: ",
  max_budget
)


# Orthonormal proposal-contrast basis

C <- qr.Q(
  qr(
    stats::contr.helmert(
      n_proposals
    )
  )
)

if (
  nrow(C) != n_proposals ||
  ncol(C) != n_proposals - 1L
) {
  stop(
    "Contrast basis has unexpected dimensions."
  )
}

if (
  max(
    abs(
      colSums(
        C
      )
    )
  ) > 1e-10
) {
  stop(
    "Contrast basis is not orthogonal to the common-shift direction."
  )
}

a_factor <-
  2 /
  (n_proposals - 1)


# A-optimality loss for one conditional covariance matrix
#
# With an orthonormal basis C for the proposal-contrast subspace,
#
# L_A(V_theta)
# =
# 2/(n-1) * tr(C' V_theta C),
#
# which is equivalent to the average posterior variance of all pairwise
# proposal contrasts used in Chapter 6.

a_loss <- function(
    V
) {
  V_theta <- V[
    seq_len(n_proposals),
    seq_len(n_proposals),
    drop = FALSE
  ]
  
  a_factor *
    sum(
      diag(
        crossprod(
          C,
          V_theta %*% C
        )
      )
    )
}


# Construct baseline conditional covariance for each selected posterior draw

baseline_V <- array(
  NA_real_,
  dim = c(
    n_vertices,
    n_vertices,
    n_design_draws
  )
)

baseline_A_loss <- numeric(
  n_design_draws
)

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
  
  if (
    tau_theta <= 0 ||
    tau_b <= 0 ||
    sigma <= 0
  ) {
    stop(
      "Posterior scale parameters must be positive."
    )
  }
  
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
  
  Q <-
    P0 +
    L_G / sigma^2
  
  V <- solve(
    Q
  )
  
  V <- (
    V +
      t(V)
  ) / 2
  
  baseline_V[
    ,
    ,
    s
  ] <- V
  
  baseline_A_loss[s] <- a_loss(
    V
  )
}

if (
  any(
    !is.finite(
      baseline_A_loss
    ) |
    baseline_A_loss <= 0
  )
) {
  stop(
    "Invalid baseline A-optimality loss."
  )
}


# Posterior-averaged marginal A reduction for a set of candidate edges
#
# This vectorized calculation exploits the sparse incidence vector:
# V x_e is simply the proposal column of V minus the reviewer column.

posterior_mean_A_reduction <- function(
    V_array,
    candidate_indices
) {
  if (length(candidate_indices) == 0L) {
    return(
      numeric(0)
    )
  }
  
  proposal_positions <-
    candidate_proposal_position[
      candidate_indices
    ]
  
  reviewer_positions <-
    candidate_reviewer_position[
      candidate_indices
    ]
  
  accumulated_reduction <- numeric(
    length(
      candidate_indices
    )
  )
  
  for (s in seq_len(n_design_draws)) {
    V <- V_array[
      ,
      ,
      s
    ]
    
    # t(C) times the proposal rows of V.
    W <- crossprod(
      C,
      V[
        seq_len(n_proposals),
        ,
        drop = FALSE
      ]
    )
    
    Z <-
      W[
        ,
        proposal_positions,
        drop = FALSE
      ] -
      W[
        ,
        reviewer_positions,
        drop = FALSE
      ]
    
    denominator <-
      sigma_draws[s]^2 +
      V[
        cbind(
          proposal_positions,
          proposal_positions
        )
      ] +
      V[
        cbind(
          reviewer_positions,
          reviewer_positions
        )
      ] -
      2 *
      V[
        cbind(
          proposal_positions,
          reviewer_positions
        )
      ]
    
    if (
      any(
        !is.finite(
          denominator
        ) |
        denominator <= 0
      )
    ) {
      stop(
        "Invalid Sherman-Morrison denominator while evaluating candidates."
      )
    }
    
    accumulated_reduction <-
      accumulated_reduction +
      a_factor *
      colSums(
        Z^2
      ) /
      denominator
  }
  
  accumulated_reduction /
    n_design_draws
}


# Apply one selected edge to every posterior-draw covariance state

update_all_draws <- function(
    V_array,
    candidate_index
) {
  proposal_position <-
    candidate_proposal_position[
      candidate_index
    ]
  
  reviewer_position <-
    candidate_reviewer_position[
      candidate_index
    ]
  
  delta_A <- numeric(
    n_design_draws
  )
  
  for (s in seq_len(n_design_draws)) {
    V <- V_array[
      ,
      ,
      s
    ]
    
    u <-
      V[
        ,
        proposal_position
      ] -
      V[
        ,
        reviewer_position
      ]
    
    denominator <-
      sigma_draws[s]^2 +
      V[
        proposal_position,
        proposal_position
      ] +
      V[
        reviewer_position,
        reviewer_position
      ] -
      2 *
      V[
        proposal_position,
        reviewer_position
      ]
    
    if (
      !is.finite(
        denominator
      ) ||
      denominator <= 0
    ) {
      stop(
        "Invalid Sherman-Morrison denominator during policy update."
      )
    }
    
    z <- as.numeric(
      crossprod(
        C,
        u[
          seq_len(n_proposals)
        ]
      )
    )
    
    delta_A[s] <-
      a_factor *
      sum(
        z^2
      ) /
      denominator
    
    V_new <-
      V -
      tcrossprod(
        u
      ) /
      denominator
    
    V_array[
      ,
      ,
      s
    ] <- (
      V_new +
        t(V_new)
    ) / 2
  }
  
  list(
    V = V_array,
    delta_A = delta_A
  )
}


# Apply one selected edge to a single posterior-draw covariance state

update_one_draw <- function(
    V,
    sigma,
    candidate_index
) {
  proposal_position <-
    candidate_proposal_position[
      candidate_index
    ]
  
  reviewer_position <-
    candidate_reviewer_position[
      candidate_index
    ]
  
  u <-
    V[
      ,
      proposal_position
    ] -
    V[
      ,
      reviewer_position
    ]
  
  denominator <-
    sigma^2 +
    V[
      proposal_position,
      proposal_position
    ] +
    V[
      reviewer_position,
      reviewer_position
    ] -
    2 *
    V[
      proposal_position,
      reviewer_position
    ]
  
  if (
    !is.finite(
      denominator
    ) ||
    denominator <= 0
  ) {
    stop(
      "Invalid Sherman-Morrison denominator in random-policy simulation."
    )
  }
  
  z <- as.numeric(
    crossprod(
      C,
      u[
        seq_len(n_proposals)
      ]
    )
  )
  
  delta_A <-
    a_factor *
    sum(
      z^2
    ) /
    denominator
  
  V_new <-
    V -
    tcrossprod(
      u
    ) /
    denominator
  
  V_new <- (
    V_new +
      t(V_new)
  ) / 2
  
  list(
    V = V_new,
    delta_A = delta_A
  )
}


# Summarize posterior-averaged A-optimality reduction
#
# The single-review analysis ranks edges by posterior-averaged absolute
# A-optimality reduction. The multi-review comparison therefore uses the same
# convention. Percentage reduction is calculated only after averaging the
# baseline and updated A losses over posterior draws:
#
#   100 * E[L_A(0) - L_A(m)] / E[L_A(0)].
#
# This is preferable here to averaging draw-wise percentages, which gives
# disproportionate weight to posterior draws with very small baseline loss.

baseline_A_mean <- mean(
  baseline_A_loss
)

summarise_posterior_average_reduction <- function(
    policy,
    budget,
    current_A_loss
) {
  posterior_mean_current_loss <-
    mean(
      current_A_loss
    )
  
  posterior_mean_reduction <-
    mean(
      baseline_A_loss -
        current_A_loss
    )
  
  tibble::tibble(
    policy = policy,
    budget = budget,
    posterior_mean_A_loss_before =
      baseline_A_mean,
    posterior_mean_A_loss_after =
      posterior_mean_current_loss,
    posterior_mean_A_reduction =
      posterior_mean_reduction,
    percent_reduction =
      100 *
      posterior_mean_reduction /
      baseline_A_mean
  )
}


# Sequential deterministic policies
#
# Both policies use the same A-optimality objective when a reviewer must be
# chosen. They differ in how the proposal is targeted:
#
# - Sequential A-optimal searches globally over all eligible edges.
# - Disagreement-targeted first restricts attention to proposals with the
#   largest original reviewer disagreement, then chooses the most A-informative
#   available edge within that highest-disagreement group.
#
# At most one additional review is assigned to any proposal.

run_deterministic_policy <- function(
    policy = c(
      "Sequential A-optimal",
      "Disagreement-targeted"
    )
) {
  policy <- match.arg(
    policy
  )
  
  V_current <- baseline_V
  
  current_A_loss <- baseline_A_loss
  
  used_proposals <- character(0)
  
  assignment_rows <- vector(
    "list",
    max_budget
  )
  
  summary_rows <- vector(
    "list",
    max_budget
  )
  
  for (step in seq_len(max_budget)) {
    eligible_indices <- which(
      !candidate_edges$proposal_id %in%
        used_proposals
    )
    
    if (length(eligible_indices) == 0L) {
      stop(
        "No eligible candidate edges remain."
      )
    }
    
    if (
      policy ==
      "Disagreement-targeted"
    ) {
      highest_disagreement <- max(
        candidate_edges$observed_abs_disagreement[
          eligible_indices
        ]
      )
      
      eligible_indices <- eligible_indices[
        candidate_edges$observed_abs_disagreement[
          eligible_indices
        ] ==
          highest_disagreement
      ]
    }
    
    mean_reduction <-
      posterior_mean_A_reduction(
        V_current,
        eligible_indices
      )
    
    choice_table <- tibble::tibble(
      candidate_index =
        eligible_indices,
      mean_A_reduction =
        mean_reduction,
      proposal_id =
        candidate_edges$proposal_id[
          eligible_indices
        ],
      reviewer_id =
        candidate_edges$reviewer_id[
          eligible_indices
        ]
    ) |>
      dplyr::arrange(
        dplyr::desc(
          mean_A_reduction
        ),
        proposal_id,
        reviewer_id
      )
    
    selected_index <-
      choice_table$candidate_index[1]
    
    selected_mean_reduction <-
      choice_table$mean_A_reduction[1]
    
    update <- update_all_draws(
      V_current,
      selected_index
    )
    
    V_current <- update$V
    
    current_A_loss <-
      current_A_loss -
      update$delta_A
    
    selected_proposal <-
      candidate_edges$proposal_id[
        selected_index
      ]
    
    used_proposals <- c(
      used_proposals,
      selected_proposal
    )
    
    assignment_rows[[step]] <-
      tibble::tibble(
        policy = policy,
        step = step,
        proposal_id =
          selected_proposal,
        reviewer_id =
          candidate_edges$reviewer_id[
            selected_index
          ],
        observed_abs_disagreement =
          candidate_edges$observed_abs_disagreement[
            selected_index
          ],
        posterior_mean_marginal_A_reduction =
          selected_mean_reduction
      )
    
    summary_rows[[step]] <-
      summarise_posterior_average_reduction(
        policy = policy,
        budget = step,
        current_A_loss =
          current_A_loss
      )
    
    message(
      policy,
      " step ",
      step,
      ": ",
      selected_proposal,
      " -- ",
      candidate_edges$reviewer_id[
        selected_index
      ],
      " | disagreement = ",
      candidate_edges$observed_abs_disagreement[
        selected_index
      ],
      " | mean marginal A reduction = ",
      format(
        selected_mean_reduction,
        scientific = TRUE,
        digits = 6
      )
    )
  }
  
  list(
    assignments =
      dplyr::bind_rows(
        assignment_rows
      ),
    summary =
      dplyr::bind_rows(
        summary_rows
      )
  )
}


message(
  "\nRunning sequential A-optimal policy..."
)

sequential_A <- run_deterministic_policy(
  "Sequential A-optimal"
)

message(
  "\nRunning disagreement-targeted policy..."
)

disagreement_targeted <- run_deterministic_policy(
  "Disagreement-targeted"
)


# Random allocation policy
#
# Each random path is paired with one selected posterior scale draw and then
# allocates one random currently unobserved edge at each step, excluding
# proposals that already received an additional review.
#
# The posterior-draw sequence is balanced: with 5,000 paths and 5,000 design
# draws, every selected posterior draw is used exactly once. This makes the
# random-policy mean directly comparable with the posterior averages used for
# the deterministic policies.

set.seed(
  random_seed
)

posterior_draw_sequence <- sample(
  rep(
    seq_len(
      n_design_draws
    ),
    length.out =
      n_random_paths
  ),
  size =
    n_random_paths,
  replace = FALSE
)

random_rows <- vector(
  "list",
  n_random_paths
)

for (replicate_id in seq_len(n_random_paths)) {
  posterior_draw_index <-
    posterior_draw_sequence[
      replicate_id
    ]
  
  V_current <- baseline_V[
    ,
    ,
    posterior_draw_index
  ]
  
  baseline_loss <-
    baseline_A_loss[
      posterior_draw_index
    ]
  
  current_loss <-
    baseline_loss
  
  used_proposals <- character(0)
  
  replicate_rows <- vector(
    "list",
    max_budget
  )
  
  for (step in seq_len(max_budget)) {
    eligible_indices <- which(
      !candidate_edges$proposal_id %in%
        used_proposals
    )
    
    selected_index <- sample(
      eligible_indices,
      size = 1L
    )
    
    update <- update_one_draw(
      V = V_current,
      sigma =
        sigma_draws[
          posterior_draw_index
        ],
      candidate_index =
        selected_index
    )
    
    V_current <- update$V
    
    current_loss <-
      current_loss -
      update$delta_A
    
    selected_proposal <-
      candidate_edges$proposal_id[
        selected_index
      ]
    
    used_proposals <- c(
      used_proposals,
      selected_proposal
    )
    
    absolute_A_reduction <-
      baseline_loss -
      current_loss
    
    drawwise_percent_reduction <-
      100 *
      absolute_A_reduction /
      baseline_loss
    
    replicate_rows[[step]] <-
      tibble::tibble(
        replicate =
          replicate_id,
        posterior_draw_index =
          posterior_draw_index,
        budget =
          step,
        selected_proposal =
          selected_proposal,
        selected_reviewer =
          candidate_edges$reviewer_id[
            selected_index
          ],
        baseline_A_loss =
          baseline_loss,
        current_A_loss =
          current_loss,
        absolute_A_reduction =
          absolute_A_reduction,
        drawwise_percent_reduction =
          drawwise_percent_reduction
      )
  }
  
  random_rows[[replicate_id]] <-
    dplyr::bind_rows(
      replicate_rows
    )
  
  if (
    replicate_id == 1L ||
    replicate_id %% 500L == 0L ||
    replicate_id == n_random_paths
  ) {
    message(
      "Completed random path ",
      replicate_id,
      " of ",
      n_random_paths
    )
  }
}

random_paths <- dplyr::bind_rows(
  random_rows
)

random_summary <- random_paths |>
  dplyr::group_by(
    budget
  ) |>
  dplyr::summarise(
    policy =
      "Random",
    posterior_mean_A_loss_before =
      baseline_A_mean,
    posterior_mean_A_loss_after =
      mean(
        current_A_loss
      ),
    posterior_mean_A_reduction =
      mean(
        absolute_A_reduction
      ),
    percent_reduction =
      100 *
      posterior_mean_A_reduction /
      baseline_A_mean,
    drawwise_q05_percent =
      unname(
        stats::quantile(
          drawwise_percent_reduction,
          0.05
        )
      ),
    drawwise_q95_percent =
      unname(
        stats::quantile(
          drawwise_percent_reduction,
          0.95
        )
      ),
    .groups = "drop"
  )


# Combine and save policy summaries

policy_summary <- dplyr::bind_rows(
  random_summary,
  disagreement_targeted$summary,
  sequential_A$summary
) |>
  dplyr::mutate(
    policy = factor(
      policy,
      levels = c(
        "Random",
        "Disagreement-targeted",
        "Sequential A-optimal"
      )
    )
  ) |>
  dplyr::arrange(
    budget,
    policy
  )

policy_assignments <- dplyr::bind_rows(
  disagreement_targeted$assignments,
  sequential_A$assignments
)

write.csv(
  policy_summary,
  file.path(
    results_dir,
    "multi_review_policy_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  policy_assignments,
  file.path(
    results_dir,
    "multi_review_policy_assignments.csv"
  ),
  row.names = FALSE
)

write.csv(
  random_paths,
  file.path(
    results_dir,
    "random_policy_simulation.csv"
  ),
  row.names = FALSE
)

write.csv(
  data.frame(
    setting = c(
      "max_budget",
      "posterior_draws_used",
      "random_paths",
      "random_seed",
      "max_one_additional_review_per_proposal"
    ),
    value = c(
      max_budget,
      n_design_draws,
      n_random_paths,
      random_seed,
      TRUE
    )
  ),
  file.path(
    results_dir,
    "multi_review_policy_settings.csv"
  ),
  row.names = FALSE
)


# Create practical comparison figure
#
# The raw draw-wise interval is not shown because it is dominated by posterior
# uncertainty in the scale parameters and therefore obscures the policy
# comparison. The figure displays the posterior-averaged policy performance,
# which is the quantity used for the main comparison.

policy_plot <- ggplot2::ggplot(
  policy_summary,
  ggplot2::aes(
    x = budget,
    y = percent_reduction,
    linetype = policy,
    shape = policy,
    group = policy
  )
) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    size = 2.4
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_len(
      max_budget
    )
  ) +
  ggplot2::labs(
    title =
      "Limited additional-review allocation policies",
    subtitle =
      "Posterior-averaged A-optimality reduction; each proposal can receive at most one additional review",
    x =
      "Number of additional reviews",
    y =
      "Reduction in average pairwise proposal-contrast variance (%)",
    linetype =
      "Allocation policy",
    shape =
      "Allocation policy"
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    legend.position = "bottom"
  )

figure_name <-
  "review_policy_comparison.png"

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    figure_name
  ),
  plot = policy_plot,
  width = 8.8,
  height = 5.8,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path(
    report_figures_dir,
    figure_name
  ),
  plot = policy_plot,
  width = 8.8,
  height = 5.8,
  dpi = 300
)


# Write compact factual output for the technical report

budget_six <- policy_summary |>
  dplyr::filter(
    budget == max_budget
  ) |>
  dplyr::mutate(
    policy = as.character(
      policy
    )
  )

lookup_value <- function(
    policy_name,
    column_name
) {
  budget_six[
    budget_six$policy == policy_name,
    column_name
  ][[1]]
}

sequential_six <- lookup_value(
  "Sequential A-optimal",
  "percent_reduction"
)

disagreement_six <- lookup_value(
  "Disagreement-targeted",
  "percent_reduction"
)

random_six <- lookup_value(
  "Random",
  "percent_reduction"
)

seq_minus_random <-
  sequential_six -
  random_six

disagreement_minus_random <-
  disagreement_six -
  random_six

report_lines <- c(
  paste0(
    "At a budget of ",
    max_budget,
    " additional reviews, the posterior-averaged reduction in average pairwise ",
    "proposal-contrast variance was ",
    sprintf("%.3f", sequential_six),
    "% under sequential A-optimal allocation, ",
    sprintf("%.3f", disagreement_six),
    "% under disagreement-targeted allocation, and ",
    sprintf("%.3f", random_six),
    "% under random allocation."
  ),
  paste0(
    "Relative to random allocation, the difference was ",
    sprintf("%+.3f", seq_minus_random),
    " percentage points for sequential A-optimal allocation and ",
    sprintf("%+.3f", disagreement_minus_random),
    " percentage points for disagreement-targeted allocation."
  )
)

writeLines(
  report_lines,
  file.path(
    results_dir,
    "multi_review_policy_report_numbers.txt"
  )
)


# Print main results

message(
  "\nPolicy comparison finished."
)

message(
  "\nPolicy summary:"
)

print(
  policy_summary
)

message(
  "\nSelected deterministic assignments:"
)

print(
  policy_assignments
)

message(
  "\nReport-ready numerical summary:"
)

cat(
  paste(
    report_lines,
    collapse = "\n"
  ),
  "\n"
)

message(
  "\nResults written to: ",
  results_dir
)

message(
  "Figure written to: ",
  file.path(
    figures_dir,
    figure_name
  )
)