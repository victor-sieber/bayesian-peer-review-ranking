# Visualize posterior-averaged single-review design results.
#
# This script reads the derived candidate-edge results from
# analysis/07_single_review_design.R and creates the A-optimality heatmap used
# in Chapter 6. It does not read the evaluation dataset or refit any model.
#
# Run from the project root with:
# source("analysis/08_visualize_review_design_results.R")


# Packages

required_packages <- c(
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
    )
  )
}


# Paths

results_path <- file.path(
  "results",
  "review_design",
  "single_review_candidate_results.csv"
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

dir.create(
  figures_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  report_figures_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(results_path)) {
  stop(
    "Review-design results not found: ",
    results_path,
    ". Run analysis/07_single_review_design.R first."
  )
}


# Load derived candidate-edge results

candidate_results <- read.csv(
  results_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_columns <- c(
  "proposal_id",
  "reviewer_id",
  "A_mean",
  "A_rank"
)

missing_columns <- setdiff(
  required_columns,
  names(candidate_results)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing columns in review-design results: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}

if (nrow(candidate_results) != 294) {
  stop(
    "Expected 294 candidate assignments, found ",
    nrow(candidate_results),
    "."
  )
}

proposal_ids <- sort(
  unique(
    candidate_results$proposal_id
  )
)

reviewer_ids <- sort(
  unique(
    candidate_results$reviewer_id
  )
)

if (length(proposal_ids) != 42) {
  stop(
    "Expected 42 proposals in the candidate results."
  )
}

if (length(reviewer_ids) != 9) {
  stop(
    "Expected 9 reviewers in the candidate results."
  )
}


# Reconstruct the complete 42 x 9 assignment grid
#
# Candidate pairs have an A_mean value. Pairs absent from the candidate table
# are the 84 assignments already observed in the original review design.

all_pairs <- expand.grid(
  proposal_id = proposal_ids,
  reviewer_id = reviewer_ids,
  stringsAsFactors = FALSE
)

heatmap_data <- all_pairs |>
  dplyr::left_join(
    candidate_results |>
      dplyr::select(
        proposal_id,
        reviewer_id,
        A_mean,
        A_rank
      ),
    by = c(
      "proposal_id",
      "reviewer_id"
    )
  ) |>
  dplyr::mutate(
    assignment_status = dplyr::if_else(
      is.na(A_mean),
      "Observed assignment",
      "Candidate assignment"
    ),
    reviewer_id = factor(
      reviewer_id,
      levels = reviewer_ids
    ),
    proposal_id = factor(
      proposal_id,
      levels = rev(proposal_ids)
    )
  )

if (
  sum(
    heatmap_data$assignment_status == "Observed assignment"
  ) != 84
) {
  stop(
    "Expected 84 observed assignments in the reconstructed grid."
  )
}


# Plot A-optimality utility
#
# A_rank <= 4 outlines the highest-ranked group. Rank 4 is shared by several
# candidate assignments, so all tied assignments are retained.

heatmap_plot <- ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = heatmap_data |>
      dplyr::filter(
        assignment_status == "Observed assignment"
      ),
    ggplot2::aes(
      x = reviewer_id,
      y = proposal_id
    ),
    fill = "grey85",
    color = "white",
    linewidth = 0.25
  ) +
  ggplot2::geom_tile(
    data = heatmap_data |>
      dplyr::filter(
        assignment_status == "Candidate assignment"
      ),
    ggplot2::aes(
      x = reviewer_id,
      y = proposal_id,
      fill = A_mean
    ),
    color = "white",
    linewidth = 0.25
  ) +
  ggplot2::geom_tile(
    data = heatmap_data |>
      dplyr::filter(
        !is.na(A_rank),
        A_rank <= 4
      ),
    ggplot2::aes(
      x = reviewer_id,
      y = proposal_id
    ),
    fill = NA,
    color = "black",
    linewidth = 0.7
  ) +
  ggplot2::scale_fill_viridis_c(
    name = expression(bar(Delta)[A])
  ) +
  ggplot2::labs(
    x = "Reviewer",
    y = "Proposal"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  ) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      size = 8
    ),
    axis.text.y = ggplot2::element_text(
      size = 6.5
    ),
    legend.position = "right"
  )


# Save figure in the analysis and report figure directories

figure_path <- file.path(
  figures_dir,
  "a_optimality_candidate_heatmap.png"
)

report_figure_path <- file.path(
  report_figures_dir,
  "a_optimality_candidate_heatmap.png"
)

ggplot2::ggsave(
  filename = figure_path,
  plot = heatmap_plot,
  width = 8.2,
  height = 11.5,
  dpi = 300
)

file.copy(
  from = figure_path,
  to = report_figure_path,
  overwrite = TRUE
)

message(
  "A-optimality heatmap written to: ",
  figure_path
)