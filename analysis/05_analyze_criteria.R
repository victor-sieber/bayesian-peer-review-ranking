# Descriptive analysis of CRS supporting criteria and holistic overall grades.
#
# Run from the evaluation-data project root with:
# source("analysis/05_analyze_criteria.R")

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

results_dir <- file.path(
  "results",
  "criteria_descriptive"
)

figures_dir <- file.path(
  "figures",
  "criteria_descriptive"
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
  stop("Data file not found: ", data_path)
}

# 3. Read and validate data

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

if (nrow(reviews) != 84) {
  stop("Expected 84 completed reviews.")
}

if (length(unique(reviews$proposal_id)) != 42) {
  stop("Expected 42 proposals.")
}

if (length(unique(reviews$reviewer_id)) != 9) {
  stop("Expected 9 reviewers.")
}

message("Rows imported: ", nrow(reviews))
message("Proposals: ", length(unique(reviews$proposal_id)))
message("Reviewers: ", length(unique(reviews$reviewer_id)))

# 4. Construct equal-weight criterion average

reviews$criterion_average <- rowMeans(
  reviews[, criterion_columns]
)

reviews$criterion_minus_overall <-
  reviews$criterion_average - reviews$overall_grade

# 5. Reviewer-level descriptive summary

reviewer_summary <- reviews |>
  dplyr::group_by(reviewer_id) |>
  dplyr::summarise(
    n_reviews = dplyr::n(),
    mean_overall_grade = mean(overall_grade),
    sd_overall_grade = stats::sd(overall_grade),
    minimum_overall_grade = min(overall_grade),
    maximum_overall_grade = max(overall_grade),
    mean_criterion_average = mean(criterion_average),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(n_reviews),
    reviewer_id
  )

write.csv(
  reviewer_summary,
  file.path(
    results_dir,
    "reviewer_summary.csv"
  ),
  row.names = FALSE
)

# 6. Individual criteria versus holistic overall grade

criterion_labels <- c(
  scientific_quality = "Scientific quality",
  community_building = "Community building",
  scaling_potential = "Scaling potential",
  scientific_rigor = "Scientific rigor"
)

criterion_correlations <- lapply(
  criterion_columns,
  function(criterion_name) {
    x <- reviews[[criterion_name]]
    y <- reviews$overall_grade
    
    tibble::tibble(
      criterion = criterion_name,
      criterion_label = unname(
        criterion_labels[[criterion_name]]
      ),
      mean_criterion_score = mean(x),
      sd_criterion_score = stats::sd(x),
      pearson_correlation = stats::cor(
        x,
        y,
        method = "pearson"
      ),
      spearman_correlation = stats::cor(
        x,
        y,
        method = "spearman"
      )
    )
  }
) |>
  dplyr::bind_rows() |>
  dplyr::arrange(
    dplyr::desc(spearman_correlation)
  )

write.csv(
  criterion_correlations,
  file.path(
    results_dir,
    "criterion_correlations_with_overall.csv"
  ),
  row.names = FALSE
)

# 7. Equal-weight criterion average versus overall grade at review level

review_level_summary <- tibble::tibble(
  metric = c(
    "Number of reviews",
    "Mean holistic overall grade",
    "Mean equal-weight criterion average",
    "Pearson correlation",
    "Spearman correlation",
    "Mean criterion average minus overall grade",
    "Mean absolute difference",
    "Proportion within 0.5 grade points",
    "Proportion exactly equal"
  ),
  value = c(
    nrow(reviews),
    mean(reviews$overall_grade),
    mean(reviews$criterion_average),
    stats::cor(
      reviews$criterion_average,
      reviews$overall_grade,
      method = "pearson"
    ),
    stats::cor(
      reviews$criterion_average,
      reviews$overall_grade,
      method = "spearman"
    ),
    mean(reviews$criterion_minus_overall),
    mean(abs(reviews$criterion_minus_overall)),
    mean(abs(reviews$criterion_minus_overall) <= 0.5),
    mean(reviews$criterion_average == reviews$overall_grade)
  )
)

write.csv(
  review_level_summary,
  file.path(
    results_dir,
    "review_level_equal_weight_summary.csv"
  ),
  row.names = FALSE
)

# 8. Proposal-level equal-weight criterion summary

proposal_summary <- reviews |>
  dplyr::group_by(proposal_id) |>
  dplyr::summarise(
    n_reviews = dplyr::n(),
    mean_overall_grade = mean(overall_grade),
    mean_criterion_average = mean(criterion_average),
    minimum_overall_grade = min(overall_grade),
    maximum_overall_grade = max(overall_grade),
    qualifies_original =
      n_reviews == 2 &
      maximum_overall_grade == 5 &
      minimum_overall_grade >= 4,
    .groups = "drop"
  ) |>
  dplyr::mutate(
    overall_raw_rank = rank(
      -mean_overall_grade,
      ties.method = "average"
    ),
    criterion_raw_rank = rank(
      -mean_criterion_average,
      ties.method = "average"
    ),
    criterion_minus_overall =
      mean_criterion_average - mean_overall_grade,
    delta_raw_rank =
      criterion_raw_rank - overall_raw_rank,
    abs_delta_raw_rank = abs(delta_raw_rank)
  ) |>
  dplyr::arrange(
    dplyr::desc(abs_delta_raw_rank),
    proposal_id
  )

write.csv(
  proposal_summary,
  file.path(
    results_dir,
    "proposal_level_comparison.csv"
  ),
  row.names = FALSE
)

proposal_level_summary <- tibble::tibble(
  metric = c(
    "Pearson correlation of proposal means",
    "Spearman correlation of proposal means",
    "Spearman correlation of raw ranks",
    "Mean absolute raw-rank change",
    "Maximum absolute raw-rank change",
    "Number of proposals changing raw rank",
    "Number of original qualifiers in criterion top four"
  ),
  value = c(
    stats::cor(
      proposal_summary$mean_overall_grade,
      proposal_summary$mean_criterion_average,
      method = "pearson"
    ),
    stats::cor(
      proposal_summary$mean_overall_grade,
      proposal_summary$mean_criterion_average,
      method = "spearman"
    ),
    stats::cor(
      proposal_summary$overall_raw_rank,
      proposal_summary$criterion_raw_rank,
      method = "spearman"
    ),
    mean(proposal_summary$abs_delta_raw_rank),
    max(proposal_summary$abs_delta_raw_rank),
    sum(proposal_summary$delta_raw_rank != 0),
    sum(
      proposal_summary$qualifies_original &
        proposal_summary$criterion_raw_rank <= 4
    )
  )
)

write.csv(
  proposal_level_summary,
  file.path(
    results_dir,
    "proposal_level_summary.csv"
  ),
  row.names = FALSE
)

# 9. Review-level figure

review_level_plot <- ggplot2::ggplot(
  reviews,
  ggplot2::aes(
    x = overall_grade,
    y = criterion_average
  )
) +
  ggplot2::geom_count() +
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  ggplot2::scale_x_continuous(
    breaks = 1:5,
    limits = c(0.75, 5.25)
  ) +
  ggplot2::scale_y_continuous(
    breaks = 1:5,
    limits = c(0.75, 5.25)
  ) +
  ggplot2::labs(
    title = "Equal-weight criterion average versus holistic overall grade",
    subtitle = "One point location per review; point size indicates repeated combinations",
    x = "Holistic overall grade",
    y = "Equal-weight criterion average",
    size = "Number of reviews"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "review_level_criterion_average_vs_overall.png"
  ),
  plot = review_level_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# 10. Proposal-level figure

label_proposals <- proposal_summary |>
  dplyr::arrange(
    dplyr::desc(abs_delta_raw_rank),
    proposal_id
  ) |>
  dplyr::slice_head(n = 5) |>
  dplyr::pull(proposal_id)

label_proposals <- union(
  label_proposals,
  proposal_summary |>
    dplyr::filter(qualifies_original) |>
    dplyr::pull(proposal_id)
)

proposal_level_plot <- ggplot2::ggplot(
  proposal_summary,
  ggplot2::aes(
    x = mean_overall_grade,
    y = mean_criterion_average
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  ggrepel::geom_text_repel(
    data = proposal_summary |>
      dplyr::filter(
        proposal_id %in% label_proposals
      ),
    ggplot2::aes(label = proposal_id),
    seed = 20260727,
    max.overlaps = Inf,
    box.padding = 0.3,
    point.padding = 0.2
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(1, 5, by = 0.5),
    limits = c(0.75, 5.25)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(1, 5, by = 0.5),
    limits = c(0.75, 5.25)
  ) +
  ggplot2::labs(
    title = "Proposal-level criterion average versus overall-grade average",
    subtitle = "Original qualifiers and proposals with the largest raw-rank changes are labelled",
    x = "Mean holistic overall grade",
    y = "Mean equal-weight criterion average"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(
    figures_dir,
    "proposal_level_criterion_average_vs_overall.png"
  ),
  plot = proposal_level_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# 11. Reproducibility information

capture.output(
  sessionInfo(),
  file = file.path(
    results_dir,
    "session_info.txt"
  )
)

message("Criterion descriptive analysis finished.")
message("Results directory: ", results_dir)
message("Figures directory: ", figures_dir)

print(criterion_correlations)
print(review_level_summary)
print(proposal_level_summary)

print(
  proposal_summary |>
    dplyr::select(
      proposal_id,
      mean_overall_grade,
      mean_criterion_average,
      overall_raw_rank,
      criterion_raw_rank,
      delta_raw_rank,
      qualifies_original
    ) |>
    head(10)
)