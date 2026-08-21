library(igraph)
library(dplyr)

reviews <- read.csv(
  "data/evaluation_anonymized.csv",
  stringsAsFactors = FALSE
)

reviews$proposal_id <- trimws(as.character(reviews$proposal_id))
reviews$reviewer_id <- trimws(as.character(reviews$reviewer_id))

dir.create(
  "figures/review_design",
  recursive = TRUE,
  showWarnings = FALSE
)

# Build reviewer-pair table

proposal_pairs <- reviews |>
  dplyr::group_by(proposal_id) |>
  dplyr::summarise(
    reviewer_1 = sort(reviewer_id)[1],
    reviewer_2 = sort(reviewer_id)[2],
    .groups = "drop"
  )

# Count how often reviewer pairs occur together

reviewer_pair_counts <- proposal_pairs |>
  dplyr::count(
    reviewer_1,
    reviewer_2,
    name = "n_shared"
  ) |>
  dplyr::arrange(
    dplyr::desc(n_shared)
  )

# Choose the reviewer pair with the largest number of shared proposals

main_pair <- reviewer_pair_counts[1, ]

selected_reviewers <- c(
  main_pair$reviewer_1,
  main_pair$reviewer_2
)

# Add a third reviewer that shares the most proposals
# with either reviewer in the main pair

third_reviewer_candidates <- reviewer_pair_counts |>
  dplyr::filter(
    reviewer_1 %in% selected_reviewers |
      reviewer_2 %in% selected_reviewers
  ) |>
  dplyr::mutate(
    candidate = ifelse(
      reviewer_1 %in% selected_reviewers,
      reviewer_2,
      reviewer_1
    )
  ) |>
  dplyr::filter(
    !(candidate %in% selected_reviewers)
  ) |>
  dplyr::group_by(candidate) |>
  dplyr::summarise(
    shared_total = sum(n_shared),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(shared_total)
  )

selected_reviewers <- c(
  selected_reviewers,
  third_reviewer_candidates$candidate[1]
)

# Select proposals whose two assigned reviewers
# are both among the selected reviewers

selected_proposals <- proposal_pairs |>
  dplyr::filter(
    reviewer_1 %in% selected_reviewers,
    reviewer_2 %in% selected_reviewers
  ) |>
  dplyr::pull(proposal_id)

# If the resulting graph is too small, include proposals
# connected to at least one selected reviewer

if (length(selected_proposals) < 5) {
  
  selected_proposals <- reviews |>
    dplyr::filter(
      reviewer_id %in% selected_reviewers
    ) |>
    dplyr::distinct(proposal_id) |>
    dplyr::slice_head(n = 7) |>
    dplyr::pull(proposal_id)
}

# Restrict the observed data to the illustrative subset

reviews_subset <- reviews |>
  dplyr::filter(
    reviewer_id %in% selected_reviewers,
    proposal_id %in% selected_proposals
  )

# Build bipartite graph

edges <- data.frame(
  from = reviews_subset$reviewer_id,
  to = reviews_subset$proposal_id
)

g <- graph_from_data_frame(
  edges,
  directed = FALSE
)

V(g)$type <- V(g)$name %in% selected_proposals

# Order vertices

reviewer_order <- selected_reviewers

proposal_order <- proposal_pairs |>
  dplyr::filter(
    proposal_id %in% selected_proposals
  ) |>
  dplyr::arrange(
    reviewer_1,
    reviewer_2,
    proposal_id
  ) |>
  dplyr::pull(proposal_id)

layout <- matrix(
  NA_real_,
  nrow = vcount(g),
  ncol = 2
)

reviewer_vertices <- match(
  reviewer_order,
  V(g)$name
)

proposal_vertices <- match(
  proposal_order,
  V(g)$name
)

# Reviewers on left

layout[reviewer_vertices, 1] <- 0
layout[reviewer_vertices, 2] <- seq(
  0.85,
  0.15,
  length.out = length(reviewer_vertices)
)

# Proposals on right

layout[proposal_vertices, 1] <- 1
layout[proposal_vertices, 2] <- seq(
  0.95,
  0.05,
  length.out = length(proposal_vertices)
)

# Plot illustrative subset

png(
  "figures/review_design/reviewer_proposal_graph_subset.png",
  width = 2600,
  height = 1800,
  res = 300
)

plot(
  g,
  layout = layout,
  vertex.label = V(g)$name,
  
  vertex.size = ifelse(
    V(g)$name %in% reviewer_order,
    10,
    6
  ),
  
  vertex.label.cex = ifelse(
    V(g)$name %in% reviewer_order,
    0.95,
    0.9
  ),
  
  vertex.label.dist = ifelse(
    V(g)$name %in% reviewer_order,
    1.8,
    1.8
  ),
  
  vertex.label.degree = ifelse(
    V(g)$name %in% reviewer_order,
    pi,
    0
  ),
  
  edge.width = 1.0,
  edge.curved = 0,
  asp = FALSE,
  margin = 0.16,
  frame = FALSE
)

dev.off()

message(
  "Selected reviewers: ",
  paste(
    selected_reviewers,
    collapse = ", "
  )
)

message(
  "Selected proposals: ",
  paste(
    selected_proposals,
    collapse = ", "
  )
)