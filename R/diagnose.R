#' Diagnose functions on paper similarity
#'
#' @param item_df A tibble with papers.
#' @param distance_df A tibble with paper distance metrics.
#'
#' @returns
#' Diagnostic summary of the count, proportion, and average similarity score
#' for number of decisions per paper pair.
#'
#' Diagnostic summary of item score for each paper pair.
#'
#' @export
#' @rdname diagnose
diag_decision_ppp <- function(item_df, distance_df){

  avg_similarity_score_df <- item_df |>
    dplyr::left_join(distance_df) |>
    dplyr::group_by(n) |>
    dplyr::summarize(similarity = mean(1 - dist))

  df |>
    dplyr::count(paper1, paper2, sort = TRUE) |>
    dplyr::count(n) |>
    dplyr::mutate(prop = nn/sum(nn)) |>
    dplyr::left_join(ave_similarity_score_df) |>
    dplyr::rename(num_decision_ppp = n, count = nn)

}

#' View the coding detail for a pair of papers
#'
#' @param paper_df A data frame containing paper codings
#' @param item_df A data frame containing item-level distances
#' @param pp1,pp2 A single string each to identify the two papers
#'
#' @returns
#' A data frame containing item-level comparisons between the two papers.
#'
#' @export
#' @rdname diagnose
#' @export
#' @rdname diagnose
view_pairs <- function(paper_df, item_df, pp1, pp2){
  res <- paper_df |>
    dplyr::filter(paper %in% c(pp1, pp2)) |>
    tidyr::pivot_longer(-c(paper, model), names_to = "item", values_to = "reason") |>
    tidyr::pivot_wider(id_cols = -model, names_from = paper, values_from = reason) |>
    unnest(pp1, pp2)

  score_df <- item_df |>
    dplyr::filter((paper1 == pp1 & paper2 == pp2) | (paper1 == pp2 & paper2 == pp1)) |>
    dplyr::select(item, dist)

  res |>
    dplyr::left_join(score_df) |>
    dplyr::select(-pp1, -pp2)

}
