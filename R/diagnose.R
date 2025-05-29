#' Diagnose functions on paper similarity
#'
#' @param decision_df A tibble with papers.
#' @param distance_df A tibble with paper distance metrics.
#'
#' @returns
#' Diagnostic summary of the count, proportion, and average similarity score
#' for number of decisions per paper pair.
#'
#' Diagnostic summary of decisions similarity score for each paper pair.
#'
#' @export
#' @rdname diagnose
diag_decision_ppp <- function(decision_df, distance_df){

  avg_similarity_score_df <- decision_df |>
    dplyr::count(paper1, paper2, sort = TRUE) |>
    dplyr::left_join(distance_df) |>
    dplyr::group_by(n) |>
    dplyr::summarize(similarity = mean(1 - dist))

  decision_df |>
    dplyr::count(paper1, paper2, sort = TRUE) |>
    dplyr::count(n) |>
    dplyr::mutate(prop = nn/sum(nn)) |>
    dplyr::left_join(avg_similarity_score_df) |>
    dplyr::rename(num_decision = n, count = nn)

}

#' View the coding detail for a pair of papers
#'
#' @param paper_df A data frame containing paper codings
#' @param decision_df A data frame containing item-level distances
#' @param pp1,pp2 A single string each to identify the two papers
#'
#' @returns
#' A data frame containing decision-level comparisons between the two papers.
#'
#' @export
#' @rdname diagnose
view_pairs <- function(paper_df, decision_df, pp1, pp2){
  res <- paper_df |>
    dplyr::filter(paper %in% c(pp1, pp2)) |>
    pivot_decision_tbl_longer() |>
    tidyr::pivot_wider(id_cols = -c(model, id), names_from = paper, values_from = reason) |>
    unnest(c(pp1, pp2)) |>
    select(-c(variable, type, parameter))

  score_df <- decision_df |>
    dplyr::filter((paper1 == pp1 & paper2 == pp2) | (paper1 == pp2 & paper2 == pp1)) |>
    dplyr::select(decision, dist)

  res |> dplyr::left_join(score_df)

}
