#' Diagnostics and inspection functions for paper similarity calculation
#'
#' @param decision_df Output from `calc_decision_similarity`
#' @param distance_df Output from `calc_paper_similarity`
#'
#' @returns
#' Diagnostic summary
#'
#' @export
#' @rdname diagnose
diag_decision_ppp <- function(decision_df, distance_df){

  pp_cols <- attr(distance_df, "pp_cols")
  avg_similarity_score_df <- decision_df |>
    dplyr::count(!!!syms(pp_cols), sort = TRUE) |>
    dplyr::left_join(distance_df, by = pp_cols) |>
    dplyr::group_by(n) |>
    dplyr::summarize(similarity = mean(1 - dist))

  decision_df |>
    dplyr::count(!!!syms(pp_cols), sort = TRUE) |>
    dplyr::count(n, name = "nn") |>
    dplyr::mutate(prop = nn/sum(nn)) |>
    dplyr::left_join(avg_similarity_score_df, by = "n") |>
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
  # TODO verify pp1 and pp2 are in the grid
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
