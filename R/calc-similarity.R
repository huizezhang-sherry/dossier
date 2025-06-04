#' Calculate decision and paper similarity score
#'
#' The `calc_decision_similarity` function presents text similarity score for
#' matched decisions for each paper pair.`calc_paper_similarity` aggregates all
#' the decision scores to produce similarity score for paper pairs.
#'
#' @param df A decision table object
#' @param embed Optional. A text embedding object from `compute_text_embed()`.
#' @param text_model A text model.See `text::textEmbed()`
#' @param new_names A character vector of length 2. The column names for the
#' paper pairs. Default to `c("paper1", "paper2")`
#' @param .f Optional. A function to aggregate decision similarity score to
#' paper similarity score, defaults to `mean`.
#' @param res The output from `calc_decision_similarity()`
#' @returns
#' A tibble object
#' @export
#' @rdname calc-similarity
#' @examples
#'\dontrun{
#' library(text)
#' library(readr)
#' # preprocessing
#' tbl_df <- readr::read_csv(system.file("papers.csv", package = "dossier")) |>
#'   as_decision_tbl()
#' paper_df <- tbl_df |>
#'   filter_var_type(n = 6) |> # first 6 variable-type pairs
#'   filter_papers(n_value = 3)
#'
#' # calculate the text embed
#' embed_df <- paper_df |> compute_text_embed()
#' # calculate decision similarity
#' distance_decision_df <- calc_decision_similarity(paper_df, embed = embed_df)
#' # aggregate from decision similarity to paper similarity
#' distance_df <- distance_decision_df |> calc_paper_similarity()
#'}

calc_decision_similarity <- function(df, embed = NULL, text_model = "bert-base-uncased",
                                     new_names = c("paper1", "paper2")){

  verify_df_std(df)
  long_df <- pivot_decision_tbl_longer(df)

  if (is.null(embed)) embed <- text::textEmbed(long_df$reason, model = text_model)

  res_df <- gen_paper_grid(df, "paper", new_names = new_names)
  pp_cols <- attr(res_df, "pp_cols")
  res_df <- mapply(
    function(x, y) decision_similarity_workhorse(x, y, long_df, embed, pp_cols),
    res_df[[pp_cols[[1]]]], res_df[[pp_cols[[2]]]] , SIMPLIFY = FALSE
    )

  res <- dplyr::bind_rows(res_df)
  res

}

#' @export
#' @rdname calc-similarity
compute_text_embed <- function(df, text_model ="bert-base-uncased"){
  verify_df_std(df)
  long_df <- pivot_decision_tbl_longer(df)
  embed <- text::textEmbed(long_df$reason, model = text_model)
  return(embed)
}


#' @keywords internal
decision_similarity_workhorse <- function(paper1, paper2, long_df, embed, pp_cols){
    aa_split <- long_df |>
      dplyr::filter(paper %in% c(paper1, paper2)) |>
      dplyr::group_split(variable, type, decision)
    aa_split_good <- lapply(aa_split, function(x){if(length(unique(x$paper)) != 1){x}})
    aa_split_good <- aa_split_good[!vapply(aa_split_good, is.null, logical(1))]
    aa <- lapply(aa_split_good, function(x) x[["id"]])
    aa <- lapply(aa, function(x) if (length(x) == 2) x else NULL) # currently only compare groups with two matches
    aa <- aa[!sapply(aa, is.null)]

    bb <- aa[!vapply(aa, function(x) length(x) == 0, logical(1))]

    aa_df <- lapply(aa_split_good, function(x) if (nrow(x) == 2) x else NULL)
    aa_df <- aa_df[!sapply(aa_df, is.null)]

    type <- lapply(aa_df, function(x) {
      items <- unique(x[["decision"]])
      matched <- regmatches(items, regexpr("method|reason|decision", items))
      matched
    })

    res1 <- process_reasons(df = aa_df[which(type == "method")], type = "method", embed = embed)
    res2 <- process_reasons(df = aa_df[which(type != "method")], type = "others", embed = embed)
    res <- dplyr::bind_rows(res1, res2)
    if (nrow(res) == 0) res <- tibble(id1 = NA, id2 = NA, dist = NA)

    res <- res |>
      dplyr::left_join(long_df |>
                         dplyr::select(paper, decision, id) |>
                         dplyr::rename(!!pp_cols[[1]] := paper),
                by = c("id1" = "id")) |>
      dplyr::left_join(long_df |>
                         dplyr::select(paper, id) |>
                         dplyr::rename(!!pp_cols[[2]] := paper),
                by = c("id2" = "id")) |>
      dplyr::select(!!pp_cols, decision, dist) |>
      dplyr::filter(!is.na(dist))

    class(res) <- c("paper_grid_df", class(res))
    attr(res, "pp_cols") <- pp_cols
    return(res)
}

#' @keywords internal
process_reasons <- function(df, type, embed = NULL){
  switch(type,
         method = purrr::map_dfr(df, process_reason_method),
         others = purrr::map_dfr(df, ~process_reason_others(.x, embed))
         )
}

#' @keywords internal
process_reason_method <- function(df){
  rea1 <- df$reason[1]; rea2 <- df$reason[2]
  # here reason is the general column
  tibble::tibble(id1 = df$id[1], id2 = df$id[2],
         dist = as.numeric(identical(rea1, rea2)))
}

#' @keywords internal
process_reason_others <- function(df, embed){
  tibble::tibble(
    id1 = df$id[1], id2 = df$id[2],
    dist = text::textSimilarity(embed$texts$texts[df$id[1], ],
                                embed$texts$texts[df$id[2], ]))
}

#' @export
#' @rdname calc-similarity
calc_paper_similarity <- function(res, .f = mean) {

  verify_df_pp_grid(res)
  pp_cols <- attr(res, "pp_cols")
  comparison_long <- res |>
    dplyr::group_by(!!!syms(pp_cols)) |>
    dplyr::summarise(dist = 1 - do.call(.f, list(dist))) |>
    dplyr::ungroup()

  comparison_long |>
    gen_paper_grid(cols = pp_cols) |>
    dplyr::mutate(id = paste0(!!sym(pp_cols[1]), "-", !!sym(pp_cols[2]))) |>
    dplyr::left_join(comparison_long |>
                       dplyr::mutate(id = paste0(!!sym(pp_cols[1]), "-", !!sym(pp_cols[2]))) |>
                       dplyr::select(id, dist), by = "id") |>
    dplyr::left_join(comparison_long |>
                       dplyr::mutate(id2 = paste0(!!sym(pp_cols[2]), "-", !!sym(pp_cols[1]))) |>
                       dplyr::select(id2, dist), by = c("id" = "id2")) |>
    dplyr::mutate(dist = dplyr::coalesce(dist.x, dist.y)) |>
    dplyr::mutate(dist = ifelse(is.na(dist), 1, dist)) |>
    dplyr::mutate(dplyr::across(pp_cols, as.factor)
    ) |>
    dplyr::select(-c(id, dist.x, dist.y)) |>
    dplyr::mutate(similarity = 1 - dist)
}


globalVariables(c("paper", "model", "variable", "method", "parameter",
                  "type", "reason", "decision", "id", "paper1", "paper2",
                  "dist", "id2", "dist.x", "dist.y", "item", "embed_df",
                  "distance_decision_df", "count", "a", "variable_type",
                  "n", "df", "nn", "ave_similarity_score_df", "dist_df", ".n",
                  "distance_df"))
