#' Calculate item similarity
#'
#' @param df A data frame.
#' @param embed Optional. A text embedding.
#' @param text_model A text model.
#' @param ... Additional arguments passed to `text::textEmbed()`.
#'
#' @returns
#' A list of similarity scores between pairs of items.
#'
#' @export
#' @rdname calc-similarity
calc_decision_similarity <- function(df, embed = NULL, text_model = "bert-base-uncased", ...){
  long_df <- pivot_decision_longer(df)

  if (is.null(embed)) {
    embed <- text::textEmbed(long_df$reason, model = text_model, ...)
    }
  res_df <- gen_pairwise_paper_grid(df, "paper")
  res_df <- mapply(
    function(x, y) decision_similarity_workhorse(x, y, long_df, embed),
    res_df[["paper1"]], res_df[["paper2"]] , SIMPLIFY = FALSE
    )

  dplyr::bind_rows(res_df)

}

#' @export
#' @rdname calc-similarity
compute_text_embed <- function(df, text_model ="bert-base-uncased", ...){
  long_df <- pivot_decision_longer(df)
  embed <- text::textEmbed(long_df$reason, model = text_model, ...)
  return(embed)
}


#' @keywords internal
decision_similarity_workhorse <- function(paper1, paper2, long_df, embed){
    aa_split <- long_df |>
      dplyr::filter(paper %in% c(paper1, paper2)) |>
      dplyr::group_split(item)
    aa_split_good <- lapply(aa_split, function(x){if(length(unique(x$paper)) != 1){x}})
    aa_split_good <- aa_split_good[!vapply(aa_split_good, is.null, logical(1))]
    aa <- lapply(aa_split_good, function(x) x[["id"]])
    aa <- lapply(aa, function(x) if (length(x) == 2) x else NULL) # currently only compare groups with two matches
    aa <- aa[!sapply(aa, is.null)]

    bb <- aa[!vapply(aa, function(x) length(x) == 0, logical(1))]

    aa_df <- lapply(aa_split_good, function(x) if (nrow(x) == 2) x else NULL)
    aa_df <- aa_df[!sapply(aa_df, is.null)]

    type <- lapply(aa_df, function(x) {
      items <- unique(x[["item"]])
      matched <- regmatches(items, regexpr("method|reason|decision", items))
      matched
    })

    res1 <- process_reasons(df = aa_df[which(type == "method")], type = "method", embed = embed_df)
    res2 <- process_reasons(df = aa_df[which(type != "method")], type = "others", embed = embed_df)
    res <- dplyr::bind_rows(res1, res2)
    if (nrow(res) == 0) res <- tibble(id1 = NA, id2 = NA, dist = NA)

    res |>
      dplyr::left_join(long_df |>
                         dplyr::select(paper, item, id) |>
                         dplyr::rename(paper1 = paper),
                by = c("id1" = "id")) |>
      dplyr::left_join(long_df |>
                         dplyr::select(paper, id) |>
                         dplyr::rename(paper2 = paper),
                by = c("id2" = "id")) |>
      dplyr::select(paper1, paper2, item, dist) |>
      dplyr::filter(!is.na(dist))
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

#' Calculate paper similarity scores
#'
#' @param res A data frame containing paper comparison results with columns `paper1`, `paper2`, and `dist`.
#' @param .f Optional. A function to aggregate distances, defaults to `mean`.
#'
#' @returns
#' A data frame containing pairwise paper similarity scores, with columns `paper1`,
#' `paper2`, and `dist`. Papers with no comparison data are assigned a distance of 1.
#'
#' @export
#' @rdname calc-similarity
calc_paper_similarity <- function(res, .f = mean) {
  comparison_long <- res |>
    dplyr::group_by(paper1, paper2) |>
    dplyr::summarise(dist = 1 - do.call(.f, list(dist))) |>
    dplyr::ungroup()

  papers <- as.vector(unique(c(res$paper1, res$paper2)))
  gen_pairwise_paper_grid(distance_item_df, paper_cols = c("paper1", "paper2")) |>
    dplyr::mutate(id = paste0(paper1, "-", paper2)) |>
    dplyr::left_join(comparison_long |>
                       mutate(id = paste0(paper1, "-", paper2)) |>
                       select(id, dist), by = "id") |>
    dplyr::left_join(comparison_long |>
                       mutate(id2 = paste0(paper2, "-", paper1)) |>
                       select(id2, dist), by = c("id" = "id2")) |>
    dplyr::mutate(dist = dplyr::coalesce(dist.x, dist.y)) |>
    dplyr::mutate(dist = ifelse(is.na(dist), 1, dist)) |>
    dplyr::mutate(dplyr::across(paper1:paper2, as.factor),
           paper1 = factor(paper1, levels = papers),
           paper2 = factor(paper2, levels = papers)
    ) |>
    dplyr::select(-c(id, dist.x, dist.y)) |>
    dplyr::mutate(similarity = 1 - dist)
}

#' Convert distance vector to distance matrix
#'
#' @param distance_df A data frame containing a `dist` column.
#' @param paper_df A data frame containing a `paper` column.
#'
#' @returns
#' A distance matrix of class `"dist"` with labels from `paper_df$paper`.
#'
#' @export
to_dist_mtx <- function(distance_df, paper_df){
  dist_m <- dist_df$dist
  papers <- paper_df$paper
  class(dist_m) <- "dist"
  attr(dist_m, "Size") <- length(papers)
  attr(dist_m, "Labels") <- papers
  attr(dist_m, "Dia") <- FALSE
  attr(dist_m, "Upper") <- FALSE
  dist_m
}


globalVariables(c("paper", "model", "variable", "method", "parameter",
                  "type", "reason", "decision", "id", "paper1", "paper2",
                  "dist", "id2", "dist.x", "dist.y", "item", "embed_df",
                  "distance_item_df", "count", "a", "variable_type",
                  "n", "df", "nn", "ave_similarity_score_df", "dist_df"))
