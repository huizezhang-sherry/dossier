#' Subset the decision table by the most common variable-type and the paper
#'
#' To ensure a reasonable computation on paper similarity, [filter_var_type()]
#' allows you to subset the decision table by the most
#' common variable/ type combinations among all papers. [filter_papers()]
#' allows you to subset the paper with at least x decisions or the top x papers
#' with the most decisions.
#'
#' @param df The decision table object
#' @param n Optional. A numeric value indicating the number of papers to keep.
#' @param n_value Optional. A numeric value indicating the minimum number of decisions a paper must have to be kept.
#'
#' @export
#' @rdname filter
#' @examples
#' raw_df <- read.csv(system.file("papers.csv", package = "dossier")) |> tibble::as_tibble()
#' tbl_df <- as_decision_tbl(raw_df)
#' df <- tbl_df |> filter_var_type(n = 6)
#' df2 <- tbl_df |> filter_var_type(n_value = 3)
#' identical(df, df2)
filter_papers <- function(df, n = NULL, n_value = NULL){
  verify_df_std(df)
  count_summary <- count_paper_decisions(df)

  if (is.null(n) && is.null(n_value)){
    n <- nrow(count_summary)
    cli::cli_alert_info("Using all {n} papers.")
    cli::cli_alert_info("Using {.fn summarize_num_decisions_pp} to choose {.arg n_paper} or {.arg n_count}.")
  }

  if (!is.null(n)) good_papers <- count_summary |> dplyr::slice_head(n = n)
  if (!is.null(n_value)) good_papers <- count_summary |> dplyr::filter(.n >= n_value)

  df |> dplyr::filter(paper %in% good_papers$paper)
}

#' @export
#' @rdname filter
filter_var_type <- function(df, n = NULL, n_value = NULL){
  verify_df_std(df)
  res <- df |> count_variable_type()

  if (is.null(n) && is.null(n_value)){
    n <- nrow(res)
    cli::cli_alert_info("Using all {n} variables.")
    cli::cli_alert_info("Using {.fn count_variable_type} to choose {.arg n} or {.arg n_value}.")
  }

  if (!is.null(n)) res <- res |> dplyr::slice_head(n = n)
  if (!is.null(n_value)) res <- res |> dplyr::filter(.n >= n_value)

  items <- res |>
    dplyr::transmute(id = paste0(variable, "_", type)) |>
    dplyr::pull(id)

  df |>
    dplyr::mutate(variable_type = paste0(variable, "_", type)) |>
    dplyr::filter(variable_type %in% items) |>
    dplyr::select(-variable_type)
}

#' Small functions to summarize the decision table for pre-processing
#'
#' @param df The decision table object.
#' @param sort A logical indicating whether to sort results by count in
#' descending order.Default to `TRUE`
#'
#' @returns
#' A data frame containing papers and their decision counts.
#'
#' @export
#' @rdname summarize
#' @examples
#' raw_df <- read.csv(system.file("papers.csv", package = "dossier")) |> tibble::as_tibble()
#' df <- as_decision_tbl(raw_df)
#' count_paper_decisions(df)
#' count_variable_type(df)
#' df |> filter_var_type(n = 6) |> count_paper_pair_decisions()
count_paper_decisions <- function(df, sort = TRUE){

  verify_df_std(df)
  res <- skim_variable_binary(df) |>
    dplyr::transmute(paper, .n = rowSums(dplyr::across(-c(paper, model))))

  if (sort) res <- res |> arrange(-.n)
  class(res) <- class(res)[-1]
  res
}

#' @export
#' @rdname summarize
count_paper_pair_decisions <- function(df){
  verify_df_std(df)
  df_long <- df |> pivot_decision_tbl_longer()
  res <- gen_paper_grid(df, "paper") |>
    dplyr::rowwise() |>
    dplyr::mutate(.n = length(intersect(
      df_long |> dplyr::filter(paper == paper1) |> dplyr::pull(decision),
      df_long |> dplyr::filter(paper == paper2) |> dplyr::pull(decision))
    )) |>
    dplyr::ungroup()

  class(res) <- class(res)[-1]
  res
}


#' @export
#' @rdname summarize
count_variable_type <- function(df, sort = TRUE){
  verify_df_std(df)
  res <- df |>
    dplyr::count(variable, type, sort = TRUE, name = ".n") |>
    dplyr::filter(variable != "model")

  class(res) <- class(res)[-1]
  res
}


#' @export
#' @rdname summarize
skim_variable_binary <- function(df){
  verify_df_std(df)
  res <- df |>
    pivot_decision_tbl_wider() |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(-c(paper, model), ~ifelse(is.na(.x), 0, 1))) |>
    dplyr::ungroup()

  class(res) <- class(res)[-1]
  res
}
