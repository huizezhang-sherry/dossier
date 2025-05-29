#' Summarize number of decisions per paper
#'
#' @param df A data frame.
#' @param df_wide A data frame.
#' @param sort Optional. A logical indicating whether to sort results by count in descending order.
#' @param n Optional. A numeric value.
#' @param n_value Optional. A numeric value.
#' @param n_paper Optional. A numeric value.
#' @param n_count Optional. A numeric value.

#' @returns
#' A data frame containing papers and their decision counts.
#'
#' @export
#' @rdname summarize
summarize_num_decisions_pp <- function(df, sort = TRUE){

  df_wide_var_type <- pivot_var_type_wider(df)
  vars <- colnames(df_wide_var_type)[-c(1,2)]
  res <- summarize_variable_binary(df_wide_var_type) |>
    dplyr::transmute(paper, count = rowSums(dplyr::across(vars)))

  if (sort) res <- res |> arrange(-count)
  return(res)
}


#' @export
#' @rdname summarize
filter_papers <- function(df, n_paper = NULL, n_count = NULL){
  count_summary <- summarize_num_decisions_pp(df)

  if (is.null(n_paper) && is.null(n_count)){
    n_paper <- nrow(count_summary)
    cli::cli_alert_info("Using all {n_paper} papers.")
    cli::cli_alert_info("Using {.fn summarize_num_decisions_pp} to choose {.arg n_paper} or {.arg n_count}.")
  }

  if (!is.null(n_paper)) good_papers <- count_summary |> dplyr::slice_head(n = n_paper)
  if (!is.null(n_count)) good_papers <- count_summary |> dplyr::filter(count >= n_count)

  df |> dplyr::filter(paper %in% good_papers$paper)
}

#' @param paper_df A data frame containing paper codings
#' @export
#' @rdname summarize
summarize_decisions_ppp <- function(paper_df){
  df <- paper_df |> pivot_decision_tbl_longer()
  gen_paper_grid(paper_df, "paper") |>
    dplyr::rowwise() |>
    dplyr::mutate(pairs = length(intersect(
      df |> dplyr::filter(paper == paper1) |> dplyr::pull(decision),
      df |> dplyr::filter(paper == paper2) |> dplyr::pull(decision))
    )) |>
    dplyr::ungroup()
}



#' @export
#' @rdname summarize
filter_var_type <- function(df, n = NULL, n_value = NULL){

  items <- create_good_variables(df, n = n, n_value = n_value)
  df |>
    dplyr::mutate(variable_type = paste0(variable, "_", type)) |>
    dplyr::filter(variable_type %in% items) |>
    dplyr::select(-variable_type)
}

#' @export
#' @rdname summarize
count_variable_type <- function(df){
  df |>
    dplyr::count(variable, type, sort = TRUE) |>
    dplyr::filter(variable != "model")
}

#' @export
#' @rdname summarize
create_good_variables <- function(df, n = NULL, n_value = NULL){
  res <- df |> count_variable_type()

  if (is.null(n) && is.null(n_value)){
    n <- nrow(res)
    cli::cli_alert_info("Using all {n} variables.")
    cli::cli_alert_info("Using {.fn count_variable_type} to choose {.arg n} or {.arg n_value}.")
  }

  if (!is.null(n)) res <- res |> dplyr::slice_head(n = n)
  if (!is.null(n_value)) res <- res |> dplyr::filter(n >= n_value)

  res |>
    dplyr::transmute(id = paste0(variable, "_", type)) |>
    dplyr::pull(id)
}

#' @export
#' @rdname summarize
summarize_variable_binary <- function(df_wide){
  # only take wide format for now
  df_wide |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(colnames(df_wide)[-c(1,2)], ~ifelse(is.na(.x), 0, 1))) |>
    dplyr::ungroup()
}
