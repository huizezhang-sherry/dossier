#' Convert decision data to wide format
#'
#' @param df A data frame.
#'
#' @returns
#' A data frame in wide format with decisions split across columns.
#'
#' @export
#' @rdname decision-table
pivot_decision_wider <- function(df){

  res <- df |>
    dplyr::mutate(variable_type = paste0(variable, "_", type)) |>
    tidyr::pivot_wider(id_cols = -c(parameter, variable_type),
                       names_from =  c(variable, type),
                       names_glue = "{variable}_{type}_{.value}",
                       names_vary = "slowest",
                       values_from = c(method, reason, decision))

  items <- df |> dplyr::pull(variable_type)
  res |>
    dplyr::select(-paste0(items[grepl("spatial|temporal", items)], "_method"))
}

#' @export
#' @rdname decision-table
pivot_variable_wider <- function(df){

  res <- df |>
    dplyr::mutate(variable_type = paste0(variable, "_", type))

  res |>
    dplyr::mutate(a = paste0(reason, decision)) |>
    tidyr::pivot_wider(
      id_cols = -c(parameter, variable_type, reason, decision, method),
      names_from = c(variable, type),
      names_glue = "{variable}_{type}",
      names_vary = "slowest",
      values_from = c(a)
    )
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


#' Convert decisions to long format
#'
#' @param df A data frame containing columns 'paper', 'model', and additional columns to pivot.
#'
#' @returns
#' A tibble in long format with columns 'paper', 'model', 'item', 'reason', and 'id'.
#'
#' @export
#' @rdname decision-table
pivot_decision_longer <- function(df){
  df |>
    tidyr::pivot_longer(-c(paper, model), names_to = "item", values_to = "reason") |>
    tidyr::unnest(reason) |>
    dplyr::filter(!is.na(reason)) |>
    dplyr::mutate(id = dplyr::row_number())
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
summarize_num_decisions_pp <- function(df_wide, sort = TRUE){

  vars <- colnames(df_wide)[-c(1,2)]
  res <- summarize_variable_binary(df_wide) |>
    dplyr::transmute(paper, count = rowSums(dplyr::across(vars)))

  if (sort) res <- res |> arrange(-count)
  return(res)
}


#' @export
#' @rdname summarize
slice_papers <- function(df, n_paper = NULL, n_count = NULL){
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


#' @export
#' @rdname summarize
filter_variable_type <- function(df, n = NULL, n_value = NULL){

  items <- create_good_variables(df, n = n, n_value = n_value)
  df |>
    dplyr::mutate(variable_type = paste0(variable, "_", type)) |>
    dplyr::filter(variable_type %in% items)
}


#' @keywords internal
gen_pairwise_paper_grid <- function(df, paper_cols, colnames = c("paper1", "paper2")){

  vec <- unname(sapply(paper_cols, function(col) unique(as.character(df[[col]])), simplify = TRUE))
  res <- c(vec) |> unique() |> utils::combn(2) |> t() |> tibble::as_tibble(.name_repair = "minimal")

  if (length(colnames) != 2){
    cli::cli_abort("The {.arg colnames} argument must have length 2, not {length(colnames)}.")
  }
  colnames(res) <- colnames
  return(res)
}
