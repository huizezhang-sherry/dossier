#' Convert the decision table into long or wide format
#'
#' `pivot_decision_tbl_wider`, pivots the decision table into a wider form with one
#' paper per row and each column as a decision. `pivot_decision_tbl_longer`
#' pivots all the columns, except paper and model, to the long format.
#'
#' @param df A decision_tbl object created by `as_decision_tbl`
#'
#' @returns
#' A data frame in wide format with decisions split across columns.
#'
#' @export
#' @rdname decision-table
#' @examples
#' raw_df <- read.csv(system.file("papers.csv", package = "dossier")) |> tibble::as_tibble()
#' tbl_df <- as_decision_tbl(raw_df)
#' pivot_decision_tbl_wider(tbl_df)
#' pivot_decision_tbl_longer(tbl_df)
pivot_decision_tbl_wider <- function(df){

  verify_decision_tbl(df)
  df <- df |> dplyr::mutate(variable_type = paste0(variable, "_", type))
  res <- df |>
    tidyr::pivot_wider(id_cols = -c(parameter, variable_type),
                       names_from =  c(variable, type),
                       names_glue = "{variable}_{type}_{.value}",
                       names_vary = "slowest",
                       values_from = c(method, reason, decision))

  items <- df |> dplyr::pull(variable_type)
  res <- res |> dplyr::select(-paste0(items[grepl("spatial|temporal", items)], "_method"))
  res <- new_decision_tbl(res, form = "tbl_wide")
  res
}


#' @export
#' @rdname decision-table
pivot_decision_tbl_longer <- function(df){
  verify_df_std(df)

  res <- df |>
    dplyr::mutate(dplyr::across(-c(paper, model), as.character)) |>
    tidyr::pivot_longer(-c(paper, model, variable, type, parameter),
                        names_to = "decision", values_to = "reason") |>
    dplyr::mutate(decision = paste0(variable, "_", type, "_", decision)) |>
    dplyr::filter(!is.na(reason)) |>
    dplyr::mutate(id = dplyr::row_number())
  res <- new_decision_tbl(res, form = "long")
  res
}

#' @keywords internal
pivot_var_type_wider <- function(df){

  verify_decision_tbl(df)
  res <- df |>
    dplyr::mutate(variable_type = paste0(variable, "_", type)) |>
    dplyr::mutate(a = paste0(reason, decision)) |>
    tidyr::pivot_wider(
      id_cols = -c(parameter, variable_type, reason, decision, method),
      names_from = c(variable, type),
      names_glue = "{variable}_{type}",
      names_vary = "slowest",
      values_from = c(a)
    )
  res <- new_decision_tbl(res, form = "var_type_wide")
  res
}


#' Generate pairwise grid for papers
#'
#' @param x A character vector of paper names or a decision_tbl object.
#' @param cols A character vector of column names to use for generating the grid. Used if a `decision_tbl` is provided.
#' @param ... Optional.
#' @param new_names A character vector of length 2 to name the resulting columns in the grid.
#' @returns
#' A tibble with two columns containing unique pairs of paper names.
#' @export
#' @rdname grid
#' @examples
#' paper_vec <- c("braga", "katsouyanni", "ostro", "peel", "schwartz", "zanobetti")
#' gen_paper_grid(paper_vec)
gen_paper_grid <- function(x, cols, new_names = c("paper1", "paper2"), ...){
  UseMethod("gen_paper_grid")
}

#' @export
#' @rdname grid
gen_paper_grid.character <- function(x, cols, new_names = c("paper1", "paper2"), ...){
  res <- c(x) |> unique() |> utils::combn(2) |> t() |>
    tibble::as_tibble(.name_repair = "minimal")

  if (length(new_names) != 2) {
    cli::cli_abort("The {.arg new_names} argument must have length 2, not {new_names}.")
  }
  colnames(res) <- new_names
  class(res) <- c("paper_grid_df", class(res))
  attr(res, "pp_cols") <- new_names
  return(res)
}

#' @export
#' @rdname grid
gen_paper_grid.tbl_df <- function(x, cols, new_names = c("paper1", "paper2"), ...){

  # could input two columns
  vec <- c(unname(sapply(cols, function(col) unique(as.character(x[[col]])), simplify = TRUE)))

  gen_paper_grid.character(vec, new_names = new_names)
}

verify_df_pp_grid <- function(x){
  if (!inherits(x, "paper_grid_df")){
    cli::cli_abort("An {.field paper_grid_df} object is required as the input, please check.")
  }
}
