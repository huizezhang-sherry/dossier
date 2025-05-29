#' Summarize PDF via LLM
#'
#' The output from LLM may contains additional notes about the process,
#' unexpected line breaks, etc. We recommend capturing the output in a markdown
#' file and instruct the LLM to write the output in a json block. Use the function
#' [clean_md] to extract the decisions from the JSON block.
#'
#' @param prompt_file A single string containing the prompt
#' @param pdf A single string specifying the path to a PDF file
#' @param file A single string specifying where to save the output. By default,
#' it saves a markdown file with the same name as the PDF file in the same directory.
#' @param llm_model One of `"claude"` or `"gemini"` for processing pdf documents. Default to `"claude"`
#' @param ... other arguments supplied to the LLM chat function, such as `seed`, `temperature`, etc.
#'
#' @returns
#' Writes output to the specified file.
#'
#' @export
#' @rdname llm
#' @seealso [clean_md]
extract_decisions <- function(prompt_file, pdf, file = NULL, ..., llm_model = "claude"){
  args <- rlang::list2(...)

  prompt <- ellmer::interpolate_file(prompt_file)

  if (llm_model == "claude") {
    chat <- do.call("chat_anthropic", args = list(params = args))
  } else if (llm_model == "gemini") {
    chat <- do.call("chat_google_gemini", args = list(params = args))
  } else {
    stop("Unsupported LLM model. Please use 'claude' or 'gemini'.")
  }

  if (is.null(file)) {
    file <- paste0(tools::file_path_sans_ext(pdf), ".md")
  }

  writeLines(
    chat$chat(prompt, ellmer::content_pdf_file(pdf)),
    con = file
  )

}

#' Deprecated functions
#' @description
#' `r lifecycle::badge("deprecated")`
#' * [summarize_pdf()]/ [summarise_pdf()] are renamed to [extract_decisions()].
#' @keywords internal
#' @rdname deprecated
#' @export
summarize_pdf <- function(...) {
  lifecycle::deprecate_warn("0.1.1", "summarize_pdf()", "extract_decisions()")
  extract_decisions(...)
}

#' @rdname deprecated
#' @export
summarize_pdf <- function(...) {
  lifecycle::deprecate_warn("0.1.1", "summarize_pdf()", "extract_decisions()")
  extract_decisions(...)
}


#' Extract JSON decisions from the markdown file
#'
#' @description
#' Extract the json block from the markdown file and convert it to a tibble.
#'
#' @param file A single string containing a path to a JSON file
#'
#' @returns
#' A tibble containing the "decisions" field from the JSON data.
#'
#' @export
clean_md <- function(file){
  lines <- readLines(file)
  start <- which(grepl("^```json", lines))
  end <- which(grepl("^```", lines))[-1]
  json_lines <- lines[(start + 1):(end[1] - 1)]
  json_text <- paste(json_lines, collapse = "\n")

  # remove no-ASCII, newlines before letters, and replace NA with "NA"
  json_text <- gsub("[^\x01-\x7F]", "", json_text)
  json_text <- gsub("\n(?=[A-Za-z(0-9])", "", json_text, perl = TRUE)
  json_text <- gsub("NA,", '"NA",', json_text)

  res <- jsonlite::fromJSON(json_text)$decisions |> tibble::as_tibble()
  res <- tibble::tibble(paper = tools::file_path_sans_ext(basename(file))) |>
    dplyr::bind_cols(res)

  class(res) <- "decision_tbl"
  attr(res, "form") <- "std"
  res
}

#' Create the decision table object
#'
#' @description
#' Extract the json block from the markdown file and convert it to a tibble.
#'
#' @param df A data frame or tibble to be casted into a decision table object
#'
#' @returns
#' A tibble of class decision_tbl
#'
#' @export
#' @rdname class
#' @examples
#' raw_df <- read.csv(system.file("papers.csv", package = "dossier")) |> tibble::as_tibble()
#' to_decision_tbl(raw_df)
to_decision_tbl <- function(df){

  new_decision_tbl(df, form = "std")
}


new_decision_tbl <- function(df, ...){

  args <- rlang::list2(...)

  res <- tibble::new_tibble(df, !!!args)
  class(res) <- c("decision_tbl", class(df))
  res
}


check_df_std <- function(df){

  check_decision_tbl(df)
  if (attr(df, "form") != "std"){
    cli::cli_abort("A {.field std} format of the decision table is required. ")
  }
}

check_df_var_type_wide <- function(df){

  check_decision_tbl(df)
  if (attr(df, "form") != "var_type_wide"){
    cli::cli_abort("A {.field wide} format of the decision table is required.
                   Consider using {.fn pivot_var_type_wider} to reshape the decision table.")
  }
}


check_df_tbl_wide <- function(df){

  check_decision_tbl(df)
  if (attr(df, "form") != "tbl_wide"){
    cli::cli_abort("A {.field wide} format of the decision table is required.
                   Consider using {.fn pivot_decision_tbl_wider} to reshape the decision table.")
  }
}



check_df_long <- function(df){

  check_decision_tbl(df)
  if (attr(df, "form") != "long"){
    cli::cli_abort("A {.field long} format of the decision table is required.
                   Consider using {.fn pivot_decision_tbl_longer} to reshape the decision table.")
  }
}


check_decision_tbl <- function(df){
  if (!inherits(df, "decision_tbl")){
    cli::cli_abort("An {.field decision_tbl} object is required as the input, please check")
  }
}
