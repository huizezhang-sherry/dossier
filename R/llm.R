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
#'
#' @returns
#' Writes output to the specified file.
#'
#' @export
#' @rdname llm
#' @seealso [clean_md]
summarize_pdf <- function(prompt_file, pdf, file = NULL, llm_model = "claude"){
  prompt <- ellmer::interpolate_file(prompt_file)

  if (llm_model == "claude") {
    chat <- ellmer::chat_claude()
  } else if (llm_model == "gemini") {
    chat <- ellmer::chat_gemini()
  } else {
    stop("Unsupported LLM model. Please use 'claude' or 'gemini'.")
  }

  if (is.null(file)) {
    file <- paste0(tools::file_path_sans_ext(pdf), ".md")
  }

  writeLines(
    chat$chat(prompt_file, ellmer::content_pdf_file(pdf)),
    con = file
  )

}

#' @rdname llm
#' @export
summarise_pdf <- summarize_pdf

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

  jsonlite::fromJSON(json_text)$decisions |> tibble::as_tibble()
}
