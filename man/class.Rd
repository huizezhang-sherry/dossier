% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llm.R
\name{as_decision_tbl}
\alias{as_decision_tbl}
\alias{as_decision_tbl.character}
\alias{as_decision_tbl.tbl_df}
\title{Create the decision table object}
\usage{
as_decision_tbl(x)

\method{as_decision_tbl}{character}(x)

\method{as_decision_tbl}{tbl_df}(x)
}
\arguments{
\item{x}{The input to be casted into a decision table object}
}
\value{
A tibble of class decision_tbl
}
\description{
The function takes an input of a character string (path to the markdown file
generated by [extract_decisions()]) or a data frame/tibble and returns a
decision table object
}
\examples{
raw_df <- read.csv(system.file("papers.csv", package = "dossier")) |> tibble::as_tibble()
as_decision_tbl(raw_df)
as_decision_tbl(system.file("braga.md", package = "dossier"))
}
