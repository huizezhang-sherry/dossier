% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dim-red.R
\name{to_dist_mtx}
\alias{to_dist_mtx}
\alias{run_hclust}
\alias{run_mds}
\title{Wrapper for hierarchical clustering and Multi-Dimensional Scaling (MDS)}
\usage{
to_dist_mtx(df, paper_cols = c("paper1", "paper2"))

run_hclust(df, method = "ave")

run_mds(df, paper_cols = c("paper1", "paper2"))
}
\arguments{
\item{df}{A dataframe.}

\item{paper_cols}{Optional. A character vector of length 2 specifying the column names for the papers being compared.}

\item{method}{see [hclust] for details.}
}
\value{
A distance matrix object of class `"dist"`.
}
\description{
Wrapper for hierarchical clustering and Multi-Dimensional Scaling (MDS)
}
