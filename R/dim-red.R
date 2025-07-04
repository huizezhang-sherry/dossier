#' Wrapper for hierarchical clustering and Multi-Dimensional Scaling (MDS)
#'
#' @param df A dataframe.
#' @param paper_cols Optional. A character vector of length 2 specifying the column names for the papers being compared.
#' @param method see [hclust] for details.
#'
#' @returns
#' A distance matrix object of class `"dist"`.
#'
#' @export
#' @rdname dim-red
to_dist_mtx <- function(df, paper_cols = c("paper1", "paper2")){
  papers <- unname(sapply(paper_cols, function(col) unique(as.character(df[[col]])), simplify = TRUE))
  papers <- c(papers[,1], utils::tail(papers[,2], 1))
  dist_m <- df$dist
  class(dist_m) <- "dist"
  attr(dist_m, "Size") <- length(papers)
  attr(dist_m, "Labels") <- papers
  attr(dist_m, "Dia") <- FALSE
  attr(dist_m, "Upper") <- FALSE
  dist_m
}

#' @export
#' @rdname dim-red
run_hclust <- function(df, method = "ave"){
  dist_m <- to_dist_mtx(df)
  hclust_res <- stats::hclust(dist_m, method = method)

  if (!requireNamespace("ggdendro")) {
    cli::cli_abort(
      "The {.pkg ggdendro} package is required for dendrogram plotting. \\
  Please install it with {.code install.packages('ggdendro')}."
    )
  } else{
    ggdendro::dendro_data(hclust_res)
  }

}

#' @export
#' @rdname dim-red
run_mds <- function(df, paper_cols = c("paper1", "paper2")){
  cmod <- stats::cmdscale(to_dist_mtx(distance_df))
  tibble::tibble(paper = rownames(cmod), V1 = cmod[,1], V2 = cmod[,2])

}

