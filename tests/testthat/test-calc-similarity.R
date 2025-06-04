
# test_that("multiplication works", {
#   expect_snapshot(count_variable_type(raw_df))
#   df <- raw_df |> filter_var_type(n = 6) # first 6 variable-type pairs
#   good_papers <- pivot_variable_wider(df) |> slice_papers(n_count = 3)
#   paper_df <- pivot_decision_wider(df) |> filter(paper %in% good_papers$paper)
#
#   embed_df <- paper_df |> compute_text_embed()
#   distance_item_df <- calc_decision_similarity(paper_df, embed = embed_df)
#   distance_df <- distance_item_df |> calc_paper_similarity()
#   to_dist_mtx(distance_df)
#
# })



test_that("generate paper grid", {
  paper_vec <- c("braga", "katsouyanni", "ostro", "peel", "schwartz", "zanobetti")
  expect_snapshot(gen_paper_grid(paper_vec))

  set.seed(123)
  cols <- paper_vec |> combn(2) |> t()
  colnames(cols) <- c("V1", "V2")
  grid <- cols |>
    tibble::as_tibble() |>
    dplyr::slice_sample(prop = 0.9) |>
    dplyr::arrange(V1, V2)

  expect_snapshot(grid |> gen_paper_grid(cols = c("V1", "V2")))
  expect_snapshot(grid |> gen_paper_grid(cols = c("V1", "V2"), new_names = c("V1", "V2")))

})


test_that("workflow", {
  raw_df <- readr::read_csv(system.file("papers.csv", package = "dossier"))
  tbl_df <- as_decision_tbl(raw_df)
  expect_snapshot(tbl_df)

  expect_snapshot(count_variable_type(tbl_df))
  df <- tbl_df |> filter_var_type(n = 6)
  expect_snapshot(df)
  expect_snapshot(count_paper_decisions(df))
  paper_df <- df |> filter_papers(n_value = 3)
  expect_snapshot(paper_df)

  expect_snapshot(count_paper_pair_decisions(paper_df))
  embed_df <- compute_text_embed(paper_df)
  expect_snapshot(embed_df)

  # distance_decision_df <- calc_decision_similarity(paper_df, embed = embed_df)
  # expect_snapshot(distance_decision_df)
  # distance_decision_df2 <- calc_decision_similarity(paper_df, embed = embed_df, new_names = c("V1", "V2"))
  # expect_snapshot(distance_decision_df2)

})



