test_that("multiplication works", {
  raw_df <- read.csv(system.file("papers.csv", package = "dossier")) |> tibble::as_tibble()
  df <- to_decision_tbl(raw_df)

  expect_equal(class(df)[1], "decision_tbl")
  expect_equal(class(pivot_var_type_wider(df))[1], "decision_tbl")
  expect_equal(class(filter_var_type(df, n = 6))[1], "decision_tbl")
  expect_equal(class(pivot_decision_tbl_wider(df))[1], "decision_tbl")
  expect_equal(class(pivot_decision_tbl_longer(df))[1], "decision_tbl")


})
