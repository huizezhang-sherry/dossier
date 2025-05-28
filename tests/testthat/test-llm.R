# test_that("multiplication works", {
#   purrr::walk(1:10, ~{
#     summarize_pdf(
#       prompt_file = here::here("inst/prompt.md"),
#       pdf = here::here("inst/burnett2004associations.pdf"),
#       llm_model = "claude",
#       seed = 123,
#       file = here::here(paste0("inst/burnett2004associations-", .x, ".md"))
#     )
#   })
#
#    purrr::walk(1:10, ~{
#     summarize_pdf(
#       prompt_file = here::here("inst/prompt.md"),
#       pdf = here::here("inst/burnett2004associations.pdf"),
#       llm_model = "gemini",
#       seed = 123, temperature = 0,
#       file = here::here(paste0("inst/burnett2004association-gemini-", .x, ".md"))
#     )
#   })
#
#    t1 <- Sys.time()
#    purrr::walk(1:10, ~{
#      summarize_pdf(
#        prompt_file = here::here("inst/prompt.md"),
#        pdf = here::here("inst/mate.pdf"),
#        llm_model = "gemini",
#        seed = 123, temperature = 0,
#        file = here::here(paste0("inst/mate-gemini-", .x, ".md"))
#      )
#    })
#    t2 <- Sys.time()
#
#    purrr::walk(1:10, ~{
#      summarize_pdf(
#        prompt_file = here::here("inst/prompt.md"),
#        pdf = here::here("inst/pope.pdf"),
#        llm_model = "gemini",
#        seed = 123, temperature = 0,
#        file = here::here(paste0("inst/pope1/pope-gemini-", .x, ".md"))
#      )
#    })
#
#    purrr::walk(1:10, ~{
#      summarize_pdf(
#        prompt_file = here::here("inst/prompt.md"),
#        pdf = here::here("inst/pope.pdf"),
#        llm_model = "gemini",
#        seed = 123, temperature = 0,
#        file = here::here(paste0("inst/pope2/pope-gemini-", .x, ".md"))
#      )
#    })
#
#    purrr::walk(1:10, ~{
#      summarize_pdf(
#        prompt_file = here::here("inst/prompt.md"),
#        pdf = here::here("inst/zan.pdf"),
#        llm_model = "gemini",
#        seed = 123, temperature = 0,
#        file = here::here(paste0("inst/zan-gemini-", .x, ".md"))
#      )
#    })
#
#    purrr::walk(1:10, ~{
#      summarize_pdf(
#        prompt_file = here::here("inst/prompt.md"),
#        pdf = here::here("inst/zan.pdf"),
#        llm_model = "gemini",
#        seed = 123, temperature = 0,
#        file = here::here(paste0("inst/zan2/zan-gemini-", .x, ".md"))
#      )
#    })
#
#
#    purrr::walk(1:10, ~{
#      summarize_pdf(
#        prompt_file = here::here("inst/prompt.md"),
#        pdf = here::here("inst/zan.pdf"),
#        llm_model = "gemini",
#        seed = 123, temperature = 0,
#        file = here::here(paste0("inst/zan3/zan-gemini-", .x, ".md"))
#      )
#    })
#
#
# # good ones:
#   res <- list.files(here::here("inst"), full.names = TRUE,
#                     pattern = "ostro2006effects-gemini-[0-9]") |>
#     purrr::map(~.x |> clean_md() |> arrange(variable))
#
#   # contamination from the results section
#   res <- list.files(here::here("inst"), full.names = TRUE,
#                     pattern = "burnett2004association-gemini-[0-9]") |>
#     purrr::map(~.x |> clean_md() |> arrange(variable))
#
#   ####################################################################################
#   res <- list.files(here::here("inst/pope1"), full.names = TRUE,
#                     pattern = "pope-gemini-[0-9]") |>
#     purrr::map(~.x |> clean_md() |> arrange(variable))
#
#   res2 <- list.files(here::here("inst/pope2"), full.names = TRUE,
#                     pattern = "pope-gemini-[0-9]") |>
#     purrr::map(~.x |> clean_md() |> arrange(variable))
#
#   ####################################################################################
#   res <- list.files(here::here("inst/zan1"), full.names = TRUE,
#                     pattern = "zan-gemini-[0-9]") |>
#     purrr::map(~.x |> clean_md() |> arrange(variable))
#
#   res2 <- list.files(here::here("inst/zan2"), full.names = TRUE,
#                     pattern = "zan-gemini-[0-9]") |>
#     purrr::map(~.x |> clean_md() |> arrange(variable))
#
#   res3 <- list.files(here::here("inst/zan3"), full.names = TRUE,
#                      pattern = "zan-gemini-[0-9]") |>
#     purrr::map(~.x |> clean_md() |> arrange(variable))
#
#
#   ####################################################################################
#   res <- list.files(here::here("inst"), full.names = TRUE,
#                     pattern = "linares2010short-gemini-[0-9]") |>
#     purrr::map(~.x |> clean_md() |> arrange(variable))
#
#
#   res <- list.files(here::here("inst"), full.names = TRUE,
#                     pattern = "mate-gemini-[0-9]") |>
#     purrr::map(~.x |> clean_md() |> arrange(variable))
#
#   res2 <- list.files(here::here("inst"), full.names = TRUE,
#                     pattern = "ostro2006effects-[0-9]") |>
#     purrr::map(~.x |> clean_md() |> arrange(variable, parameter))
#
# })
#
#
# dt <- 1:6 |> combn(2) |> t() |> as_tibble() |> rowwise() |> mutate(y = identical(res[[V1]], res[[V2]])) |>
#   ungroup()
#
# waldo::compare(res[[2]], res[[3]])
#
# library(dplyr)
# library(purrr)
#
# find_true_groups <- function(df) {
#   df_true <- df %>% filter(y) %>% select(V1, V2)
#
#   # Convert rows to list of numeric pairs
#   pair_list <- df_true %>%
#     mutate(id = row_number()) %>%
#     pivot_longer(cols = c(V1, V2), names_to = NULL, values_to = "V") %>%
#     group_by(id) %>%
#     summarise(Vs = list(sort(V))) %>%
#     pull(Vs)
#
#   groups <- list()
#
#   while (length(pair_list) > 0) {
#     current <- pair_list[[1]]
#     pair_list <- pair_list[-1]
#
#     changed <- TRUE
#     while (changed) {
#       changed <- FALSE
#       i <- 1
#       while (i <= length(pair_list)) {
#         pair <- pair_list[[i]]
#         if (length(intersect(current, pair)) > 0) {
#           current <- union(current, pair)
#           pair_list <- pair_list[-i]
#           changed <- TRUE
#         } else {
#           i <- i + 1
#         }
#       }
#     }
#
#     groups[[length(groups) + 1]] <- sort(current)
#   }
#
#   groups
# }
#
#
# find_true_groups(dt)
