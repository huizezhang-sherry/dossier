# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
#
#
# library(tidyverse)
# raw_df <- read_csv(file.choose()) |>
#   # 1) standardize model names (and remove case crossover and distributed lag models)
#   mutate(model = ifelse(paper == "chock2000study", "Poisson generalized additive model", model)) %>%
#   mutate(model = ifelse(paper == "fairley1999daily", "generalized additive model", model)) %>%
#   mutate(model = ifelse(paper %in% c("peters2000associations", "cifuentes2011effect"),
#                         "Poisson generalized additive model", model)) |>
#   mutate(model = ifelse(paper == "venners2003particulate", "Poisson regression", model)) |>
#   mutate(model = ifelse(paper %in% c("jimenez2010role"), "Poisson generalized linear model", model)) |>
#   filter(!model %in% c("case-crossover design", "unconstrained distributed lag model",
#                        "regression tree", "lag distributed model", "unconstrained distributed lag models"))|>
#   # 2) no method or parameter for type spatial/temporal
#   mutate(method = ifelse(type %in% c("spatial", "temporal"), NA, method),
#          parameter = ifelse(type %in% c("spatial", "temporal"), NA, parameter)) |>
#   # 5) misc pre-processing
#   mutate(variable = str_replace_all(variable, " ", "_"),
#          variable = str_replace_all(variable, "dew-point", "dewpoint"),
#          variable = str_replace_all(variable, "dew_point", "dewpoint"),
#          decision = str_replace(decision, "degrees of freedom", ""),
#          decision = str_replace(decision, "df", "")) |>
#   # 3) group variables
#   mutate(variable = ifelse(variable %in% c("pollutants", "air_pollutants",
#                                            "particulate_matter", "exposure",
#                                            "particulate_concentrations",
#                                            "air_pollutants_variables", "pollutant",
#                                            "pollution", "air_pollution_variables"),
#                            "PM", variable),
#          variable = ifelse(str_detect(variable, "PM"), "PM", variable),
#          variable = ifelse(variable %in% c("temperature", "dewpoint temperature", "ambient_temperature",
#                                            "dewpoint_temperature", "minimum_temperature",
#                                            "maximum_temperature", "mean_temperature",
#                                            "maximum temperature", "minimum temperature"),
#                            "temperature", variable),
#          variable = ifelse(variable %in% c("relative humidity", "relative_humidity",
#                                            "humidity", "dewpoint temperature"),
#                            "humidity", variable),
#          variable = ifelse(variable %in% c("time_trend", "temporal_trends", "trend", "date",
#                                            "temporal_trend", "time_trends",
#                                            "long-term_trend_and_seasonality"), "time", variable)) |>
#   # 4) standardize method and parameter
#   mutate(method = ifelse(method %in% c("smoothing spline", "smoothing splines", "penalized spline",
#                                        "penalized regression spline", "penalized spline smoothing",
#                                        "spline smoother", "cubic smoothing spline"),
#                          "smoothing spline", method),
#          method = ifelse(method %in% c("natural cubic splines", "natural splines", "cubic splines",
#                                        "natural cubic spline", "natural spline",
#                                        "natural cubic regression spline"),
#                          "natural spline", method),
#          method = ifelse(method %in% c("loess", "LOESS"),
#                          "LOESS", method),
#          parameter = ifelse(method == "natural spline", "knots", parameter),
#          parameter = ifelse(method == "LOESS", "smoothing window", parameter),
#          parameter = ifelse(method == "smoothing spline", "smoothing parameter", parameter))
#
#
# # If there are multiple decisions for the same variable/type combination, concatenate them
# raw_df <- raw_df %>%
#   group_by(paper, variable, type) %>%
#   summarise(
#     model = first(model),
#     method = first(method),
#     parameter = first(parameter),
#     reason = ifelse(all(is.na(reason)), NA_character_,
#                     paste(unique(na.omit(reason)), collapse = " , ")),
#     decision = ifelse(all(is.na(decision)), NA_character_,
#                       paste(unique(na.omit(decision)), collapse = " , ")),
#     .groups = "drop"
#   )
#
#
# count_variable_type(raw_df)
# df <- raw_df |> filter_variable_type(n = 6)
# # pivot_variable_wider(df)
# # summarize_variable_binary(df_wide)
# # summarize_num_decisions_pp(df_wide)
# df_wide <- pivot_decision_wider(df)
# paper_df <- df_wide |> slice_papers(n_paper = 10)
#
# embed_df <- paper_df |> compute_text_embed()
# distance_item_df <- calc_decision_similarity(paper_df, embed = embed_df)
# distance_df <- distance_item_df |> calc_paper_similarity()
