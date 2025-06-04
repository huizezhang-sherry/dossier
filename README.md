
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dossier

<!-- badges: start -->

[![R-CMD-check](https://github.com/huizezhang-sherry/dossier/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/huizezhang-sherry/dossier/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/huizezhang-sherry/dossier/graph/badge.svg)](https://app.codecov.io/gh/huizezhang-sherry/dossier)
<!-- badges: end -->

The `dossier` package provides tools to analyze and compare decisions
made in the data analysis across multiple applied studies on the same
topic. It helps explore the “garden of forking paths” by highlighting
patterns, similarities, and differences in how analyses are conducted.

## Installation

<!-- You can install the released version of dossier from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("dossier") -->

<!-- ``` -->

<!-- And  -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("huizezhang-sherry/dossier")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dossier)
library(dplyr)
library(text)
raw_df <- readr::read_csv(system.file("papers.csv", package = "dossier"))
(tbl_df <- as_decision_tbl(raw_df))
#> # A tibble: 35 × 8
#>    paper       variable            type   model method parameter reason decision
#>    <chr>       <chr>               <chr>  <chr> <chr>  <chr>     <chr>  <chr>   
#>  1 braga       barometric_pressure param… gene… LOESS  smoothin… to mi… <NA>    
#>  2 braga       barometric_pressure spati… gene… <NA>   <NA>      to al… chosen …
#>  3 braga       humidity            param… gene… LOESS  smoothin… to mi… <NA>    
#>  4 braga       humidity            spati… gene… <NA>   <NA>      to al… chosen …
#>  5 braga       temperature         param… gene… LOESS  smoothin… to mi… <NA>    
#>  6 braga       temperature         spati… gene… <NA>   <NA>      to al… chosen …
#>  7 braga       time                param… gene… LOESS  smoothin… to el… <NA>    
#>  8 braga       time                spati… gene… <NA>   <NA>      seaso… chosen …
#>  9 katsouyanni humidity            param… gene… LOESS  smoothin… to mi… <NA>    
#> 10 katsouyanni humidity            tempo… gene… <NA>   <NA>      <NA>   same da…
#> # ℹ 25 more rows
```

``` r
# select the variable-type pair to compare papers
count_variable_type(tbl_df) 
#> # A tibble: 11 × 3
#>    variable            type         .n
#>    <chr>               <chr>     <int>
#>  1 temperature         parameter     6
#>  2 time                parameter     6
#>  3 humidity            parameter     4
#>  4 temperature         temporal      4
#>  5 PM                  temporal      3
#>  6 humidity            temporal      3
#>  7 time                spatial       2
#>  8 barometric_pressure parameter     1
#>  9 barometric_pressure spatial       1
#> 10 humidity            spatial       1
#> 11 temperature         spatial       1
(df <- tbl_df |> filter_var_type(n = 6)) # first 6 variable-type pairs
#> # A tibble: 26 × 8
#>    paper       variable    type      model      method parameter reason decision
#>    <chr>       <chr>       <chr>     <chr>      <chr>  <chr>     <chr>  <chr>   
#>  1 braga       humidity    parameter generaliz… LOESS  smoothin… to mi… <NA>    
#>  2 braga       temperature parameter generaliz… LOESS  smoothin… to mi… <NA>    
#>  3 braga       time        parameter generaliz… LOESS  smoothin… to el… <NA>    
#>  4 katsouyanni humidity    parameter generaliz… LOESS  smoothin… to mi… <NA>    
#>  5 katsouyanni humidity    temporal  generaliz… <NA>   <NA>      <NA>   same da…
#>  6 katsouyanni temperature parameter generaliz… LOESS  smoothin… to mi… <NA>    
#>  7 katsouyanni temperature temporal  generaliz… <NA>   <NA>      <NA>   same da…
#>  8 katsouyanni time        parameter generaliz… LOESS  smoothin… to mi… <NA>    
#>  9 ostro       PM          temporal  Poisson r… <NA>   <NA>      previ… 2-day a…
#> 10 ostro       humidity    parameter Poisson r… smoot… smoothin… <NA>   3       
#> # ℹ 16 more rows

# subset to papers with at least 3 decisions (include all)
count_paper_decisions(df)
#>         paper .n
#> 1       ostro 11
#> 2 katsouyanni  8
#> 3    schwartz  8
#> 4   zanobetti  8
#> 5       braga  6
#> 6        peel  6
(paper_df <- df |> filter_papers(n_value = 3))
#> # A tibble: 26 × 8
#>    paper       variable    type      model      method parameter reason decision
#>    <chr>       <chr>       <chr>     <chr>      <chr>  <chr>     <chr>  <chr>   
#>  1 braga       humidity    parameter generaliz… LOESS  smoothin… to mi… <NA>    
#>  2 braga       temperature parameter generaliz… LOESS  smoothin… to mi… <NA>    
#>  3 braga       time        parameter generaliz… LOESS  smoothin… to el… <NA>    
#>  4 katsouyanni humidity    parameter generaliz… LOESS  smoothin… to mi… <NA>    
#>  5 katsouyanni humidity    temporal  generaliz… <NA>   <NA>      <NA>   same da…
#>  6 katsouyanni temperature parameter generaliz… LOESS  smoothin… to mi… <NA>    
#>  7 katsouyanni temperature temporal  generaliz… <NA>   <NA>      <NA>   same da…
#>  8 katsouyanni time        parameter generaliz… LOESS  smoothin… to mi… <NA>    
#>  9 ostro       PM          temporal  Poisson r… <NA>   <NA>      previ… 2-day a…
#> 10 ostro       humidity    parameter Poisson r… smoot… smoothin… <NA>   3       
#> # ℹ 16 more rows

count_paper_pair_decisions(paper_df)
#> # A data frame: 15 × 3
#>    paper1      paper2         .n
#>    <chr>       <chr>       <int>
#>  1 braga       katsouyanni     6
#>  2 braga       ostro           4
#>  3 braga       peel            2
#>  4 braga       schwartz        4
#>  5 braga       zanobetti       6
#>  6 katsouyanni ostro           6
#>  7 katsouyanni peel            3
#>  8 katsouyanni schwartz        4
#>  9 katsouyanni zanobetti       8
#> 10 ostro       peel            6
#> 11 ostro       schwartz        7
#> 12 ostro       zanobetti       6
#> 13 peel        schwartz        5
#> 14 peel        zanobetti       3
#> 15 schwartz    zanobetti       4
```

``` r
# calculate the text embed
embed_df <- paper_df |> compute_text_embed()
#> [0;34mProcessing batch 1/1
#> [0m
#> [0;32mCompleted layers output for texts (variable: 1/1, duration: 3.614024 secs).
#> [0m
#> [0;32mCompleted layers aggregation for word_type_embeddings. 
#> [0m
#> [0;34mCompleted layers aggregation (variable 1/1, duration: 3.068682 secs).
#> [0m
#> [0;34mCompleted layers aggregation (variable 1/1, duration: 3.034339 secs).
#> [0m
#> [0;32mMinutes from start:  0.164[0m
#> [0;30mEstimated embedding time left = 0 minutes[0m
# calculate decision similarity
(distance_decision_df <- calc_decision_similarity(paper_df, embed = embed_df))
#> # A tibble: 74 × 4
#>    paper1 paper2      decision                      dist
#>    <chr>  <chr>       <chr>                        <dbl>
#>  1 braga  katsouyanni humidity_parameter_method    1    
#>  2 braga  katsouyanni temperature_parameter_method 1    
#>  3 braga  katsouyanni time_parameter_method        1    
#>  4 braga  katsouyanni humidity_parameter_reason    0.946
#>  5 braga  katsouyanni temperature_parameter_reason 0.946
#>  6 braga  katsouyanni time_parameter_reason        0.760
#>  7 braga  ostro       humidity_parameter_method    0    
#>  8 braga  ostro       temperature_parameter_method 0    
#>  9 braga  ostro       time_parameter_method        0    
#> 10 braga  ostro       time_parameter_reason        0.728
#> # ℹ 64 more rows
# aggregate from decision similarity to paper similarity
(distance_df <- distance_decision_df |> calc_paper_similarity())
#> # A tibble: 15 × 4
#>    paper1      paper2        dist similarity
#>    <fct>       <fct>        <dbl>      <dbl>
#>  1 braga       katsouyanni 0.0578      0.942
#>  2 braga       ostro       0.818       0.182
#>  3 braga       peel        1           0    
#>  4 braga       schwartz    0.101       0.899
#>  5 braga       zanobetti   0.0546      0.945
#>  6 katsouyanni ostro       0.627       0.373
#>  7 katsouyanni peel        0.786       0.214
#>  8 katsouyanni schwartz    0.140       0.860
#>  9 katsouyanni zanobetti   0.0799      0.920
#> 10 ostro       peel        0.545       0.455
#> 11 ostro       schwartz    0.565       0.435
#> 12 ostro       zanobetti   0.686       0.314
#> 13 peel        schwartz    0.633       0.367
#> 14 peel        zanobetti   0.825       0.175
#> 15 schwartz    zanobetti   0.143       0.857
```

## Diagnostics

``` r
diag_decision_ppp(distance_decision_df, distance_df)
#> Joining with `by = join_by(paper1, paper2)`
#> Storing counts in `nn`, as `n` already present in input
#> Joining with `by = join_by(n)`
#> # A tibble: 7 × 4
#>   num_decision count   prop similarity
#>          <int> <int>  <dbl>      <dbl>
#> 1            2     1 0.0667      0    
#> 2            3     2 0.133       0.194
#> 3            4     4 0.267       0.699
#> 4            5     1 0.0667      0.367
#> 5            6     5 0.333       0.606
#> 6            7     1 0.0667      0.435
#> 7            8     1 0.0667      0.920
view_pairs(paper_df, distance_decision_df, "braga", "zanobetti")
#> Joining with `by = join_by(decision)`
#> # A tibble: 8 × 4
#>   decision                      braga                           zanobetti   dist
#>   <chr>                         <chr>                           <chr>      <dbl>
#> 1 humidity_parameter_method     LOESS                           LOESS      1    
#> 2 humidity_parameter_reason     to minimize Akaike's Informati… to minim…  0.946
#> 3 temperature_parameter_method  LOESS                           LOESS      1    
#> 4 temperature_parameter_reason  to minimize Akaike's Informati… to minim…  0.946
#> 5 time_parameter_method         LOESS                           LOESS      1    
#> 6 time_parameter_reason         to eliminate seasonal patterns… to minim…  0.779
#> 7 humidity_temporal_decision    <NA>                            previous… NA    
#> 8 temperature_temporal_decision <NA>                            previous… NA
```
