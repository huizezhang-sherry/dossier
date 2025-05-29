
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
(raw_df <- readr::read_csv(system.file("papers.csv", package = "dossier")))
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
tbl_df <- to_decision_tbl(raw_df)
```

``` r
# select the variable-type pair to compare papers
count_variable_type(tbl_df)
#> # A tibble: 11 × 3
#>    variable            type          n
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
df <- tbl_df |> filter_var_type(n = 6) # first 6 variable-type pairs

# subset to papers with at least 3 decisions
summarize_num_decisions_pp(df)
#> # A tibble: 6 × 2
#>   paper       count
#>   <chr>       <dbl>
#> 1 ostro           6
#> 2 katsouyanni     5
#> 3 zanobetti       5
#> 4 peel            4
#> 5 braga           3
#> 6 schwartz        3
paper_df <- df |> filter_papers(n_count = 3)

pivot_decision_tbl_longer(paper_df)
#> # A tibble: 47 × 8
#>    paper       variable    type      model       parameter decision reason    id
#>    <chr>       <chr>       <chr>     <chr>       <chr>     <chr>    <chr>  <int>
#>  1 braga       humidity    parameter generalize… smoothin… humidit… LOESS      1
#>  2 braga       humidity    parameter generalize… smoothin… humidit… to mi…     2
#>  3 braga       temperature parameter generalize… smoothin… tempera… LOESS      3
#>  4 braga       temperature parameter generalize… smoothin… tempera… to mi…     4
#>  5 braga       time        parameter generalize… smoothin… time_pa… LOESS      5
#>  6 braga       time        parameter generalize… smoothin… time_pa… to el…     6
#>  7 katsouyanni humidity    parameter generalize… smoothin… humidit… LOESS      7
#>  8 katsouyanni humidity    parameter generalize… smoothin… humidit… to mi…     8
#>  9 katsouyanni humidity    temporal  generalize… <NA>      humidit… same …     9
#> 10 katsouyanni temperature parameter generalize… smoothin… tempera… LOESS     10
#> # ℹ 37 more rows
pivot_decision_tbl_wider(paper_df)
#> # A tibble: 6 × 17
#>   paper       model                humidity_parameter_m…¹ humidity_parameter_r…²
#>   <chr>       <chr>                <chr>                  <chr>                 
#> 1 braga       generalized additiv… LOESS                  to minimize Akaike's …
#> 2 katsouyanni generalized additiv… LOESS                  to minimize Akaike's …
#> 3 ostro       Poisson regression   smoothing spline       <NA>                  
#> 4 peel        Poisson generalized… <NA>                   <NA>                  
#> 5 schwartz    Poisson regression   <NA>                   <NA>                  
#> 6 zanobetti   generalized additiv… LOESS                  to minimize Akaike's …
#> # ℹ abbreviated names: ¹​humidity_parameter_method, ²​humidity_parameter_reason
#> # ℹ 13 more variables: humidity_parameter_decision <chr>,
#> #   temperature_parameter_method <chr>, temperature_parameter_reason <chr>,
#> #   temperature_parameter_decision <chr>, time_parameter_method <chr>,
#> #   time_parameter_reason <chr>, time_parameter_decision <chr>,
#> #   humidity_temporal_reason <chr>, humidity_temporal_decision <chr>,
#> #   temperature_temporal_reason <chr>, temperature_temporal_decision <chr>, …
```

``` r
summarize_decisions_ppp(paper_df)
#> # A tibble: 15 × 3
#>    paper1      paper2      pairs
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
summarize_decisions_ppp(paper_df) |> dplyr::count(pairs)
#> # A tibble: 7 × 2
#>   pairs     n
#>   <int> <int>
#> 1     2     1
#> 2     3     2
#> 3     4     4
#> 4     5     1
#> 5     6     5
#> 6     7     1
#> 7     8     1
```

``` r
# calculate the text embed
embed_df <- paper_df |> compute_text_embed()
#> [0;34mProcessing batch 1/1
#> [0m
#> [0;32mCompleted layers output for texts (variable: 1/1, duration: 4.061170 secs).
#> [0m
#> [0;32mCompleted layers aggregation for word_type_embeddings. 
#> [0m
#> [0;34mCompleted layers aggregation (variable 1/1, duration: 2.610729 secs).
#> [0m
#> [0;34mCompleted layers aggregation (variable 1/1, duration: 2.439704 secs).
#> [0m
#> [0;32mMinutes from start:  0.159[0m
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
