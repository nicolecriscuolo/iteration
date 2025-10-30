writing_functions
================
Nicole Criscuolo
2025-10-24

## Start small.

``` r
x_vec = rnorm(20, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  2.6155045 -0.4697215 -0.5291157 -0.1101609  0.0943158 -0.2375611
    ##  [7]  1.0333760  0.6905066  0.9580470  1.3471925 -0.8563685 -0.4474551
    ## [13]  0.2161763 -0.8662173 -0.8012114 -0.2629186 -1.2027800 -0.6007217
    ## [19] -1.4878171  0.9169303

Write a function to compute z scores.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("The input x should be numeric")
  }
  
  if (length(x) < 5) {
    stop("Only compute z scores when the input has 5 or more numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
  
}
```

Let’s try our function.

``` r
z_scores(x = x_vec)
```

    ##  [1]  2.6155045 -0.4697215 -0.5291157 -0.1101609  0.0943158 -0.2375611
    ##  [7]  1.0333760  0.6905066  0.9580470  1.3471925 -0.8563685 -0.4474551
    ## [13]  0.2161763 -0.8662173 -0.8012114 -0.2629186 -1.2027800 -0.6007217
    ## [19] -1.4878171  0.9169303

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1]  0.428742158 -0.540802854  1.313816956 -0.162126252 -0.111524367
    ##   [6] -1.596734196 -1.635542046  0.738112806 -1.751986102 -0.217399415
    ##  [11] -0.545805703 -1.670892006 -1.057956534  1.180217599  0.116376515
    ##  [16]  0.466166341 -1.468363203  0.470182790  0.927440837 -0.128729780
    ##  [21] -0.884081995  0.142289085  0.647108686 -0.533022040 -0.159745021
    ##  [26] -0.700256063 -0.916937274 -1.196343290 -0.401924317  1.180726573
    ##  [31] -1.023906782  0.751235646  0.078931621 -0.061773471 -0.105770032
    ##  [36]  0.545112621  1.283475680 -0.220662350  1.639740112  0.392031247
    ##  [41]  1.454489855  0.732865860  0.442903831  0.126773866  1.308853783
    ##  [46]  0.986852073  0.873471131  0.016363554  0.501277081  0.377110868
    ##  [51] -1.071374302  0.072669879 -1.328074497  0.103463847  0.227676529
    ##  [56] -0.613877871 -0.082053434 -1.639077076  2.160907304  0.686548416
    ##  [61] -0.108730934  0.702365351  0.859079085  1.472034729 -0.865453042
    ##  [66]  0.074065760 -2.245051116 -0.179977769 -0.619679162 -1.205688692
    ##  [71]  0.611840307 -0.047379799 -0.048260990 -1.450037654 -1.072801497
    ##  [76]  0.267016259 -2.374415824 -2.573717010  1.379704308 -0.352206088
    ##  [81] -0.827046328  1.243798721  0.881444603 -0.408148925  0.003191203
    ##  [86] -1.493347993  0.250584862 -0.382334894  0.235584111  1.049507907
    ##  [91] -0.314141760  1.336308973  1.058069734  0.282568082 -1.439296279
    ##  [96] -0.743070374 -0.399160292  1.742431351 -0.759535412  2.599520902
    ## [101]  0.583353787 -0.068063518  0.149434655 -2.025542554 -0.892605728
    ## [106]  0.853083029 -0.756303481 -0.442714526  0.048762409  0.932014438
    ## [111]  0.424596450  0.832556074  1.283758736 -1.185545416  0.041262374
    ## [116]  1.881499769  0.834203323  0.924568740 -0.480618549  0.295627597
    ## [121]  1.322640394 -0.629994281 -0.612801010

Let’s break our function.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Only compute z scores when the input has 5 or more numbers

``` r
z_scores("my name is Nicole")
```

    ## Error in z_scores("my name is Nicole"): The input x should be numeric

## Let’s compute stuff.

Let’s compute and return mean and sd of numeric vector.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("The input x should be numeric")
  }
  
  if (length(x) < 5) {
    stop("Only compute mean and sd when the input has 5 or more numbers")
  }
  
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  
  c(mean_x, sd_x)
  
#not necessary just makes output look nicer
  tibble( 
    mean = mean_x,
    sd = sd_x
  )
}
```

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.83  3.69

Let’s *simulate* some data.

``` r
sim_df =
  tibble(
    x = rnorm(n = 30, mean = 3, sd = 2)
  )

sim_df |> 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.64      2.14

Write a function to do simulations.

The inputs are

- `n_subject` is the number of subjects
- `mu` is the true mean
- \`sigma~ is the true sd

Function simulates data from a normal and computes sample mean and sd.

``` r
sim_mean_sd = function(n_subj, mu = 3, sigma = 2) { ## default values not necessary, just will have to specify later
  
  sim_df =
    tibble(
      x = rnorm(n = n_subj, mean = mu, sd = sigma)
    )
  
  sim_df |> 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )

}
```

(we now are sourcing this code chunk)

``` r
source("sim_mean_sd.R")
```

Let’s run this function.

``` r
sim_mean_sd(n_subj = 3800, mu = 8) ##provide named arguments if default not specified or if it is it overrides it
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   8.00      2.01

Import LOTR data.

``` r
fellowship_ring =
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") |> 
  mutate(movie = "Fellowship of the Ring")

two_towers =
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") |> 
  mutate(movie = "Two Towers")

return_king =
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") |> 
  mutate(movie = "Return od the King")

lotr_df =
  bind_rows(fellowship_ring, two_towers, return_king)
```

Turn this into a function

``` r
lotr_import = function(cell_range, movie_title) {
  
  df =
    read_excel("data/LotR_Words.xlsx", range = cell_range) |> 
    mutate(movie = movie_title)
  
  df
}
```

``` r
fellowship = lotr_import(cell_range = "B3:D6", movie_title = "Fellowship")
two_towers = lotr_import(cell_range = "F3:H6", movie_title = "Two Towers")
return = lotr_import(cell_range = "J3:L6", movie_title = "Return")

bind_rows(fellowship, two_towers, return)
```

    ## # A tibble: 9 × 4
    ##   Race   Female  Male movie     
    ##   <chr>   <dbl> <dbl> <chr>     
    ## 1 Elf      1229   971 Fellowship
    ## 2 Hobbit     14  3644 Fellowship
    ## 3 Man         0  1995 Fellowship
    ## 4 Elf       331   513 Two Towers
    ## 5 Hobbit      0  2463 Two Towers
    ## 6 Man       401  3589 Two Towers
    ## 7 Elf       183   510 Return    
    ## 8 Hobbit      2  2673 Return    
    ## 9 Man       268  2459 Return

``` r
full_join(fellowship, two_towers)
```

    ## Joining with `by = join_by(Race, Female, Male, movie)`

    ## # A tibble: 6 × 4
    ##   Race   Female  Male movie     
    ##   <chr>   <dbl> <dbl> <chr>     
    ## 1 Elf      1229   971 Fellowship
    ## 2 Hobbit     14  3644 Fellowship
    ## 3 Man         0  1995 Fellowship
    ## 4 Elf       331   513 Two Towers
    ## 5 Hobbit      0  2463 Two Towers
    ## 6 Man       401  3589 Two Towers

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

Write an import function.

``` r
nsduh_import = function(html, table_num) {
  
  table = 
  html |> 
  html_table() |> 
  nth(table_num) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
  
}

nsduh_results = 
  bind_rows(
    nsduh_import(nsduh_html, 1),
    nsduh_import(nsduh_html, 4),
    nsduh_import(nsduh_html, 5)
  )
```
