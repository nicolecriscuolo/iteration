writing_functions
================
Nicole Criscuolo
2025-10-24

## Start small.

``` r
x_vec = rnorm(20, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.25127477 -2.15273302  1.22630220 -0.58780740  0.19544357  1.51190598
    ##  [7]  1.36930835 -1.54078122  1.36641866 -0.44913493  0.63466038  0.20975845
    ## [13] -0.77120986  0.72693852  0.65457599  0.02597080  0.04020835 -0.39659701
    ## [19] -0.82935694 -0.98259609

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

    ##  [1] -0.25127477 -2.15273302  1.22630220 -0.58780740  0.19544357  1.51190598
    ##  [7]  1.36930835 -1.54078122  1.36641866 -0.44913493  0.63466038  0.20975845
    ## [13] -0.77120986  0.72693852  0.65457599  0.02597080  0.04020835 -0.39659701
    ## [19] -0.82935694 -0.98259609

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1]  2.198575821  0.004633813  0.760034208 -1.918109287 -0.211437571
    ##   [6] -1.301110197  0.100562291  1.070402807  0.764549044  0.559851324
    ##  [11] -0.029379359 -0.491279249  0.238359824  0.113526202  0.310692277
    ##  [16] -0.659367327 -0.231599032 -0.484065988  1.082037271  0.124988702
    ##  [21]  0.501595391 -0.627565837 -1.254538501 -2.297542008 -0.335802030
    ##  [26] -0.194028973 -1.490836188  0.708544054  0.403882510  1.129666193
    ##  [31]  0.753132433  0.479713047 -2.095733803  0.881554633 -0.577058529
    ##  [36] -0.980577910  0.586008414  0.126163773 -0.152153127  0.744928033
    ##  [41] -0.299910816 -1.079947990 -0.038631908  0.901606066  1.284796276
    ##  [46] -0.945287926 -1.013639148  2.072785181 -0.050529334 -0.271951474
    ##  [51] -0.399811029  1.227719601 -0.267813798 -1.536245750 -0.278319731
    ##  [56]  0.221403945 -0.106679797  2.009606398 -0.702585200  1.640934011
    ##  [61]  0.999778659  0.939703303 -0.357259484  0.132310588  2.181752121
    ##  [66]  0.548950170 -0.981728861  0.447283612 -0.310552534  0.160350824
    ##  [71]  2.282849914 -0.041265501  0.135299459  2.065420066  0.649096371
    ##  [76]  0.191478920  1.084951084 -0.681050371 -0.209052146 -0.225995050
    ##  [81] -0.500245823 -0.194314333  0.638440398  0.123774264  0.820949224
    ##  [86]  0.455164524  0.849987338  1.060420627  0.755623098 -0.091921858
    ##  [91] -1.734748184 -1.203589298  1.003315300 -0.984987003  0.147988492
    ##  [96] -2.416223245 -0.962157381 -0.528273806 -1.493650028 -1.920335259
    ## [101] -0.025767606 -1.285027103  0.373089067  0.109110283 -0.612459221
    ## [106] -0.903965330  1.382535130  0.009904877 -1.839469412 -0.643077030
    ## [111] -0.300422606  0.022958182 -0.999008781 -0.267664028  0.687608366
    ## [116] -1.380099978 -0.805551593 -0.623565799  0.418756197  0.265174563
    ## [121]  2.787301089  0.561098599 -0.447710783

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
    ## 1  9.52  3.35

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
    ## 1   2.57      2.03

Write a function to do simulations.

The inputs are

- `n_subject` is the number of subjects
- `mu` is the true mean
- \`sigma~ is the true sd

Function simulates data from a normal and computes sample mean and sd.

``` r
sim_mean_sd = function(n_subject = 30, mu = 3, sigma = 2) { ## default values not necessary, just will have to specify later
  
  sim_df =
    tibble(
      x = rnorm(n = n_subject, mean = mu, sd = sigma)
    )
  
  sim_df |> 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )

}
```

Let’s run this function.

``` r
sim_mean_sd(n_subject = 3800, mu = 8) ##providing named arguments if default not specified or if it is it overrides it
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   7.98      2.00

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
