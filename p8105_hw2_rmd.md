Homework 2
================
Pavithra Srinivasan
October 2, 2023

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
```

### Problem 1

``` r
month_df = 
  tibble(
    month_num = 1:12,
    month_abb = month.abb,
    month = month.name
  )

pols = 
  read_csv("./data/pols-month.csv") |>
  separate(mon, into = c("year", "month_num", "day"), convert = TRUE) |>
  mutate(
    president = recode(prez_gop, "0" = "dem", "1" = "gop", "2" = "gop")) |>
  left_join(x = _, y = month_df) |> 
  select(year, month, everything(), -day, -starts_with("prez")) 
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Joining with `by = join_by(month_num)`

We also clean the 538 `snp` data, which contains information related to
Standard & Poor’s stock market index.

``` r
snp = 
  read_csv("./data/snp.csv") |>
  separate(date, into = c("month", "day", "year"), convert = TRUE) |>
  arrange(year, month) |>
  mutate(month = month.name[month]) |>
  select(year, month, close) 
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Finally, we tidy the `unemployment` data so that it can be merged with
the `pols` and `snp` datasets.

``` r
unemployment = 
  read_csv("./data/unemployment.csv") |>
  rename(year = Year) |>
  pivot_longer(
    Jan:Dec, 
    names_to = "month_abb",
    values_to = "unemployment"
  ) |> 
  left_join(x = _, y = month_df) |> 
  select(year, month, unemployment)
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Joining with `by = join_by(month_abb)`

Now we merge the three datasets!

``` r
data_538 = 
  left_join(pols, snp) |>
  left_join(x = _, y = unemployment)
```

    ## Joining with `by = join_by(year, month)`
    ## Joining with `by = join_by(year, month)`

``` r
str(data_538)
```

    ## tibble [822 × 13] (S3: tbl_df/tbl/data.frame)
    ##  $ year        : num [1:822] 1947 1947 1947 1947 1947 ...
    ##  $ month       : chr [1:822] "January" "February" "March" "April" ...
    ##  $ month_num   : int [1:822] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ gov_gop     : num [1:822] 23 23 23 23 23 23 23 23 23 23 ...
    ##  $ sen_gop     : num [1:822] 51 51 51 51 51 51 51 51 51 51 ...
    ##  $ rep_gop     : num [1:822] 253 253 253 253 253 253 253 253 253 253 ...
    ##  $ gov_dem     : num [1:822] 23 23 23 23 23 23 23 23 23 23 ...
    ##  $ sen_dem     : num [1:822] 45 45 45 45 45 45 45 45 45 45 ...
    ##  $ rep_dem     : num [1:822] 198 198 198 198 198 198 198 198 198 198 ...
    ##  $ president   : chr [1:822] "dem" "dem" "dem" "dem" ...
    ##  $ month_abb   : chr [1:822] "Jan" "Feb" "Mar" "Apr" ...
    ##  $ close       : num [1:822] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ unemployment: num [1:822] NA NA NA NA NA NA NA NA NA NA ...

### Problem 2

Read in and clean the Mr. Trash Wheel sheet:

``` r
df_trash_wheel = read_excel("./data/trash_wheel.xlsx") 
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
  sheet = "Mr. Trash Wheel"
  janitor::clean_names(df_trash_wheel)
```

    ## # A tibble: 548 × 16
    ##    dumpster month year  date                weight_tons volume_cubic_yards
    ##       <dbl> <chr> <chr> <dttm>                    <dbl>              <dbl>
    ##  1        1 May   2014  2014-05-16 00:00:00        4.31                 18
    ##  2        2 May   2014  2014-05-16 00:00:00        2.74                 13
    ##  3        3 May   2014  2014-05-16 00:00:00        3.45                 15
    ##  4        4 May   2014  2014-05-17 00:00:00        3.1                  15
    ##  5        5 May   2014  2014-05-17 00:00:00        4.06                 18
    ##  6        6 May   2014  2014-05-20 00:00:00        2.71                 13
    ##  7        7 May   2014  2014-05-21 00:00:00        1.91                  8
    ##  8        8 May   2014  2014-05-28 00:00:00        3.7                  16
    ##  9        9 June  2014  2014-06-05 00:00:00        2.52                 14
    ## 10       10 June  2014  2014-06-11 00:00:00        3.76                 18
    ## # ℹ 538 more rows
    ## # ℹ 10 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, grocery_bags <dbl>,
    ## #   chip_bags <dbl>, sports_balls <dbl>, homes_powered <dbl>, x15 <lgl>,
    ## #   x16 <lgl>

``` r
  names(df_trash_wheel)
```

    ##  [1] "Dumpster"             "Month"                "Year"                
    ##  [4] "Date"                 "weight_tons"          "Volume (cubic yards)"
    ##  [7] "Plastic Bottles"      "Polystyrene"          "Cigarette Butts"     
    ## [10] "Glass Bottles"        "Grocery Bags"         "Chip Bags"           
    ## [13] "Sports Balls"         "Homes Powered*"       "...15"               
    ## [16] "...16"

``` r
  drop_na(df_trash_wheel) |> 
  mutate(homes_powered = (weight_tons*500)/30,
    trash_wheel = "Trash_1"
  )
```

    ## # A tibble: 0 × 18
    ## # ℹ 18 variables: Dumpster <dbl>, Month <chr>, Year <chr>, Date <dttm>,
    ## #   weight_tons <dbl>, Volume (cubic yards) <dbl>, Plastic Bottles <dbl>,
    ## #   Polystyrene <dbl>, Cigarette Butts <dbl>, Glass Bottles <dbl>,
    ## #   Grocery Bags <dbl>, Chip Bags <dbl>, Sports Balls <dbl>,
    ## #   Homes Powered* <dbl>, ...15 <lgl>, ...16 <lgl>, homes_powered <dbl>,
    ## #   trash_wheel <chr>

Read in and clean the Professor Trash Wheel sheet:

``` r
df_prof_wheel = read_excel("./data/trash_wheel.xlsx") 
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
  sheet = "Professor Trash Wheel"
  janitor::clean_names(df_prof_wheel)
```

    ## # A tibble: 548 × 16
    ##    dumpster month year  date                weight_tons volume_cubic_yards
    ##       <dbl> <chr> <chr> <dttm>                    <dbl>              <dbl>
    ##  1        1 May   2014  2014-05-16 00:00:00        4.31                 18
    ##  2        2 May   2014  2014-05-16 00:00:00        2.74                 13
    ##  3        3 May   2014  2014-05-16 00:00:00        3.45                 15
    ##  4        4 May   2014  2014-05-17 00:00:00        3.1                  15
    ##  5        5 May   2014  2014-05-17 00:00:00        4.06                 18
    ##  6        6 May   2014  2014-05-20 00:00:00        2.71                 13
    ##  7        7 May   2014  2014-05-21 00:00:00        1.91                  8
    ##  8        8 May   2014  2014-05-28 00:00:00        3.7                  16
    ##  9        9 June  2014  2014-06-05 00:00:00        2.52                 14
    ## 10       10 June  2014  2014-06-11 00:00:00        3.76                 18
    ## # ℹ 538 more rows
    ## # ℹ 10 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, grocery_bags <dbl>,
    ## #   chip_bags <dbl>, sports_balls <dbl>, homes_powered <dbl>, x15 <lgl>,
    ## #   x16 <lgl>

``` r
  names(df_prof_wheel)
```

    ##  [1] "Dumpster"             "Month"                "Year"                
    ##  [4] "Date"                 "weight_tons"          "Volume (cubic yards)"
    ##  [7] "Plastic Bottles"      "Polystyrene"          "Cigarette Butts"     
    ## [10] "Glass Bottles"        "Grocery Bags"         "Chip Bags"           
    ## [13] "Sports Balls"         "Homes Powered*"       "...15"               
    ## [16] "...16"

``` r
  drop_na(df_prof_wheel) |> 
  mutate(homes_powered = (weight_tons*500)/30,
    trash_wheel = "Trash_2"
  )
```

    ## # A tibble: 0 × 18
    ## # ℹ 18 variables: Dumpster <dbl>, Month <chr>, Year <chr>, Date <dttm>,
    ## #   weight_tons <dbl>, Volume (cubic yards) <dbl>, Plastic Bottles <dbl>,
    ## #   Polystyrene <dbl>, Cigarette Butts <dbl>, Glass Bottles <dbl>,
    ## #   Grocery Bags <dbl>, Chip Bags <dbl>, Sports Balls <dbl>,
    ## #   Homes Powered* <dbl>, ...15 <lgl>, ...16 <lgl>, homes_powered <dbl>,
    ## #   trash_wheel <chr>

Read in and clean the Gwynnda Trash Wheel sheet:

``` r
df_gwynn_wheel = read_excel("./data/trash_wheel.xlsx") 
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
  sheet = "Gwynnda Trash Wheel"
  janitor::clean_names(df_gwynn_wheel)
```

    ## # A tibble: 548 × 16
    ##    dumpster month year  date                weight_tons volume_cubic_yards
    ##       <dbl> <chr> <chr> <dttm>                    <dbl>              <dbl>
    ##  1        1 May   2014  2014-05-16 00:00:00        4.31                 18
    ##  2        2 May   2014  2014-05-16 00:00:00        2.74                 13
    ##  3        3 May   2014  2014-05-16 00:00:00        3.45                 15
    ##  4        4 May   2014  2014-05-17 00:00:00        3.1                  15
    ##  5        5 May   2014  2014-05-17 00:00:00        4.06                 18
    ##  6        6 May   2014  2014-05-20 00:00:00        2.71                 13
    ##  7        7 May   2014  2014-05-21 00:00:00        1.91                  8
    ##  8        8 May   2014  2014-05-28 00:00:00        3.7                  16
    ##  9        9 June  2014  2014-06-05 00:00:00        2.52                 14
    ## 10       10 June  2014  2014-06-11 00:00:00        3.76                 18
    ## # ℹ 538 more rows
    ## # ℹ 10 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, grocery_bags <dbl>,
    ## #   chip_bags <dbl>, sports_balls <dbl>, homes_powered <dbl>, x15 <lgl>,
    ## #   x16 <lgl>

``` r
  names(df_gwynn_wheel)
```

    ##  [1] "Dumpster"             "Month"                "Year"                
    ##  [4] "Date"                 "weight_tons"          "Volume (cubic yards)"
    ##  [7] "Plastic Bottles"      "Polystyrene"          "Cigarette Butts"     
    ## [10] "Glass Bottles"        "Grocery Bags"         "Chip Bags"           
    ## [13] "Sports Balls"         "Homes Powered*"       "...15"               
    ## [16] "...16"

``` r
  drop_na(df_gwynn_wheel) |> 
  mutate(homes_powered = (weight_tons*500)/30,
    trash_wheel = "Trash_3"
  )
```

    ## # A tibble: 0 × 18
    ## # ℹ 18 variables: Dumpster <dbl>, Month <chr>, Year <chr>, Date <dttm>,
    ## #   weight_tons <dbl>, Volume (cubic yards) <dbl>, Plastic Bottles <dbl>,
    ## #   Polystyrene <dbl>, Cigarette Butts <dbl>, Glass Bottles <dbl>,
    ## #   Grocery Bags <dbl>, Chip Bags <dbl>, Sports Balls <dbl>,
    ## #   Homes Powered* <dbl>, ...15 <lgl>, ...16 <lgl>, homes_powered <dbl>,
    ## #   trash_wheel <chr>

Tidying and Combining all 3 datasets into one:

``` r
df_combined_trash = 
  bind_rows(df_trash_wheel, df_prof_wheel, df_gwynn_wheel)
```

Description: The combination of these 3 datasets inform us how much
trash was collected all together between Mr. Trash Wheel, Professor
Trash wheel and Gwynnda Trash Wheel.

### Problem 3

READ, TIDY AND COMBINE THE MCI BASELINE AND AMYLOID DATASETS:

``` r
df_MCI_baseline = 
  read_csv("./data/MCI_baseline.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  mutate(
    sex = case_match(
      sex, 1 ~ "male", 0 ~ "female"),
    apoe4 = case_match(
      apoe4, 1 ~ "carrier", 0 ~ "non_carrier"),
  )
```

    ## Rows: 483 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Age_ at_ onset
    ## dbl (5): ID, Current Age, Sex, Education, apoe4
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
    filter(df_MCI_baseline, age_at_onset != ".")
```

    ## # A tibble: 97 × 6
    ##       id current_age sex    education apoe4       age_at_onset
    ##    <dbl>       <dbl> <chr>      <dbl> <chr>       <chr>       
    ##  1     3        62.5 male          16 carrier     66.8        
    ##  2     5        66   male          16 non_carrier 68.7        
    ##  3     7        66.5 male          18 non_carrier 74          
    ##  4    13        63.1 male          12 carrier     69          
    ##  5    14        58.4 female        20 non_carrier 66.2        
    ##  6    18        67.8 male          16 non_carrier 69.8        
    ##  7    22        67.3 female        20 carrier     74.6        
    ##  8    26        64.8 female        20 carrier     71.1        
    ##  9    30        66.3 female        12 non_carrier 73.1        
    ## 10    39        68.3 female        16 carrier     70.2        
    ## # ℹ 87 more rows

``` r
df_MCI_amyloid = 
  read_csv("./data/mci_amyloid.csv", skip = 1) %>%
  janitor::clean_names() %>% 
  mutate(id = `study_id`) %>% 
  select(-study_id)
```

    ## Rows: 487 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): Baseline, Time 2, Time 4, Time 6, Time 8
    ## dbl (1): Study ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
  drop_na(df_MCI_amyloid)
```

    ## # A tibble: 347 × 6
    ##    baseline    time_2      time_4      time_6      time_8         id
    ##    <chr>       <chr>       <chr>       <chr>       <chr>       <dbl>
    ##  1 0.107481183 0.109157373 0.109457839 0.105729713 0.10661845      2
    ##  2 0.109251358 0.108699686 0.110540386 0.107476797 0.111212209     4
    ##  3 0.107950408 0.112273883 0.115139677 0.106606054 0.106052066     5
    ##  4 0.112426974 0.112853415 0.11143945  0.110279277 0.114982747     6
    ##  5 0.109119335 0.109316496 0.1114037   0.108586573 0.108993335    11
    ##  6 0.112042298 0.114167481 0.109859682 0.106842794 0.107334106    12
    ##  7 0.110300505 0.108534417 0.108100808 0.109229662 0.104861901    13
    ##  8 0.110218888 0.113741328 0.111101474 0.108852437 0.109556166    16
    ##  9 0.108399246 0.113317542 0.105909034 0.107196914 0.110199133    17
    ## 10 0.114137255 0.107093264 0.110872562 0.108982605 0.106873903    18
    ## # ℹ 337 more rows

``` r
df_MCI_combined = 
  inner_join(df_MCI_baseline, df_MCI_amyloid)
```

    ## Joining with `by = join_by(id)`

``` r
  nrow(df_MCI_combined)
```

    ## [1] 475

``` r
  drop_na(df_MCI_combined)
```

    ## # A tibble: 337 × 11
    ##       id current_age sex    education apoe4  age_at_onset baseline time_2 time_4
    ##    <dbl>       <dbl> <chr>      <dbl> <chr>  <chr>        <chr>    <chr>  <chr> 
    ##  1     2        65.6 female        20 carri… .            0.10748… 0.109… 0.109…
    ##  2     4        69.8 female        16 non_c… .            0.10925… 0.108… 0.110…
    ##  3     5        66   male          16 non_c… 68.7         0.10795… 0.112… 0.115…
    ##  4     6        62.5 male          16 non_c… .            0.11242… 0.112… 0.111…
    ##  5    11        69.5 female        16 non_c… .            0.10911… 0.109… 0.111…
    ##  6    12        66.2 female        18 non_c… .            0.11204… 0.114… 0.109…
    ##  7    13        63.1 male          12 carri… 69           0.11030… 0.108… 0.108…
    ##  8    16        64.9 female        16 non_c… .            0.11021… 0.113… 0.111…
    ##  9    17        65   female        18 non_c… .            0.10839… 0.113… 0.105…
    ## 10    18        67.8 male          16 non_c… 69.8         0.11413… 0.107… 0.110…
    ## # ℹ 327 more rows
    ## # ℹ 2 more variables: time_6 <chr>, time_8 <chr>

``` r
write.csv(df_MCI_combined, file = "results/df_MCI_combined.csv")
```

DISCUSSION QUESTIONS:

1.  Total participants recruited? = 483

2.  Of these participants how many developed MCI?

``` r
filter(df_MCI_baseline, age_at_onset != ".")
```

    ## # A tibble: 97 × 6
    ##       id current_age sex    education apoe4       age_at_onset
    ##    <dbl>       <dbl> <chr>      <dbl> <chr>       <chr>       
    ##  1     3        62.5 male          16 carrier     66.8        
    ##  2     5        66   male          16 non_carrier 68.7        
    ##  3     7        66.5 male          18 non_carrier 74          
    ##  4    13        63.1 male          12 carrier     69          
    ##  5    14        58.4 female        20 non_carrier 66.2        
    ##  6    18        67.8 male          16 non_carrier 69.8        
    ##  7    22        67.3 female        20 carrier     74.6        
    ##  8    26        64.8 female        20 carrier     71.1        
    ##  9    30        66.3 female        12 non_carrier 73.1        
    ## 10    39        68.3 female        16 carrier     70.2        
    ## # ℹ 87 more rows

3.  Of the participants recruited, 97 developed MCI.

4.  Avg. Baseline age?

``` r
mean(pull(df_MCI_baseline, current_age))
```

    ## [1] 65.04679

= 65.04679

5.  What proportion of women are APOE4 carriers?

``` r
filter(df_MCI_baseline, sex != "male", apoe4 != "non_carrier")
```

    ## # A tibble: 63 × 6
    ##       id current_age sex    education apoe4   age_at_onset
    ##    <dbl>       <dbl> <chr>      <dbl> <chr>   <chr>       
    ##  1     1        63.1 female        16 carrier .           
    ##  2     2        65.6 female        20 carrier .           
    ##  3    22        67.3 female        20 carrier 74.6        
    ##  4    26        64.8 female        20 carrier 71.1        
    ##  5    34        64.8 female        16 carrier .           
    ##  6    39        68.3 female        16 carrier 70.2        
    ##  7    43        67.1 female        16 carrier 71.6        
    ##  8    47        66.1 female        12 carrier .           
    ##  9    50        67.6 female        18 carrier .           
    ## 10    52        63.2 female        16 carrier .           
    ## # ℹ 53 more rows

Filtering the male and non-carriers out yield a total number of 63 women
who are APOE4 carriers.

6.  Comment on the steps on the import process and the features of the
    Amyloid dataset: Read in and cleaned the overall data set.
    Conditionally mutated the “study id” variable so it can correspond
    to the participants of the data set. Then the “NA” values were
    dropped to create a cohesive dataframe.

7.  Check whether some participants appear in only the baseline or
    amyloid datasets, and comment on your findings. Combine the
    demographic and biomarker datasets so that only participants who
    appear in both datasets are retained, and briefly describe the
    resulting dataset; export the result as a CSV to your data
    directory:

Creating a consistent ID variable for both datasets and then combining
them yields only those who are in both datasets. There are 337
participants in both after tidying up the data.

Exporting the result:

``` r
write.csv(df_MCI_combined, file = "results/df_MCI_combined.csv")
```
