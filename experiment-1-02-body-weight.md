# Experiment 1 - Body Weight
Brent Bachman
2025-05-01

- [<span class="toc-section-number">1</span> Packages](#packages)
- [<span class="toc-section-number">2</span> Data](#data)
  - [<span class="toc-section-number">2.1</span> Import, tidy, and
    transform](#import-tidy-and-transform)
  - [<span class="toc-section-number">2.2</span> Line Plot](#line-plot)
- [<span class="toc-section-number">3</span> Summarize](#summarize)
  - [<span class="toc-section-number">3.1</span> Bar Plot](#bar-plot)
- [<span class="toc-section-number">4</span> Model](#model)
  - [<span class="toc-section-number">4.1</span>
    Assumptions](#assumptions)
    - [<span class="toc-section-number">4.1.1</span> Linearity and
      Homoskedasticity](#linearity-and-homoskedasticity)
    - [<span class="toc-section-number">4.1.2</span>
      Normality](#normality)
  - [<span class="toc-section-number">4.2</span> Omnibus
    Tests](#omnibus-tests)
  - [<span class="toc-section-number">4.3</span> Reference =
    CHOW](#reference--chow)
  - [<span class="toc-section-number">4.4</span> Reference =
    HFD-VS](#reference--hfd-vs)
  - [<span class="toc-section-number">4.5</span> Reference =
    HFD-COCO](#reference--hfd-coco)
- [<span class="toc-section-number">5</span> Communicate](#communicate)
- [<span class="toc-section-number">6</span> References](#references)

# Packages

``` r
library("tidyverse")
```

``` r
# install.packages("tidyverse")
```

``` r
sessionInfo()
```

    R version 4.4.2 (2024-10-31 ucrt)
    Platform: x86_64-w64-mingw32/x64
    Running under: Windows 11 x64 (build 26100)

    Matrix products: default


    locale:
    [1] LC_COLLATE=English_United States.utf8 
    [2] LC_CTYPE=English_United States.utf8   
    [3] LC_MONETARY=English_United States.utf8
    [4] LC_NUMERIC=C                          
    [5] LC_TIME=English_United States.utf8    

    time zone: America/New_York
    tzcode source: internal

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
     [1] lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4    
     [5] purrr_1.0.2     readr_2.1.5     tidyr_1.3.1     tibble_3.2.1   
     [9] ggplot2_3.5.1   tidyverse_2.0.0

    loaded via a namespace (and not attached):
     [1] gtable_0.3.5      jsonlite_1.8.8    compiler_4.4.2    tidyselect_1.2.1 
     [5] scales_1.3.0      yaml_2.3.10       fastmap_1.2.0     R6_2.5.1         
     [9] generics_0.1.3    knitr_1.48        munsell_0.5.1     pillar_1.9.0     
    [13] tzdb_0.4.0        rlang_1.1.4       utf8_1.2.4        stringi_1.8.4    
    [17] xfun_0.47         timechange_0.3.0  cli_3.6.3         withr_3.0.1      
    [21] magrittr_2.0.3    digest_0.6.37     grid_4.4.2        rstudioapi_0.16.0
    [25] hms_1.1.3         lifecycle_1.0.4   vctrs_0.6.5       evaluate_1.0.0   
    [29] glue_1.7.0        fansi_1.0.6       colorspace_2.1-1  rmarkdown_2.28   
    [33] tools_4.4.2       pkgconfig_2.0.3   htmltools_0.5.8.1

# Data

## Import, tidy, and transform

``` r
# Create a dataframe called mydata
mydata <- 
  
  # Import the data
  read_csv(
    file = "data/experiment-1-statsdata.csv",
    show_col_types = FALSE
    ) |>
  
  # Select the variables of interest 
  # ei = energy intake
  select(id, age, cohort, sex, diet, bw_0:bw_24) |>
  
  # Exclude subject 7
  filter(id != 7) |>
  
  # Tidy data
  pivot_longer(
    cols = starts_with("bw"),
    names_to = "day",
    values_to = "body_weight",
    values_drop_na = TRUE
    ) |>
  
  # Transform data
  mutate(
  
    # Recode categorical variables as factors
    id = factor(id),
    cohort = factor(
      cohort,
      levels = c(0, 1),
      labels = c("Ad Libitum", "Pair-Fed")
      ),
    sex = factor(
      sex,
      levels = c(0, 1),
      labels = c("Male", "Female")
      ),
    diet = factor(
      diet,
      levels = c(0, 1, 2, 3),
      labels = c("CHOW", "HFD-VS", "HFD-COCO", "HFD-C8/10")
      ),
    
    # Recode sex using sum coding
    sex_sum = C(sex, sum),
    
    # Compute time in weeks
    day = parse_number(day),
    week = day/7,
    .before = body_weight,
    )

# View a summary of the data
summary(mydata)
```

           id           age               cohort        sex             diet    
     101    : 24   Min.   :142.0   Ad Libitum:372   Male  :612   CHOW     :264  
     102    : 24   1st Qu.:156.0   Pair-Fed  :720   Female:480   HFD-VS   :288  
     103    : 24   Median :173.0                                 HFD-COCO :252  
     104    : 24   Mean   :166.2                                 HFD-C8/10:288  
     105    : 24   3rd Qu.:175.0                                                
     106    : 24   Max.   :176.0                                                
     (Other):948                                                                
          day          sex_sum         week         body_weight   
     Min.   : 0.00   Male  :612   Min.   :0.0000   Min.   :244.9  
     1st Qu.: 5.75   Female:480   1st Qu.:0.8214   1st Qu.:305.5  
     Median :11.50                Median :1.6429   Median :496.9  
     Mean   :11.47                Mean   :1.6388   Mean   :443.4  
     3rd Qu.:17.25                3rd Qu.:2.4643   3rd Qu.:560.1  
     Max.   :24.00                Max.   :3.4286   Max.   :721.4  
                                                                  

``` r
# compute the range of each cohort's age and staring body weight
mydata |>
  filter(day == 0) |>
  group_by(cohort, sex) |>
  summarize(
    age_min = min(age),
    age_max = max(age),
    bw_min  = min(body_weight),
    bw_max  = max(body_weight)
    
  )
```

    `summarise()` has grouped output by 'cohort'. You can override using the
    `.groups` argument.

    # A tibble: 4 × 6
    # Groups:   cohort [2]
      cohort     sex    age_min age_max bw_min bw_max
      <fct>      <fct>    <dbl>   <dbl>  <dbl>  <dbl>
    1 Ad Libitum Male       142     156   495.   617.
    2 Ad Libitum Female     142     156   275.   351.
    3 Pair-Fed   Male       173     176   515    637.
    4 Pair-Fed   Female     173     176   246.   338.

``` r
# compute the total sample size
mydata |>
  distinct(id) |>
  summarize(
    total_sample_size = n()
  )
```

    # A tibble: 1 × 1
      total_sample_size
                  <int>
    1                61

``` r
# compute the grouped sample size
mydata |>
  group_by(cohort, sex, diet) |>
  distinct(id) |> 
  summarize(
    sample_size = n()
  )
```

    `summarise()` has grouped output by 'cohort', 'sex'. You can override using the
    `.groups` argument.

    # A tibble: 16 × 4
    # Groups:   cohort, sex [4]
       cohort     sex    diet      sample_size
       <fct>      <fct>  <fct>           <int>
     1 Ad Libitum Male   CHOW                4
     2 Ad Libitum Male   HFD-VS              4
     3 Ad Libitum Male   HFD-COCO            3
     4 Ad Libitum Male   HFD-C8/10           4
     5 Ad Libitum Female CHOW                4
     6 Ad Libitum Female HFD-VS              4
     7 Ad Libitum Female HFD-COCO            4
     8 Ad Libitum Female HFD-C8/10           4
     9 Pair-Fed   Male   CHOW                4
    10 Pair-Fed   Male   HFD-VS              5
    11 Pair-Fed   Male   HFD-COCO            4
    12 Pair-Fed   Male   HFD-C8/10           5
    13 Pair-Fed   Female CHOW                3
    14 Pair-Fed   Female HFD-VS              3
    15 Pair-Fed   Female HFD-COCO            3
    16 Pair-Fed   Female HFD-C8/10           3

## Line Plot

``` r
# Create a line plot of each group's body weight over time
line_plot <-
  mydata |>  
  ggplot(
    aes(
      x = week, 
      y = body_weight, 
      color = diet, 
      shape = diet
      )
    ) +
  
  # Facet by sex and cohort
  facet_grid(
    sex ~ cohort,
    scales = "free_y"
    ) + 
  
  # Plot the observed means as points
  stat_summary(
    fun = mean,
    geom = "point"
    ) +
  
  # Plot the observed standard errors as errorbars
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar"
    ) +
  
  # Plot connecting lines
  stat_summary(
    fun = mean,
    geom = "line"
    ) +
  
  # Change color title and scale
  scale_color_manual(
    name = "Diet",
    values = c(
      "CHOW"      = "black",
      "HFD-VS"    = "firebrick3",
      "HFD-COCO"  = "darkorchid3",
      "HFD-C8/10" = "dodgerblue3"
      )
    ) +
  
  # Change shape title and scale
  scale_shape_manual(
    name = "Diet",
    values = c(
      "CHOW"      = 15,
      "HFD-VS"    = 16,
      "HFD-COCO"  = 17,
      "HFD-C8/10" = 18
      )
    ) +
  
  # Change y-axis title and scale
  labs(y = "Body Weight (g)") +
  
  # Change x-axis title and scale
  scale_x_continuous(
    name = "Time (Weeks)",
    breaks = seq(from = 0, to = 4, by = 1)) +
  
  # Change overall plot theme
  theme_bw() + 
  
  # Rotate and adjust the x-axis tick labels
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      hjust = 0.5
      )
    )

# Show the plot
line_plot
```

![](experiment-1-02-body-weight_files/figure-commonmark/line-plot-1.png)

The data seem pretty flat over time for chow and HFD-C8/10 in both males
and females in the ad libitum cohort, as well as in all diet groups in
both sexes in the pair-fed cohort. However, the slope seems to be
greater in male and female HFD-VS and HFD-COCO in the ad libitum cohort.

# Summarize

Let’s calculate the slope of each subject’s body weight gain over time.

``` r
# Create a new data frame called model_data
model_data <-
  
  # Copy the original dataframe
  mydata |>
  
  # Group by subject id
  group_by(id) |>
  
  # Filter the data to include only days 0 or 23 or 24
  filter(day == 0 | day == 23 | day == 24) |>

  # Compute the slope of body weight
  mutate(
    body_weight_slope = 
      (body_weight - lag(body_weight)) / (day - lag(day))
  ) |>
  
  # Ungroup by id
  ungroup() |> 
  
  # Select only the relevant columns
  select(
    id, cohort, sex, sex_sum, diet, body_weight_slope
  ) |>
  
  # Drop na values
  drop_na(body_weight_slope)

# Show a summary of the data
summary(model_data)
```

           id            cohort       sex       sex_sum          diet   
     1      : 1   Ad Libitum:31   Male  :33   Male  :33   CHOW     :15  
     2      : 1   Pair-Fed  :30   Female:28   Female:28   HFD-VS   :16  
     3      : 1                                           HFD-COCO :14  
     4      : 1                                           HFD-C8/10:16  
     5      : 1                                                         
     6      : 1                                                         
     (Other):55                                                         
     body_weight_slope 
     Min.   :-1.86956  
     1st Qu.:-0.43478  
     Median : 0.05652  
     Mean   : 0.37228  
     3rd Qu.: 0.94583  
     Max.   : 5.16667  
                       

## Bar Plot

Now, let’s visualize the slopes.

``` r
# Create a bar plot of each group's body weight slope across the dietary intervention
bar_plot <-
  model_data |>  
  ggplot(
    aes(
      x     = diet, 
      y     = body_weight_slope, 
      color = diet, 
      fill  = diet,
      shape = diet
      )
    ) +
  
  # Facet by sex and cohort
  facet_grid(
    sex ~ cohort
    ) + 
  
  # Plot the observed means as points
  stat_summary(
    fun = mean,
    geom = "bar",
    color = "black",
    ) +
  
  # Plot the observed standard errors as errorbars
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    color = "black",
    width = 0.2
    ) +
  
  # Plot individual data points
  geom_point(
    position = position_jitter(
      width = 0.2
    )
  ) +
  
  # Change color title and scale
  scale_color_manual(
    name = "Diet",
    values = c(
      "CHOW"      = "black",
      "HFD-VS"    = "firebrick4",
      "HFD-COCO"  = "darkorchid4",
      "HFD-C8/10" = "dodgerblue4"
      )
    ) +
  
  # Change fill title and scale
  scale_fill_manual(
    name = "Diet",
    values = c(
      "CHOW"      = "ivory3",
      "HFD-VS"    = "firebrick2",
      "HFD-COCO"  = "darkorchid2",
      "HFD-C8/10" = "dodgerblue2"
      )
    ) +
  
  # Change shape title and scale
  scale_shape_manual(
    name = "Diet",
    values = c(
      "CHOW"      = 15,
      "HFD-VS"    = 16,
      "HFD-COCO"  = 17,
      "HFD-C8/10" = 18
      )
    ) +
  
  # Change x-axis title
  labs(x = "Diet") +
  
  # Change y-axis title and scale
  scale_y_continuous(
    name = "Body Weight Slope (g/day)",
    limits = c(-2, 6),
    breaks = seq(from = -2, to = 6, by = 2)
  ) +
  
  # Change overall plot theme
  theme_bw() + 
  
  # Remove x-axis tick labels
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
    )

# Show the plot
bar_plot
```

![](experiment-1-02-body-weight_files/figure-commonmark/bar-plot-1.png)

Consistent with the previous visualization, it looks like HFD-VS has a
greater body weight slope than all other groups in the ad libitum
cohort, but not the pair-fed cohort. It looks like HFD-COCO has a
greater body weight slope than CHOW and HFD-C8/10 in the ad libitum
cohort, but not the pair-fed cohort; and it looks like the body weight
slope of HFD-C8/10 is not different from CHOW in either cohort.

Looks great. Let’s save the plot.

# Model

To test the effects of sex, diet, cohort, and their interactions on the
slope of body weight over time, a multiple linear regression will be
built to predict body weight gain (in g/day) with sex (sum-coded,
Levels: male = 1, female = -1), diet (treatment coded, levels: CHOW,
HFD-VS, HFD-COCO, HFD-C8/10), cohort (treatment coded, levels: ad
libitum and pair-fed) and their interactions.

``` r
# Build a linear model with the outcome variable average energy intake and the predictors sex, diet, cohort, and the interaction between diet and cohort
model <- lm(body_weight_slope ~ sex_sum + diet * cohort, data = model_data)
```

## Assumptions

Before I run any statistical tests, let’s check how well the model
satisfies the assumptions.

First, I need to add the fitted and residual values to the data.

``` r
# Add the fitted and residual values to the dataset
model_data_fits <- 
  model_data |>
  mutate(
    fits = c(fitted(model)),
    resids = c(residuals(model))
  )
```

### Linearity and Homoskedasticity

Next, let’s create a residuals vs fitted plot to check for linearity and
homoskedasticity.

``` r
# Create a residuals plot to check for linearity and homoskedasticity
residuals_plot <-
  model_data_fits |>
  ggplot(
    aes(x = fits, y = resids)
  ) +
  
  # Plot individual data points
  geom_point() +
  
  # Plot a horizontal line at y = 0
  geom_hline(yintercept = 0, col = "red") + 
  
  # Change aesthetics
  labs(
    title = "Residuals vs Predicted Values",
    x = "Predicted Values",
    y = "Residuals"
  ) +
  theme_bw() 

# Show the plot
residuals_plot
```

![](experiment-1-02-body-weight_files/figure-commonmark/residuals-plot-1.png)

The data seem approximately linear and homoskedastic.

### Normality

Finally, let’s create a qq plot to check for normality.

``` r
# Create a QQ plot to check for normality
qq_plot <-
  model_data_fits |>
  ggplot(
    aes(sample = resids)
  ) +
  
  # Plot boxplots
  geom_qq() +
  geom_qq_line(col = "red") +
  
  # Change aesthestics
  labs(
    title = "Normal QQ Plot",
    x = "Theoretical",
    y = "Sample"
  ) +
  theme_bw() 

# Show the plot
qq_plot
```

![](experiment-1-02-body-weight_files/figure-commonmark/qq-plot-1.png)

The data seem approximately normally distributed.

## Omnibus Tests

Now that we know this is a good model, let’s perform omnibus tests for
the effects of each predictor using the function “anova()”. This
function uses type I sum of squares. Thus, it will produce F test
statistics for the effects of each predictor entered sequentially (i.e.,
the residual effect of each predictor after accounting for the effects
of all the other predictors entered in the model before it).

``` r
# Perform omnibus tests
model |> anova()
```

    Analysis of Variance Table

    Response: body_weight_slope
                Df Sum Sq Mean Sq F value    Pr(>F)    
    sex_sum      1  0.020   0.020  0.0305 0.8620019    
    diet         3 16.802   5.601  8.5247 0.0001057 ***
    cohort       1 43.321  43.321 65.9371 8.225e-11 ***
    diet:cohort  3 19.805   6.602 10.0481 2.505e-05 ***
    Residuals   52 34.164   0.657                      
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

When predicting body weight gain over the course of the dietary
intervention, there was not a significant effect of sex,
$F(1, 52) = 0.04, p = .851$, However, after accounting for sex, there
were significant additional effects of diet,
$F(3, 52) = 10.01, p < .001$, cohort, $F(1, 52) = 77.44, p < .001$, and
the interaction between diet and cohort, $F(1, 52) = 10.05, p < .001$.

## Reference = CHOW

Let’s probe these effects by producing the summary output.

``` r
# Produce summary output
model |> summary()
```


    Call:
    lm(formula = body_weight_slope ~ sex_sum + diet * cohort, data = model_data)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -1.95635 -0.48766 -0.00066  0.45670  2.31757 

    Coefficients:
                                 Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                    0.3510     0.2866   1.225  0.22611    
    sex_sum1                       0.1288     0.1050   1.226  0.22561    
    dietHFD-VS                     2.3693     0.4053   5.846 3.37e-07 ***
    dietHFD-COCO                   1.2870     0.4198   3.066  0.00344 ** 
    dietHFD-C8/10                 -0.2286     0.4053  -0.564  0.57506    
    cohortPair-Fed                -0.3930     0.4198  -0.936  0.35343    
    dietHFD-VS:cohortPair-Fed     -2.8687     0.5834  -4.917 9.19e-06 ***
    dietHFD-COCO:cohortPair-Fed   -1.8479     0.6033  -3.063  0.00347 ** 
    dietHFD-C8/10:cohortPair-Fed  -0.4925     0.5834  -0.844  0.40241    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.8106 on 52 degrees of freedom
    Multiple R-squared:  0.7006,    Adjusted R-squared:  0.6545 
    F-statistic: 15.21 on 8 and 52 DF,  p-value: 3.178e-11

Overall, the model explained 70% of the variance in body weight gain;
and, after adjusting for the number of predictors, it explained 66%,
$F(8, 52) = 15.21, R^2 = .70, R^2_{adj} = .66, p < .001$.

Across sex, the difference between the ad libitum and pair-fed cohorts’
CHOW groups was not statistically significant
($B = -0.39, SE - 0.42, p = .353$). However, both HFD-VS
($B = 2.34, SE = 0.41, p < .001$) and HFD-COCO
($B = 1.29, SE = 0.42, p = .003$) gained significantly more body weight
per day than CHOW in the ad libitum cohort. The magnitude of the
difference between CHOW and both HFD-VS
($B = 2.87, SE = 0.58, p < .001$) and HFD-COCO
($B = 1.85, SE = 0.60, p = .004$) was significantly reduced in the
pair-fed cohort. Although HFD-C8/10 gained slightly *less* body weight
per day than CHOW in the ad libitum cohort, this effect was not
statistically significant ($B = -0.23, SE = 0.41, p = .575$); and it was
not significantly different in the pair-fed cohort
($B = -0.49, SE = 0.58, p = .402$).

## Reference = HFD-VS

To directly compare HFD-COCO and HFD-C8/C10 to HFD-VS, let’s relevel the
diet variable to make HFD-VS the reference group, then refit the model
and produce the summary output. (There is no need to rerun the omnibus
tests, as the “total” effects are already captured, and thus the output
would be identical).

``` r
# Relevel factors to change reference group to HFD-VS
model_data_vs <- 
  model_data |>
  mutate(
    diet = fct_relevel(diet, "HFD-VS"), # relevel diet
  )

# Build the model
model_vs <- 
  model |>
  update(
    data = model_data_vs
  )

# Produce the summary output
model_vs |> summary()
```


    Call:
    lm(formula = body_weight_slope ~ sex_sum + diet * cohort, data = model_data_vs)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -1.95635 -0.48766 -0.00066  0.45670  2.31757 

    Coefficients:
                                 Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                    2.7203     0.2866   9.493 6.08e-13 ***
    sex_sum1                       0.1288     0.1050   1.226 0.225614    
    dietCHOW                      -2.3693     0.4053  -5.846 3.37e-07 ***
    dietHFD-COCO                  -1.0823     0.4198  -2.578 0.012802 *  
    dietHFD-C8/10                 -2.5979     0.4053  -6.410 4.31e-08 ***
    cohortPair-Fed                -3.2617     0.4061  -8.031 1.14e-10 ***
    dietCHOW:cohortPair-Fed        2.8687     0.5834   4.917 9.19e-06 ***
    dietHFD-COCO:cohortPair-Fed    1.0208     0.5933   1.721 0.091255 .  
    dietHFD-C8/10:cohortPair-Fed   2.3762     0.5731   4.146 0.000125 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.8106 on 52 degrees of freedom
    Multiple R-squared:  0.7006,    Adjusted R-squared:  0.6545 
    F-statistic: 15.21 on 8 and 52 DF,  p-value: 3.178e-11

Both HFD-COCO ($B = -1.08, SE = 0.42,p = .013$) and HFD-C8/10
($B = -2.60, SE = 0.41, p < .001$) gained significantly less body weight
per day than HFD-VS in the ad libitum cohort. The magnitude of the
difference between HFD-VS and HFD-C8/10
($B = 2.38, SE = 0.57, p < .001$) but not HFD-COCO
($B = 1.02, SE = 0.59, p = .091$) was significantly reduced in the
pair-fed cohort.

## Reference = HFD-COCO

To directly compare HFD-C8/C10 to HFD-COCO, let’s relevel the diet
variable to make HFD-COCO the reference group, then refit the model and
produce the summary output. (There is no need to rerun the omnibus
tests, as the “total” effects are already captured, and thus the output
would be identical).

``` r
# Relevel factors to change reference group to HFD-VS
model_data_coco <- 
  model_data |>
  mutate(
    diet = fct_relevel(diet, "HFD-COCO"), # relevel diet
  )

# Build the model
model_coco <- 
  model |>
  update(
    data = model_data_coco
  )

# Produce the summary output
model_coco |> summary()
```


    Call:
    lm(formula = body_weight_slope ~ sex_sum + diet * cohort, data = model_data_coco)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -1.95635 -0.48766 -0.00066  0.45670  2.31757 

    Coefficients:
                                 Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                    1.6380     0.3067   5.340 2.07e-06 ***
    sex_sum1                       0.1288     0.1050   1.226 0.225614    
    dietCHOW                      -1.2870     0.4198  -3.066 0.003438 ** 
    dietHFD-VS                     1.0823     0.4198   2.578 0.012802 *  
    dietHFD-C8/10                 -1.5156     0.4198  -3.611 0.000687 ***
    cohortPair-Fed                -2.2409     0.4343  -5.160 3.93e-06 ***
    dietCHOW:cohortPair-Fed        1.8479     0.6033   3.063 0.003465 ** 
    dietHFD-VS:cohortPair-Fed     -1.0208     0.5933  -1.721 0.091255 .  
    dietHFD-C8/10:cohortPair-Fed   1.3553     0.5933   2.285 0.026453 *  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.8106 on 52 degrees of freedom
    Multiple R-squared:  0.7006,    Adjusted R-squared:  0.6545 
    F-statistic: 15.21 on 8 and 52 DF,  p-value: 3.178e-11

Lastly, HFD-C8/10 gained significantly less body weight per day than
HFD-COCO in the ad libitum cohort ($B = -1.52, SE = 0.42, p < .001$);
and the magnitude of this difference was reduced in the pair-fed cohort
($B = 1.36, SE = 0.59, p = .027$).

# Communicate

Let’s save the plots.

``` r
# Save the ...

# line plot
ggsave(
  plot = line_plot,
  filename = "output/experiment-1/02-body-weight/01-line-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)

# bar plot
ggsave(
  plot = bar_plot,
  filename = "output/experiment-1/02-body-weight/02-bar-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)

# residuals plot
ggsave(
  plot = residuals_plot,
  filename = "output/experiment-1/02-body-weight/03-residuals-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)

# qq plot
ggsave(
  plot = qq_plot,
  filename = "output/experiment-1/02-body-weight/04-qq-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)
```

``` r
# Save the bar plot as an rds file to be imported later and arranged into a single multi-panel plot with all the body composition data
saveRDS(
  bar_plot,
  file = "output/experiment-1/05-body-comp-figure/02-body-weight.rds"
)
```

# References
