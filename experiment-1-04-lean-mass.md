# Experiment 1 - Lean Mass
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
  select(id, cohort, sex, diet, lm_0:lm_21) |>
  
  # Exclude subject 7
  filter(id != 7) |>
  
  # Tidy data
  pivot_longer(
    cols = starts_with("lm"),
    names_to = "day",
    values_to = "lean_mass",
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
    .before = lean_mass,
    )

# View a summary of the data
summary(mydata)
```

           id             cohort        sex             diet         day       
     1      :  4   Ad Libitum:124   Male  :132   CHOW     :60   Min.   : 0.00  
     2      :  4   Pair-Fed  :120   Female:112   HFD-VS   :64   1st Qu.: 5.25  
     3      :  4                                 HFD-COCO :56   Median :10.50  
     4      :  4                                 HFD-C8/10:64   Mean   :10.50  
     5      :  4                                                3rd Qu.:15.75  
     6      :  4                                                Max.   :21.00  
     (Other):220                                                               
       sex_sum         week        lean_mass    
     Male  :132   Min.   :0.00   Min.   :167.6  
     Female:112   1st Qu.:0.75   1st Qu.:201.8  
                  Median :1.50   Median :351.2  
                  Mean   :1.50   Mean   :301.6  
                  3rd Qu.:2.25   3rd Qu.:390.9  
                  Max.   :3.00   Max.   :471.7  
                                                

## Line Plot

``` r
# Create a line plot of each subject's lean mass over time
line_plot <-
  mydata |>  
  ggplot(
    aes(
      x = week, 
      y = lean_mass, 
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
    geom = "errorbar",
    width = 0.2
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
  labs(y = "Lean Mass (g)") +
  
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

![](experiment-1-04-lean-mass_files/figure-commonmark/line-plot-1.png)

The data seem pretty flat over time for all diet groups in both sexes in
both cohorts.

# Summarize

First, let’s create a new dataframe called “model_data” and calculate
the rate of each subject’s body weight gain over time.

``` r
# Create a new data frame called model_data
model_data <-
  
  # Copy the original dataframe
  mydata |>
  
  # Group by subject id
  group_by(id) |>
  
  # Filter the data to include only days 0 or 21
  filter(day == 0 | day == 21) |>

  # Compute the slope of body weight
  mutate(
    lean_mass_slope = 
      (lean_mass - lag(lean_mass)) / (day - lag(day))
  ) |>
  
  # Ungroup by id
  ungroup() |> 
  
  # Select only the relevant columns
  select(
    id, cohort, sex, sex_sum, diet, lean_mass_slope
  ) |>
  
  # Drop na values
  drop_na(lean_mass_slope)

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
     lean_mass_slope   
     Min.   :-1.54757  
     1st Qu.:-0.37319  
     Median : 0.09500  
     Mean   :-0.03575  
     3rd Qu.: 0.31024  
     Max.   : 2.76910  
                       

Looks good.

## Bar Plot

Now, let’s visualize the slopes.

``` r
# Create a bar plot of each group's lean mass slope
bar_plot <-
  model_data |>  
  ggplot(
    aes(
      x     = diet, 
      y     = lean_mass_slope, 
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
    name = "Lean Mass Slope (g/day)",
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

![](experiment-1-04-lean-mass_files/figure-commonmark/bar-plot-1.png)

Consistent with the previous visualization, it looks like male HFD-VS in
the ad libitum cohort has a greater lean mass slope than all other
groups.

# Model

To test the effects of sex, diet, cohort, and their interactions on the
slope of lean mass over time, a multiple linear regression will be built
to predict fat mass gain (in g/day) with sex (sum-coded, Levels: male =
1, female = -1), diet (treatment coded, levels: CHOW, HFD-VS, HFD-COCO,
HFD-C8/10), cohort (treatment coded, levels: ad libitum and pair-fed)
and their interactions.

``` r
# Build a linear model with the outcome variable average energy intake and the predictors sex, diet, cohort, and the interaction between diet and cohort
model <- lm(lean_mass_slope ~ sex_sum + diet * cohort, data = model_data)
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

![](experiment-1-04-lean-mass_files/figure-commonmark/residuals-plot-1.png)

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

![](experiment-1-04-lean-mass_files/figure-commonmark/qq-plot-1.png)

The data seem approximately normally distributed.

## Omnibus Tests

Now that we know that this is a good model, let’s perform omnibus tests
for the effects of each predictor using the function “anova()”. This
function uses type I sum of squares. Thus, it will produce F test
statistics for the effects of each predictor entered sequentially (i.e.,
the residual effect of each predictor after accounting for the effects
of all the other predictors entered in the model before it).

``` r
# Perform omnibus tests
model |> anova()
```

    Analysis of Variance Table

    Response: lean_mass_slope
                Df  Sum Sq Mean Sq F value    Pr(>F)    
    sex_sum      1  0.0078  0.0078  0.0242 0.8770392    
    diet         3  1.9991  0.6664  2.0666 0.1159394    
    cohort       1  4.3689  4.3689 13.5496 0.0005529 ***
    diet:cohort  3  1.1089  0.3696  1.1464 0.3391183    
    Residuals   52 16.7667  0.3224                      
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

There was not a significant effect of either sex,
$F(1, 52) = 0.02, p = .877$, or diet, $F(3, 52) = 2.07, p = .116$. There
was a significant effect of cohort, $F(1, 52) = 13.55, p < .001$. There
was not a significant interaction between diet and cohort
$F(3, 52) = 1.15, p = .339$.

## Reference = CHOW

To probe these effects, let’s take a look at the summary output.

``` r
# Produce summary output
model |> summary()
```


    Call:
    lm(formula = lean_mass_slope ~ sex_sum + diet * cohort, data = model_data)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -1.41853 -0.32736  0.01897  0.36261  2.10323 

    Coefficients:
                                 Estimate Std. Error t value Pr(>|t|)  
    (Intercept)                   0.21556    0.20076   1.074   0.2879  
    sex_sum1                      0.03891    0.07357   0.529   0.5992  
    dietHFD-VS                    0.41140    0.28392   1.449   0.1533  
    dietHFD-COCO                 -0.29320    0.29407  -0.997   0.3234  
    dietHFD-C8/10                -0.10448    0.28392  -0.368   0.7144  
    cohortPair-Fed               -0.23756    0.29407  -0.808   0.4229  
    dietHFD-VS:cohortPair-Fed    -0.72766    0.40870  -1.780   0.0809 .
    dietHFD-COCO:cohortPair-Fed  -0.22631    0.42261  -0.536   0.5946  
    dietHFD-C8/10:cohortPair-Fed -0.22473    0.40870  -0.550   0.5848  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.5678 on 52 degrees of freedom
    Multiple R-squared:  0.3086,    Adjusted R-squared:  0.2023 
    F-statistic: 2.902 on 8 and 52 DF,  p-value: 0.009435

Overall, the model explained 31% of the variance in lean mass gain; and,
after adjusting for the number of predictors, it explained 20%,
$F(8, 52) = 2.90, R^2 = 0.31, R^2_{adj} = 0.20, p = .009$.

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
    lm(formula = lean_mass_slope ~ sex_sum + diet * cohort, data = model_data_vs)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -1.41853 -0.32736  0.01897  0.36261  2.10323 

    Coefficients:
                                 Estimate Std. Error t value Pr(>|t|)   
    (Intercept)                   0.62696    0.20076   3.123  0.00292 **
    sex_sum1                      0.03891    0.07357   0.529  0.59916   
    dietCHOW                     -0.41140    0.28392  -1.449  0.15334   
    dietHFD-COCO                 -0.70460    0.29407  -2.396  0.02021 * 
    dietHFD-C8/10                -0.51588    0.28392  -1.817  0.07498 . 
    cohortPair-Fed               -0.96521    0.28451  -3.393  0.00133 **
    dietCHOW:cohortPair-Fed       0.72766    0.40870   1.780  0.08085 . 
    dietHFD-COCO:cohortPair-Fed   0.50134    0.41562   1.206  0.23318   
    dietHFD-C8/10:cohortPair-Fed  0.50292    0.40152   1.253  0.21597   
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.5678 on 52 degrees of freedom
    Multiple R-squared:  0.3086,    Adjusted R-squared:  0.2023 
    F-statistic: 2.902 on 8 and 52 DF,  p-value: 0.009435

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
    lm(formula = lean_mass_slope ~ sex_sum + diet * cohort, data = model_data_coco)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -1.41853 -0.32736  0.01897  0.36261  2.10323 

    Coefficients:
                                  Estimate Std. Error t value Pr(>|t|)  
    (Intercept)                  -0.077639   0.214879  -0.361   0.7193  
    sex_sum1                      0.038907   0.073570   0.529   0.5992  
    dietCHOW                      0.293199   0.294070   0.997   0.3234  
    dietHFD-VS                    0.704597   0.294070   2.396   0.0202 *
    dietHFD-C8/10                 0.188716   0.294070   0.642   0.5239  
    cohortPair-Fed               -0.463871   0.304248  -1.525   0.1334  
    dietCHOW:cohortPair-Fed       0.226312   0.422614   0.536   0.5946  
    dietHFD-VS:cohortPair-Fed    -0.501343   0.415621  -1.206   0.2332  
    dietHFD-C8/10:cohortPair-Fed  0.001579   0.415621   0.004   0.9970  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.5678 on 52 degrees of freedom
    Multiple R-squared:  0.3086,    Adjusted R-squared:  0.2023 
    F-statistic: 2.902 on 8 and 52 DF,  p-value: 0.009435

# Communicate

``` r
# Save the ...

# line plot
ggsave(
  plot = line_plot,
  filename = "output/experiment-1/04-lean-mass/01-line-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)

# bar plot
ggsave(
  plot = bar_plot,
  filename = "output/experiment-1/04-lean-mass/02-bar-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)

# residuals plot
ggsave(
  plot = residuals_plot,
  filename = "output/experiment-1/04-lean-mass/03-residuals-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)

# qq plot
ggsave(
  plot = qq_plot,
  filename = "output/experiment-1/04-lean-mass/04-qq-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)
```

``` r
# Save the bar plot as an rds file to be imported later and arranged into a single multi-panel plot with all the body composition data
saveRDS(
  bar_plot,
  file = "output/experiment-1/05-body-comp-figure/04-lean-mass.rds"
)
```

# References
