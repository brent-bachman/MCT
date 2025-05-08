# Experiment 2 - Body Weight
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
    file ="data/experiment-2-statsdata.csv",
    show_col_types = FALSE
    ) |>
  
  # Select the variables of interest 
  # bw = body weight
  select(id, sex, diet, bw_0:bw_30) |>
  
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
    sex = factor(
      sex,
      levels = c(0, 1),
      labels = c("Male", "Female")
      ),
    diet = factor(
      diet,
      levels = c(0, 1, 2),
      labels = c("CHOW", "HFD-VS", "HFD-C8/10")
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

           id           sex             diet          day          sex_sum   
     1      :  30   Male  :699   CHOW     :480   Min.   : 0.00   Male  :699  
     2      :  30   Female:720   HFD-VS   :459   1st Qu.: 7.00   Female:720  
     3      :  30                HFD-C8/10:480   Median :15.00               
     4      :  30                                Mean   :14.96               
     5      :  30                                3rd Qu.:23.00               
     7      :  30                                Max.   :30.00               
     (Other):1239                                                            
          week        body_weight   
     Min.   :0.000   Min.   :178.3  
     1st Qu.:1.000   1st Qu.:236.7  
     Median :2.143   Median :294.7  
     Mean   :2.138   Mean   :311.3  
     3rd Qu.:3.286   3rd Qu.:379.0  
     Max.   :4.286   Max.   :537.6  
                                    

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
  
  # Facet by sex
  facet_grid(
    sex ~ .,
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
      "HFD-C8/10" = "dodgerblue3"
      )
    ) +
  
  # Change shape title and scale
  scale_shape_manual(
    name = "Diet",
    values = c(
      "CHOW"      = 15,
      "HFD-VS"    = 16,
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

![](experiment-2-02-body-weight_files/figure-commonmark/line-plot-1.png)

The data for all groups seems to increase linearly over time. However,
the slope seems to be greater for HFD-VS than the other two groups in
both sexes.

# Summarize

First, let’s create a new dataframe called “model_data” and calculate
the rate of each subject’s body weight gain over time.

``` r
# Create a new data frame called model_data
model_data <-
  
  # Copy the original dataframe
  mydata |>
  
  # Filter the data to include only days 0 or 30
  filter(day == 0 | day == 30) |>

  # Compute each subject's body weight slope
  group_by(id) |>
  mutate(
    body_weight_slope = 
      (body_weight - lag(body_weight)) / (day - lag(day))
  ) |>
  ungroup() |> 
  
  # Select only the relevant columns
  select(
    id, sex, sex_sum, diet, body_weight_slope
  ) |>
  
  # Drop na values
  drop_na(body_weight_slope)

# Show a summary of the data
summary(model_data)
```

           id         sex       sex_sum          diet    body_weight_slope
     1      : 1   Male  :23   Male  :23   CHOW     :16   Min.   :0.970    
     2      : 1   Female:24   Female:24   HFD-VS   :15   1st Qu.:1.578    
     3      : 1                           HFD-C8/10:16   Median :2.867    
     4      : 1                                          Mean   :2.793    
     5      : 1                                          3rd Qu.:3.495    
     7      : 1                                          Max.   :6.363    
     (Other):41                                                           

## Bar Plot

``` r
# Create a bar plot of each group's body weight slope
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
  
  # Facet by sex
  facet_grid(
    . ~ sex,
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
      "HFD-C8/10" = "dodgerblue4"
      )
    ) +
  
  # Change fill title and scale
  scale_fill_manual(
    name = "Diet",
    values = c(
      "CHOW"      = "ivory3",
      "HFD-VS"    = "firebrick2",
      "HFD-C8/10" = "dodgerblue2"
      )
    ) +
  
  # Change shape title and scale
  scale_shape_manual(
    name = "Diet",
    values = c(
      "CHOW"      = 15,
      "HFD-VS"    = 16,
      "HFD-C8/10" = 18
      )
    ) +
  
  # Change y-axis title and scale
  scale_y_continuous(
    name = "Body Weight Slope (g/day)",
    limits = c(0, 8),
    breaks = seq(from = 0, to = 8, by = 2)
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

![](experiment-2-02-body-weight_files/figure-commonmark/bar-plot-1.png)

It looks like HFD-VS has a greater body weight slope than the other
groups in both sexes.

# Model

To test the effects of sex, diet, and their interaction on the slope of
body weight (i.e., body weight gain) over the course of the dietary
intervention, a multiple linear regression model will be built to
predict body weight gain (in g/day) with sex (sum-coded, Levels: male =
1, female = -1), diet (treatment coded, levels: CHOW, HFD-VS, and
HFD-C8/10), and their interaction.

``` r
# Build the model
model <- lm(
  body_weight_slope ~ sex_sum * diet, 
  data = model_data
  )
```

## Assumptions

Before I run any statistical tests, let’s check how well the model
satisfies the assumptions.

First, I need to add the fitted and residual values to the data.

``` r
# Add the fitted and residual values to the dataset
model_fits <- 
  model_data |>
  mutate(
    fits = c(fitted(model)),
    resids = c(residuals(model))
  )
```

### Linearity and Homoskedasticity

Next, I will create a residuals plot to check for linearity and
homoskedasticity.

``` r
# Create a residuals plot to check for linearity and homoskedasticity
residuals_plot <-
  model_fits |>
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

![](experiment-2-02-body-weight_files/figure-commonmark/residuals-plot-1.png)

The data seem to be linear and homoskedastic.

### Normality

Next, I will create a QQ plot to check for normality.

``` r
# Create a QQ plot to check for normality
qq_plot <-
  model_fits |>
  ggplot(
    aes(sample = resids)
  ) +
  
  # Plot points and qq line
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

![](experiment-2-02-body-weight_files/figure-commonmark/qq-plot-1.png)

The data seem to mostly fall on the qq line (although there is some
deviance on the lower end), indicating that the data are approximately
normally distributed.

## Omnibus Tests

With the model built, let’s perform omnibus tests for the effects of
each predictor using the function “anova()”. This function uses type I
sum of squares. Thus, it will produce F test statistics for the effects
of each predictor entered sequentially (i.e., the residual effect of
each predictor after accounting for the effects of all the other
predictors entered in the model before it).

``` r
# Perform omnibus F tests
model |> anova()
```

    Analysis of Variance Table

    Response: body_weight_slope
                 Df Sum Sq Mean Sq  F value    Pr(>F)    
    sex_sum       1 48.026  48.026 185.5277 < 2.2e-16 ***
    diet          2 27.571  13.785  53.2544 3.993e-12 ***
    sex_sum:diet  2  2.411   1.205   4.6567   0.01505 *  
    Residuals    41 10.613   0.259                       
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

There was a main effect of sex, $F(1, 41) = 185.53, p < .001$; there was
a main effect of diet, $F(2, 41) = 53.25, p < .001$; and there was a
two-way interaction between sex and diet, $F(2, 41) = 4.66, p = .015$.

## Reference = CHOW

Let’s produce the summary output.

``` r
# Show summary output
model |> summary()
```


    Call:
    lm(formula = body_weight_slope ~ sex_sum * diet, data = model_data)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -1.59708 -0.23708  0.01625  0.30687  1.10810 

    Coefficients:
                           Estimate Std. Error t value Pr(>|t|)    
    (Intercept)              2.4873     0.1272  19.555  < 2e-16 ***
    sex_sum1                 1.0319     0.1272   8.112 4.65e-10 ***
    dietHFD-VS               1.4432     0.1831   7.884 9.60e-10 ***
    dietHFD-C8/10           -0.3729     0.1799  -2.073   0.0445 *  
    sex_sum1:dietHFD-VS      0.2928     0.1831   1.600   0.1174    
    sex_sum1:dietHFD-C8/10  -0.2658     0.1799  -1.478   0.1471    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.5088 on 41 degrees of freedom
    Multiple R-squared:  0.8802,    Adjusted R-squared:  0.8656 
    F-statistic: 60.27 on 5 and 41 DF,  p-value: < 2.2e-16

Overall, the model explained 88% of the variance in average energy
intake; and, after adjusting for the number of predictors, it explained
87%, $F(5, 41) = 60.27, R^2 = 0.88, R^2_{adj} = 0.87, p < .001$.

Across the sexes, HFD-VS gained significantly more body weight per day
than CHOW ($B = 1.44, SE = 0.18, p < .001$); and HFD-C8/10 gained
significantly *less* body weight per day than CHOW
($B = -0.37, SE = 0.18, p = .045$). Between the sexes, male CHOW gained
more body weight per day than female CHOW
($B = 1.03, SE = 0.13, p < .001$). Although the magnitude of the
difference between HFD-VS and CHOW was greater in males than females,
this difference was not statistically significant
($B = 0.29, SE = 0.18, p = .117$). . Although the magnitude of the
difference between HFD-C8/10 and CHOW was greater in males than females,
this difference was not statistically significant
($B = -0.27, SE = 0.18, p = .147$).

## Reference = HFD-VS

To compare HFD-C8/C10 with HFD-VS, let’s relevel the diet variable to
make HFD-VS the reference group, then refit the model and produce the
summary output. (There is no need to rerun the omnibus tests, as the
“total” effects are already captured, and thus the output would be
identical).

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
    lm(formula = body_weight_slope ~ sex_sum * diet, data = model_data_vs)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -1.59708 -0.23708  0.01625  0.30687  1.10810 

    Coefficients:
                           Estimate Std. Error t value Pr(>|t|)    
    (Intercept)              3.9305     0.1317  29.854  < 2e-16 ***
    sex_sum1                 1.3247     0.1317  10.062 1.23e-12 ***
    dietCHOW                -1.4432     0.1831  -7.884 9.60e-10 ***
    dietHFD-C8/10           -1.8162     0.1831  -9.921 1.85e-12 ***
    sex_sum1:dietCHOW       -0.2928     0.1831  -1.600  0.11737    
    sex_sum1:dietHFD-C8/10  -0.5587     0.1831  -3.052  0.00398 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.5088 on 41 degrees of freedom
    Multiple R-squared:  0.8802,    Adjusted R-squared:  0.8656 
    F-statistic: 60.27 on 5 and 41 DF,  p-value: < 2.2e-16

Across the sexes, HFD-C8/10 gained significantly less body weight per
day than HFD-VS ($B = -1.82, SE = 0.18, p < .001$); and the magnitude of
this difference was greater in males than females
($B = -0.56, SE = 0.18, p = .004$).

# Communicate

Let’s save the plots.

``` r
# Save the ...

# line plot
ggsave(
  plot = line_plot,
  filename = "output/experiment-2/02-body-weight/01-line-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)

# bar plot
ggsave(
  plot = bar_plot,
  filename = "output/experiment-2/02-body-weight/02-bar-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)

# residuals plot
ggsave(
  plot = residuals_plot,
  filename = "output/experiment-2/02-body-weight/03-residuals-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)

# qq plot
ggsave(
  plot = qq_plot,
  filename = "output/experiment-2/02-body-weight/04-qq-plot.tiff",
  width = 6, height = 3.708, units = "in", dpi = 300
)
```

``` r
# Save the bar plot as an rds file to be imported later and arranged into a single multi-panel plot with all the body composition data
saveRDS(
  bar_plot,
  file = "output/experiment-2/05-body-comp-figure/02-body-weight.rds"
)
```

# References
