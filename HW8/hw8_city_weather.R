# HW8: City Weather Prediction
#
# This exercise is based on historical hourly weather data 2012-2017 obtained from
# [Kaggle]
# (https://www.kaggle.com/selfishgene/historical-hourly-weather-data#wind_direction.csv).
# The dataset contains approximatelyl 5 years of high temporal resolution
# data of various weather attributes, such as temperature, humidity,
# air pressure, etc. This data is available for 30 US and Canadian Cities,
# as well as 6 Israeli cities.
# Each attribute has its own file and is organized such that the rows are
# the time axis (it's the same time axis for all files), and the columns are
# the different cities (it's the same city ordering for all files as well).
#
# 1. Load the packages `tidyverse`, `lubridate`.
#    Use `read_csv` to load the file `temperature_cities.csv` located in the
#    folder `/course/data`.
#    Store the data into a tibble `temperature`.
#    `temperature` should print to:
#    # A tibble: 45,253 x 5
#    datetime            Vancouver Phoenix Miami `New York`
#    <dttm>                  <dbl>   <dbl> <dbl>      <dbl>
#    1 2012-10-01 12:00:00       NA      NA    NA         NA
#    2 2012-10-01 13:00:00      285.    297.  300.       288.
#    3 2012-10-01 14:00:00      285.    297.  300.       288.
#    4 2012-10-01 15:00:00      285.    297.  300.       288.
#    5 2012-10-01 16:00:00      285.    297.  300.       288.
#    6 2012-10-01 17:00:00      285.    297.  300.       288.
#    7 2012-10-01 18:00:00      285.    297.  300.       289.
#    8 2012-10-01 19:00:00      285.    297.  300.       289.
#    9 2012-10-01 20:00:00      285.    297.  300.       289.
#    10 2012-10-01 21:00:00      285.    297.  300.       289.
# … with 45,243 more rows
## Do not modify this line!
library(tidyverse)
library(lubridate)
temperature<-read_csv('/course/data/temperature_cities.csv')


# 2. As you can see, each city is represented by each column of the tibbles
#    (i.e., humidity variable is spread across multiple columns).
#    Our goal is to create a tibble `temperature_full` with only one column
#    that contains the data from all cities.
#    We need to tidy it. You can use :
#      - `pivot_longer()` to make rowise record for each city.
#      - `mutate()` to add the following extra columns:
#        - `temperature`: convert `temperature` from Kelvin to Celsius
#           scale. (Kelvin - 273.15 = Celsius)
#        - `yday` using `yday()` that contain the day of the corresponding
#           date in `datetime`.
#        - `hour` using `hour()` that contain the hour of the corresponding
#           date in `datetime` and `factor()` to make it a factor variable.
#        - `yearfrac` that represent the fraction of this day in this year.
#           (Use `yday(make_date(year(datetime), 12, 31))` to calculate the
#           total days in this year. The fraction is just the `yday` divided
#           by total days in this year.)
#      - `drop_na()` to drop `NA` values of this tibble.
#    `temperature_full` should print to:
#    # A tibble: 178,616 x 6
#    datetime            city      temperature  yday yearfrac hour
#    <dttm>              <chr>           <dbl> <dbl>    <dbl> <fct>
#    1 2012-10-01 13:00:00 Vancouver        11.5   275    0.751 13
#    2 2012-10-01 13:00:00 Phoenix          23.5   275    0.751 13
#    3 2012-10-01 13:00:00 Miami            26.6   275    0.751 13
#    4 2012-10-01 13:00:00 New York         15.1   275    0.751 13
#    5 2012-10-01 14:00:00 Vancouver        11.5   275    0.751 14
#    6 2012-10-01 14:00:00 Phoenix          23.5   275    0.751 14
#    7 2012-10-01 14:00:00 Miami            26.6   275    0.751 14
#    8 2012-10-01 14:00:00 New York         15.1   275    0.751 14
#    9 2012-10-01 15:00:00 Vancouver        11.5   275    0.751 15
#    10 2012-10-01 15:00:00 Phoenix          23.5   275    0.751 15
#    # … with 178,606 more rows
#    Filter New York temperature and store the tibble into `temperature_ny`.
#    (You can use `filter()`).
#    `temperature_ny` should print to:
#    # A tibble: 44,460 x 6
#    datetime            city     temperature  yday yearfrac hour
#    <dttm>              <chr>          <dbl> <dbl>    <dbl> <fct>
#    1 2012-10-01 13:00:00 New York        15.1   275    0.751 13
#    2 2012-10-01 14:00:00 New York        15.1   275    0.751 14
#    3 2012-10-01 15:00:00 New York        15.2   275    0.751 15
#    4 2012-10-01 16:00:00 New York        15.3   275    0.751 16
#    5 2012-10-01 17:00:00 New York        15.3   275    0.751 17
#    6 2012-10-01 18:00:00 New York        15.4   275    0.751 18
#    7 2012-10-01 19:00:00 New York        15.5   275    0.751 19
#    8 2012-10-01 20:00:00 New York        15.6   275    0.751 20
#    9 2012-10-01 21:00:00 New York        15.7   275    0.751 21
#    10 2012-10-01 22:00:00 New York        15.7   275    0.751 22
#    # … with 44,450 more rows
## Do not modify this line!
temperature_full<-temperature%>%
  pivot_longer(cols = 2:5,
               names_to = 'city',
               values_to = 'temperature')%>%
  mutate(temperature=temperature-273.15)%>%
  mutate(yday=yday(datetime))%>%
  mutate(yearfrac=yday/yday(make_date(year(datetime), 12, 31)))%>%
  mutate(hour=factor(hour(datetime)))%>%
  drop_na()

temperature_full

temperature_ny<-temperature_full%>%
  filter(city=="New York")
# 3. Plot New York temperature from 2012 to 2018.
#    To do that, you can use:
#       - `ggplot()` to initialize the plot object and set `aes()` so that
#          the plot will show `temperature` on y axis and `datetime` on `x`
#          axis.
#       - `geom_line()` to draw a line plot
#       - `labs()` to set `title` as
#            `"New York temperature trend from 2012 to 2018"`,
#            `y` as `"Temperature (C)"` and `x` as `"Date"`.
#       - `theme_light()` to set the light theme.
#    Save the generated plot into `ny_temp`.
## Do not modify this line!
ny_temp<-ggplot(data=temperature_ny,aes(x=datetime,y=temperature))+
  geom_line()+
  labs(title='New York temperature trend from 2012 to 2018',
       x='Date',
       y='Temperature (C)')+
  theme_light()
ny_temp


# 4. Load package `modelr`. Let's fit the temperature using a polynomial
#    model first.
#     - Create a linear model object using `lm()`:
#       - set the relationship of variables  to be `temperature ~ poly(yday, 5)`
#       - set argument `data` to be `temperature_ny`.
#    Save the generated model object to `model0`.
#     - Use `add_predictions()` and `add_residuals()` to add two
#       new columns to `temperature_ny_warming` representing the prediction
#       result of `model0` we defined.
#    Save the generated tibble into `temperature_ny_model0`.
#    `temperature_ny_model0` should print to:
#    # A tibble: 44,460 x 8
#    datetime            city     temperature  yday yearfrac hour   pred resid
#    <dttm>              <chr>          <dbl> <dbl>    <dbl> <fct> <dbl> <dbl>
#    1 2012-10-01 13:00:00 New York        15.1   275    0.751 13     17.2 -2.08
#    2 2012-10-01 14:00:00 New York        15.1   275    0.751 14     17.2 -2.06
#    3 2012-10-01 15:00:00 New York        15.2   275    0.751 15     17.2 -1.98
#    4 2012-10-01 16:00:00 New York        15.3   275    0.751 16     17.2 -1.90
#    5 2012-10-01 17:00:00 New York        15.3   275    0.751 17     17.2 -1.82
#    6 2012-10-01 18:00:00 New York        15.4   275    0.751 18     17.2 -1.74
#    7 2012-10-01 19:00:00 New York        15.5   275    0.751 19     17.2 -1.66
#    8 2012-10-01 20:00:00 New York        15.6   275    0.751 20     17.2 -1.58
#    9 2012-10-01 21:00:00 New York        15.7   275    0.751 21     17.2 -1.50
#    10 2012-10-01 22:00:00 New York        15.7   275    0.751 22     17.2 -1.42
#    # … with 44,450 more rows
## Do not modify this line!
library(modelr)
model0<-lm(temperature~poly(yday,5),data=temperature_ny)
temperature_ny_model0<-temperature_ny%>%
  add_predictions(model0)%>%
  add_residuals(model0)
temperature_ny_model0


# 5. Visualize our model performance, showing the original data points,
#    the fitted line, the residual and scale the color according to residual.
#    Specifically, you can do the following:
#    First, draw the scatter plot of the original dataset.
#      To do that, you can use:
#        - `ggplot()` to set the variables of interest: `datetime` and
#          `temperature`.
#        - `geom_point()` to plot the original data points. Set arguments
#          `size` to be 0.2 to shrink the points and `alpha` to be 0.2.
#        - `geom_point()` with `aes(y=pred)` to draw the predicted data points.
#           Set arguments `size` to be 1 and `col` to be `"red"`.
#        - `labs()` to add titles and labels. Inside `labs()`, set `title` to
#          `"The seasonality is accounted for"`, `subtitle` to
#          `"But polynomials in red miss the cyclicality (boundary effect)"`,
#          `y` to `Temperature (C)`, and `x` to `"Date"`.
#        - `theme_light()` to set the light theme.
#    Save the generated plot object to `ny_temp_fitted`.
## Do not modify this line!
ny_temp_fitted<-ggplot(data=temperature_ny_model0,aes(x=datetime,y=temperature))+
  geom_point(size=0.2,alpha=0.2)+
  geom_line(aes(y=pred),size=1,col='red')+
  labs(title='The seasonality is accounted for',
       subtitle = 'But polynomials in red miss the cyclicality (boundary effect)',
       x='Date',
       y='Temperature (C)')+
  theme_light()

ny_temp_fitted
# 6. We now want to be able to capture the cyclicality.
#    Fit a linear model of `temperature` against the cyclical transformation
#    of `yday`.
#    First, create the following function :
#    `cyclic <- function(yearfrac) mgcv::cSplineDes(yearfrac, seq(0, 1, 0.2))[, -5]`
#    Then fit linear model on the transformed variables using `lm()` and
#    `cyclic()`, that is regress the `temperature` and `cyclic(yearfrac)`.
#    Save the fitted model in `model1`.
#    Then:
#     - Use `add_predictions()` and `add_residuals()` to add two
#       new columns to `temperature_ny` representing the prediction
#       result of `model1` we defined.
#    Save the generated tibble into `temperature_ny_model1`.
#    `temperature_ny_model1` should print to :
#    # A tibble: 44,460 x 8
#    datetime            city     temperature  yday yearfrac hour   pred resid
#    <dttm>              <chr>          <dbl> <dbl>    <dbl> <fct> <dbl> <dbl>
#    1 2012-10-01 13:00:00 New York        15.1   275    0.751 13     17.3 -2.22
#    2 2012-10-01 14:00:00 New York        15.1   275    0.751 14     17.3 -2.19
#    3 2012-10-01 15:00:00 New York        15.2   275    0.751 15     17.3 -2.11
#    4 2012-10-01 16:00:00 New York        15.3   275    0.751 16     17.3 -2.03
#    5 2012-10-01 17:00:00 New York        15.3   275    0.751 17     17.3 -1.95
#    6 2012-10-01 18:00:00 New York        15.4   275    0.751 18     17.3 -1.87
#    7 2012-10-01 19:00:00 New York        15.5   275    0.751 19     17.3 -1.79
#    8 2012-10-01 20:00:00 New York        15.6   275    0.751 20     17.3 -1.71
#    9 2012-10-01 21:00:00 New York        15.7   275    0.751 21     17.3 -1.64
#    10 2012-10-01 22:00:00 New York        15.7   275    0.751 22     17.3 -1.56
#    # … with 44,450 more rows
## Do not modify this line!
cyclic <- function(yearfrac) mgcv::cSplineDes(yearfrac, seq(0, 1, 0.2))[, -5]
model1<-lm(temperature~cyclic(yearfrac),data=temperature_ny_model0)
temperature_ny_model1<-temperature_ny%>%
  add_predictions(model1)%>%
  add_residuals(model1)



# 7. Now, let's compare the predictions of `model0` and `model1`.
#    To do that, you can :
#       - add a plot `geom_line()` to previous plot of `ny_temp_fitted`.
#         Inside `geom_line()`, set `data` to `temperature_ny_model1`,
#         with `aes(y = pred)` and  `color` to `"green"` and `size` to 1.
#       - `labs()` to set `subtitle` to
#          `"Polynomials in red miss the cyclicality, but cyclic`
#          `splines in green capture it properly"`
#       - `theme_light()` to set a light background.
#    Save the ggplot object into `ny_temp_fitted2`.
## Do not modify this line!
ny_temp_fitted2<-ny_temp_fitted+
  geom_line(data=temperature_ny_model1,aes(y=pred),color='green',size=1)+
  labs(subtitle = 'Polynomials in red miss the cyclicality, but cyclicsplines in\n    green capture it properly')+
  theme_light()

ny_temp_fitted2
# 8. Use boxplot to visualize the residuals over each hour of the day.
#    To do that, you can use:
#      - `ggplot()` to set the `x` axis to be the hour of the time
#        and `y` axis to be the residual.
#      - `geom_boxplot()` to draw the plot.
#      - `labs()` to set `title` to
#          `"There is a time-of-day effect in the residuals"`,
#          `x` to be `"Hour"` and `y` to be `"Residuals (C)"`.
#    Save the generated plot to `ny_temp_residual`.
## Do not modify this line!
ny_temp_residual<-ggplot(data=temperature_ny_model1,aes(x=hour,y=resid))+
  geom_boxplot()+
  labs(title='There is a time-of-day effect in the residuals',
       x='Hour',
       y='Residuals (C)')
ny_temp_residual


# 9. We now want to be able to learn the local variations of the temperature
#    by adding `hour` as an intercept in the regression.
#    The goal is to account for the variations between day and night.
#    To do that, declare a new model called `model2` using `lm()` and `cyclic()`.
#    Save the model to `model2`.
#    Combine the prediction result of `model1` and `model2` in the following steps:
#     - use `gather_predictions()` to get the prediction results from both
#       `model1` and `model2`.
#     - use `gather_residuals()` to get the prediction residuals from both
#       `model1` and `model2`.
#     - use `left_join()` to join the above two tibbles.
#     - use `mutate()` and `factor()` to convert `model` column into a factor
#       with `levels()` set to `c("model1", "model2")`.
#    Save the generated tibble into `temperature_ny_model2`.
#    `temperature_ny_model2` should print to:
#    # A tibble: 88,920 x 9
#    model datetime            city  temperature  yday yearfrac hour
#    <fct> <dttm>              <chr>       <dbl> <dbl>    <dbl> <fct>
#    1 mode… 2012-10-01 13:00:00 New …        15.1   275    0.751 13
#    2 mode… 2012-10-01 14:00:00 New …        15.1   275    0.751 14
#    3 mode… 2012-10-01 15:00:00 New …        15.2   275    0.751 15
#    4 mode… 2012-10-01 16:00:00 New …        15.3   275    0.751 16
#    5 mode… 2012-10-01 17:00:00 New …        15.3   275    0.751 17
#    6 mode… 2012-10-01 18:00:00 New …        15.4   275    0.751 18
#    7 mode… 2012-10-01 19:00:00 New …        15.5   275    0.751 19
#    8 mode… 2012-10-01 20:00:00 New …        15.6   275    0.751 20
#    9 mode… 2012-10-01 21:00:00 New …        15.7   275    0.751 21
#    10 mode… 2012-10-01 22:00:00 New …        15.7   275    0.751 22
#    # … with 88,910 more rows, and 2 more variables: pred <dbl>,
#    #   resid <dbl>
## Do not modify this line!
model2<-lm(temperature~cyclic(yearfrac)+hour,data=temperature_ny)
temperature_ny_model2<-
  left_join(gather_predictions(temperature_ny,model1,model2),
            gather_residuals(temperature_ny,model1,model2))%>%
  mutate(model=factor(model,levels = c("model1","model2")))
temperature_ny_model2
# 10. Let's plot the residual comparison of `model1` and `model2`.
#     To do that, you can use :
#      - `ggplot()` to set the `x` axis to be the hour of the time
#         and `y` axis to be the residual.
#      - `geom_boxplot()` to draw the plot.
#      - `facet_wrap()` to draw individual plot for each model.
#      - `labs()` to set `title` to
#          `"Including the hour captures the time-of-day effect"`,
#          `x` to be `"Hour"` and `y` to be `"Residuals (C)"`.
#     Save the ggplot object into `ny_temp_residual2`.
#     We notice that the splines seem to learn local variations in the data,
#     without overfitting.
## Do not modify this line!
ny_temp_residual2<-ggplot(data=temperature_ny_model2 ,aes(x=hour,y=resid))+
  geom_boxplot()+
  facet_wrap(~model)+
  labs(title = "Including the hour captures the time-of-day effect",
       x='Hour',
       y='Residuals (C)')
ny_temp_residual2


# 11. Use `anova()` to compare the variance of `model1` and `model2`. Save
#     the result to `compare_models`.
#     `compare_models` should print to :
#     Analysis of Variance Table
#
#     Model 1: temperature ~ cyclic(yearfrac)
#     Model 2: temperature ~ cyclic(yearfrac) + hour
#     Res.Df     RSS Df Sum of Sq      F    Pr(>F)
#     1  44455 1184509
#     2  44432  944015 23    240495 492.15 < 2.2e-16 ***
#       ---
#       Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## Do not modify this line!
compare_models<-anova(model1,model2)


# 12. Now, we choose `model2` as our best model to fit temperature trend.
#     We want to deploy it to model multiple cities at once.
#     To do that, please do the following:
#       - create a function `my_model` that takes tibble
#         `df` as argument and return a fitted `model2` on the tibble.
#         (use the same formula as `model2`, `temperature ~ cyclic(yearfrac)`).
#         You can assume that `df` has columns `yearfrac` and `temperature`.
#       - Use `temperature_full`, fit `model2` on each city by doing
#         the following:
#          - use `group_by()` and `nest()` to group tibbles according to
#            city.
#          - use `mutate()` and `purrr::map()` to fit the model to each city
#            in `data` using `my_model` function.
#     Save the defined function as `my_model` and the generated tibble as
#     `by_city`. It should print to:
#     # A tibble: 4 x 3
#     # Groups:   city [4]
#     city     data                                                model
#     <chr>    <S3: vctrs_list_of>                                 <list>
#     1 Vancouv… 1.349096e+09, 1.349100e+09, 1.349104e+09, 1.349107… <S3: l…
#     2 Phoenix  1.349096e+09, 1.349100e+09, 1.349104e+09, 1.349107… <S3: l…
#     3 Miami    1.349096e+09, 1.349100e+09, 1.349104e+09, 1.349107… <S3: l…
#     4 New York 1.349096e+09, 1.349100e+09, 1.349104e+09, 1.349107… <S3: l…
## Do not modify this line!
my_model<-function(df){
  return(lm(temperature~cyclic(yearfrac)+hour,data=df))
}
by_city<-temperature_full%>%
  group_by(city)%>%
  nest()%>%
  mutate(model=map(data,my_model))
by_city


# 13. Load package `broom`. Use `broom` to output organized summary of our
#     models.
#     To do that, do the following:
#       - create new column `glance` using `mutate()`, and `map()`
#         to apply `glance()` to `data`.
#       - `unnest()` the column `glance`.
#       - `dplyr::select()` every column except `data` and `model`.
#       - `arrange()` the order the records by `adj.r.squared`.
#     Save the generated tibble into `by_city_stats`.
#     `by_city_stats` should print to:
#     # A tibble: 4 x 12
#     # Groups:   city [4]
#     city  r.squared adj.r.squared sigma statistic p.value    df  logLik
#     <chr>     <dbl>         <dbl> <dbl>     <dbl>   <dbl> <int>   <dbl>
#     1 Miami     0.611         0.611  2.61     2588.       0    28 -1.06e5
#     2 New …     0.797         0.797  4.61     6451.       0    28 -1.31e5
#     3 Vanc…     0.798         0.798  2.99     6501.       0    28 -1.12e5
#     4 Phoe…     0.848         0.848  3.87     9337.       0    28 -1.25e5
#     # … with 4 more variables: AIC <dbl>, BIC <dbl>, deviance <dbl>,
#     #   df.residual <int>
## Do not modify this line!
library(broom)
by_city_stats<-by_city%>%
  mutate(glance=map(model,glance))%>%
  unnest(glance)%>%
  select(-data,-model)%>%
  arrange(adj.r.squared)
by_city_stats


# 14. Now, we want to test the model performance in four seasons using simulated data.
#     We will generate our test data, which consists of `yearfrac` ranging from
#     0 to 0.75 with interval 0.25 and hour pulled from `temperature_full`.
#     To do that, you can use:
#       - `crossing` in which we set:
#          - `yearfrac` to be a vector generated from `seq(0, 0.75, 0.25)`.
#          - `hour` using `pull(hour)` on `temperature_full` and then
#            `unique()` to select unique hours.
#     Save the tibble into `test_df`.
#     `test_df` should print to:
#     # A tibble: 96 x 2
#    yearfrac hour
#    <dbl> <fct>
#    1        0 0
#    2        0 1
#    3        0 2
#    4        0 3
#    5        0 4
#    6        0 5
#    7        0 6
#    8        0 7
#    9        0 8
#    10        0 9
#    # … with 86 more rows
## Do not modify this line!
test_df<-crossing(yearfrac=seq(0,0.75,0.25),hour=pull(temperature_full,hour))
test_df

# 15. Apply our model in `by_city` to `test_df`.
#     To do that, please do the following:
#       - use `mutate()` to create column `pred`:
#          - first, use `map()` to apply `predict()` to `model` with
#            argument `newdata` set to `test_df`.
#          - secondly, use `map()` to apply `enframe()` to the `pred` column
#          - thirdly, use `map()` and `bind_cols()` to concatenate the predicted
#            temperature with `test_df`.
#       - use `unnest()` get unnested tibble.
#       - use `dplyr::select()` to get all the columns except `data`, `model`
#         and `name`.
#       - use `mutate()` to formulate the columns:
#          - use `factor` to convert `yearfrac` to factor and `fct_recode()` to
#            rename the factor name as `"Winter"` for `yearfrac` equal to `0`,
#            `"Spring"` for `yearfrac` equal to `0.25`, `"Summer"` for
#            `yearfrac` equal to `0.5` and `"Autumn"` for `yearfrac` equal to
#             `0.75`.
#          - convert `hour` back to a number using `as.numeric()` (don't forget
#            to substract 1).
#    Save the generate tibble into `by_city_pred`.
#    `by_city_pred` should print to:
#    # A tibble: 384 x 4
#    # Groups:   city [4]
#    city       pred yearfrac  hour
#    <chr>     <dbl> <fct>    <dbl>
#    1 Vancouver  5.47 Winter       0
#    2 Vancouver  5.12 Winter       1
#    3 Vancouver  4.56 Winter       2
#    4 Vancouver  4.01 Winter       3
#    5 Vancouver  3.26 Winter       4
#    6 Vancouver  2.58 Winter       5
#    7 Vancouver  2.16 Winter       6
#    8 Vancouver  1.68 Winter       7
#    9 Vancouver  1.36 Winter       8
#    10 Vancouver  1.14 Winter       9
#    # … with 374 more rows
## Do not modify this line!
by_city_pred<-by_city%>%
  mutate(pred=map(model,predict,newdata=test_df)%>%
           map(enframe,value="pred")%>%
           map(~bind_cols(.x,test_df)))%>%
  unnest(pred)%>%
  select(-data,-model,-name)%>%
  mutate(yearfrac=fct_recode(factor(yearfrac),
                             Winter="0",
                             Spring='0.25',
                             Summer='0.5',
                             Autumn='0.75'))%>%
  mutate(hour=as.numeric(hour)-1)
                             

by_city_pred


# 16. Plot the predicted temperature from `by_city_pred` of the four seasons
#     in city `Miami`, `Phoenix` and `Vancouver`.
#     To do that, you can use:
#      - `filter()` to select the three cities.
#      - `ggplot()` to set the `x` axis to be the hour of the day
#         and `y` axis to be the prediction
#      - `geom_point()` to plot the predicted temperature.
#      - `facet_grid()` to draw plots for each `yearfrac`.
#      - `labs()` to set `title` to
#          `"Our model can predict the temperature for different seasons!"`,
#          `x` to `"Hour"` and `y` to `"Prediction (C)"`.
#    Save the generated tibble into `temperature_per_hour_season_plot`.
#
## Do not modify this line!
temperature_per_hour_season_plot<-by_city_pred%>%
  filter(city %in% c("Miami","Phoenix","Vancouver"))%>%
  ggplot(aes(x=hour,y=pred))+
  geom_point()+
  facet_grid(city~yearfrac)+
  labs(title = 'Our model can predict the temperature for different seasons!',
       x="Hour",
       y="Prediction (C)")
temperature_per_hour_season_plot


