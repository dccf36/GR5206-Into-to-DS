# HW8: temperature
#
# For this assignment, you will use temperature data
# provided by [Berkeley Earth](http://berkeleyearth.org/data/).
# More specifically, you will use time series of average
# monthly air temperatures over land in every country
# between 1743 and today.
#
# Throughout the exercise:
# - Use `theme_light()` for the plots.
# - Do not change the default position of the plot title.
# - Do not print the plot.
#
# 1. Load the `tidyverse`, `maps`, `modelr`, and `lubridate` packages.
#    Use the function `read_csv()` to read the dataset
#    `temperature.csv` from path `/course/data/`.
#    Store it into a tibble `temperature_original`.
## Do not modify this line!
library(tidyverse)
library(maps)
library(modelr)
library(lubridate)
temperature_original<-read_csv('/course/data/temperature.csv')


# 2. Add two columns `year` and `month` to `temperature_original`.
#    `year` is the year part of the `date` column and
#    its class is numeric.
#    `month` is the month part of `date` column and
#    its class is factor. The levels are `"Jan"`, `"Feb"`,
#    ..., `"Dec"`.
#    Futhermore, filter you data to focus on the
#    20th and 21th centuries and drop `NA` values in all columns.
#    Save the altered tibble to `temperature`.
#    To do that, you can use:
#    - `mutate()` to add columns.
#    - `lubridate::year()` to extract year from `date`
#    - `lubridate::month()` to extract month from `date`.
#      Set the `label` parameter to `TRUE` to display
#      the month as a character string.
#    - `factor()` to transform `month` into factor.
#      Set the `levels` parameter to `"Jan"`, `"Feb"`,
#      ..., `"Dec"`. Make sure the levels are ordered.
#    - `filter()` to filter out years that are smaller
#      than 1900.
#    - `drop_na()` to drop NA values.
#    `temperature` should print to :
#    # A tibble: 271,355 x 6
#      date       temperature country     region  year month
#      <date>           <dbl> <chr>       <chr>  <dbl> <fct>
#    1 1900-01-01       -3.43 Afghanistan Asia    1900 Jan
#    2 1900-02-01        1.23 Afghanistan Asia    1900 Feb
#    3 1900-03-01       10.5  Afghanistan Asia    1900 Mar
#    4 1900-04-01       13.4  Afghanistan Asia    1900 Apr
#    5 1900-05-01       20.3  Afghanistan Asia    1900 May
#    6 1900-06-01       24.4  Afghanistan Asia    1900 Jun
#    7 1900-07-01       27.3  Afghanistan Asia    1900 Jul
#    8 1900-08-01       24.9  Afghanistan Asia    1900 Aug
#    9 1900-09-01       20.8  Afghanistan Asia    1900 Sep
#    10 1900-10-01       14.0  Afghanistan Asia    1900 Oct
#    # … with 271,345 more rows
## Do not modify this line!
temperature<-temperature_original%>%
  mutate(year=year(date))%>%
  mutate(month=factor(month(date,label = TRUE),
                      levels = c("Jan","Feb","Mar",'Apr',"May",'Jun',"Jul","Aug","Sep","Oct","Nov","Dec"),
                      ordered = FALSE))%>%
  filter(year>=1900)%>%
  drop_na()

temperature
# 3. Compute the change in temperature for all countries
#    in `temperature` between March 1910 and March 2010.
#    Save the result in `temperature_change`. It should has
#    two columns `country` and `temperature`.
#    To do that, you can use:
#    - `filter()` to filter the data.
#    - `group_by()` to group by `country`.
#    - `arrange()` to arrange the data by `year`.
#    - `summarize()` to compute the difference in temperature.
#    `temperature_change` should print to :
#    # A tibble: 199 x 2
#      country        temperature
#      <chr>                <dbl>
#    1 Afghanistan          6.17
#    2 Albania              1.63
#    3 Algeria              3.85
#    4 American Samoa       2.24
#    5 Andorra              0.164
#    6 Angola               0.962
#    7 Anguilla             2.88
#    8 Argentina            3.39
#    9 Armenia              3.80
#   10 Aruba                3.36
#    # … with 189 more rows
## Do not modify this line!
temperature_change<-temperature%>%
  filter(year==1910 |year==2010)%>%
  filter(month=="Mar")%>%
  group_by(country)%>%
  arrange(year)%>%
  summarize(temperature=diff(temperature))
temperature_change

# 4. Produce a worlwide map of the temperature change between
#    March 1910 and March 2010 using `temperature_change`.
#    To do that, you can use:
#    - `ggplot()` to initialize a ggplot object setting `map_id`
#      to `country` in `aes()`.
#    - `geom_map()` to draw the map.
#      Set `fill` to `temperature` in `aes()`.
#      Set `map` to `map_data("world")`.
#    - `expand_limits()` to set `x` from `-180` to `180`,
#      and `y` from `-90` to `90`.
#    - `scale_fill_gradient()` to set the legend color from
#      `"blue"` to `"red"` so that blue stands for cold and red
#      stands for hot.
#    - `labs()` to set:
#      - `title` to `"Worldwide temperature change between March 1910 and March 2010"`.
#      - `subtitle` to `"Most countries experience an increase"`.
#      - `x` to `"Longitude"`.
#      - `y` to `"Latitude"`.
#      - `fill` to `"Temperature (°C)"`.
#    - `theme_light()` to set light theme (i.e. a light background).
#    Save the ggplot object into `temperature_map`.
## Do not modify this line!
temperature_map<-ggplot(data=temperature_change,aes(map_id=country))+
  geom_map(aes(fill=temperature),map=map_data("world"))+
  expand_limits(x=c(-180,180),y=c(-90,90))+
  scale_fill_gradient(low='blue',high='red')+
  labs(title = 'Worldwide temperature change between March 1910 and March 2010',
       subtitle = 'Most countries experience an increase',
       x='Longitude',
       y='Latitude',
       fill='Temperature (°C)')+
  theme_light()
temperature_map
  


# 5. Filter the data from `temperature` to keep all data
#    corresponding to `"Switzerland"`. Save the result in `sw`.
#    To do that, you can use `filter()` to filter the data.
#    `sw` should print to :
#    # A tibble: 1,364 x 6
#      date       temperature country     region  year month
#      <date>           <dbl> <chr>       <chr>  <dbl> <fct>
#    1 1900-01-01      -0.720 Switzerland Europe  1900 Jan
#    2 1900-02-01       1.10  Switzerland Europe  1900 Feb
#    3 1900-03-01      -0.560 Switzerland Europe  1900 Mar
#    4 1900-04-01       5.34  Switzerland Europe  1900 Apr
#    5 1900-05-01       9.59  Switzerland Europe  1900 May
#    6 1900-06-01      14.8   Switzerland Europe  1900 Jun
#    7 1900-07-01      17.3   Switzerland Europe  1900 Jul
#    8 1900-08-01      14.7   Switzerland Europe  1900 Aug
#    9 1900-09-01      13.8   Switzerland Europe  1900 Sep
#    10 1900-10-01       8.10  Switzerland Europe  1900 Oct
#    # … with 1,354 more rows
## Do not modify this line!
sw<-temperature%>%
  filter(country=='Switzerland')


# 6. Plot the box plot of temperature for each month in Switzerland
#    using tibble `sw` and draw the mean temperature to the plot
#    using red dots.
#    To do that, you can use:
#    - `ggplot()` to to initialize a ggplot object, setting `aes()`
#      correctly to plot `temperature` vs `date`.
#    - `geom_boxplot()` to draw the box plot.
#    - `stat_summary()` to draw the mean temperature.
#      Set `fun.y` to `mean`, `colour` to `red`, `size` to `2`, and
#      `geom` to `point`.
#    - `labs()` to set:
#      - `title` to `"Temperature in Switzerland"`.
#      - `subtitle` to `"Winters are colder, summers are warmer (mean in red)"`.
#      - `x` to `"Month"`.
#      - `y` to `"Temperature (°C)"`.
#    - `theme_light()` to set light theme (i.e. a light background).
#    Save the ggplot into `switzerland_temperature_plot`.
## Do not modify this line!
switzerland_temperature_plot<-ggplot(data=sw,aes(x=month,y=temperature))+
  geom_boxplot()+
  stat_summary(fun.y=mean,colour='red',size=2,geom = 'point')+
  labs(title = 'Temperature in Switzerland',
       subtitle = 'Winters are colder, summers are warmer (mean in red)',
       x='Month',
       y='Temperature (°C)')+
  theme_light()
switzerland_temperature_plot


# 7. Fit a simple linear model: monthly temperature
#    vs. month for Swizerland using `sw`. Because there is a strong
#    seasonal effect, it is unclear whether the temperature is
#    increasing over the years.We want here to approximate the
#    monthly evolution of temperature.
#    To do that, please use `lm()` to fit the linear regression
#    model for `temperature` against `month`.
#    Save the model to `sw_mod`.
#    `sw_mod %>% broom::tidy()` should print :
#    # A tibble: 12 x 5
#    term        estimate std.error statistic   p.value
#    <chr>          <dbl>     <dbl>     <dbl>     <dbl>
#    1 (Intercept)    -1.84     0.152    -12.1  5.20e- 32
#    2 monthFeb        1.18     0.215      5.49 4.82e-  8
#    3 monthMar        4.78     0.215     22.2  1.35e- 93
#    4 monthApr        8.27     0.215     38.5  3.08e-219
#    5 monthMay       12.9      0.215     60.1  0.
#    6 monthJun       16.3      0.215     75.6  0.
#    7 monthJul       18.4      0.215     85.4  0.
#    8 monthAug       17.9      0.215     83.0  0.
#    9 monthSep       14.6      0.216     67.6  0.
#    10 monthOct        9.62     0.216     44.6  3.79e-268
#    11 monthNov        4.51     0.216     20.9  1.56e- 84
#    12 monthDec        1.05     0.216      4.89 1.15e-  6
## Do not modify this line!
sw_mod<-lm(temperature~month,data=sw)
sw_mod %>% broom::tidy()

# 8. Use model `sw_mod` to predict temperature in 1910 and add
#    the predictions to `sw`. Name the prediction column
#    `mean temperature` and save the result into `sw_seasonal`.
#    To do that, you can use:
#    - `add_predictions()` to add a column that contains
#      the predicted values using `sw_mod`.
#    - `filter()` to select data that is in 1910.
#    `sw_seasonal` should print to :
#    # A tibble: 12 x 7
#      date       temperature country     region  year month `mean temperature`
#      <date>           <dbl> <chr>       <chr>  <dbl> <fct>              <dbl>
#    1 1910-01-01      -1.14  Switzerland Europe  1910 Jan               -1.84
#    2 1910-02-01       0.107 Switzerland Europe  1910 Feb               -0.658
#    3 1910-03-01       2.71  Switzerland Europe  1910 Mar                2.94
#    4 1910-04-01       5.41  Switzerland Europe  1910 Apr                6.43
#    5 1910-05-01       9.34  Switzerland Europe  1910 May               11.1
#    6 1910-06-01      14.4   Switzerland Europe  1910 Jun               14.4
#    7 1910-07-01      14.2   Switzerland Europe  1910 Jul               16.5
#    8 1910-08-01      14.9   Switzerland Europe  1910 Aug               16.0
#    9 1910-09-01      10.4   Switzerland Europe  1910 Sep               12.7
#    10 1910-10-01       8.17  Switzerland Europe  1910 Oct                7.78
#    11 1910-11-01       1.27  Switzerland Europe  1910 Nov                2.67
#    12 1910-12-01       0.881 Switzerland Europe  1910 Dec               -0.785
## Do not modify this line!
sw_seasonal<-sw%>%
  add_predictions(sw_mod)%>%
  mutate(`mean temperature`=pred)%>%
  select(-pred)%>%
  filter(year==1910)
sw_seasonal


# 9. Plot the predicted monthly temperature in Swizerland in 1910
#    using `sw_seasonal` based on top of `switzerland_temperature_plot`.
#    To do that, you can use:
#    - Call `switzerland_temperature_plot`.
#    - `geom_point()` to draw the points.
#      Set `color` to `green`, `size` to `2`, and `pch` to `3`.
#    - `labs()` to set:
#      - `subtitle` to `"Winters are colder, summers are warmer (mean/fitted in red/green)"`.
#    Save the ggplot in `sw_seasonal_plot`.
## Do not modify this line!
sw_seasonal_plot<-switzerland_temperature_plot+
  geom_point(data=sw_seasonal,aes(x=month,y=`mean temperature`),color='green',size=2,pch=3)+
labs(subtitle = 'Winters are colder, summers are warmer (mean/fitted in red/green)')
sw_seasonal_plot
# 10. Add a residual column to `sw` and save the result in
#     `sw_residuals`.
#     To do that, you can use `add_residuals()`.
#     `sw_residuals` should print to :
#     # A tibble: 1,364 x 7
#       date       temperature country     region  year month  resid
#       <date>           <dbl> <chr>       <chr>  <dbl> <ord>  <dbl>
#     1 1900-01-01      -0.720 Switzerland Europe  1900 Jan    1.12
#     2 1900-02-01       1.10  Switzerland Europe  1900 Feb    1.75
#     3 1900-03-01      -0.560 Switzerland Europe  1900 Mar   -3.50
#     4 1900-04-01       5.34  Switzerland Europe  1900 Apr   -1.10
#     5 1900-05-01       9.59  Switzerland Europe  1900 May   -1.49
#     6 1900-06-01      14.8   Switzerland Europe  1900 Jun    0.342
#     7 1900-07-01      17.3   Switzerland Europe  1900 Jul    0.794
#     8 1900-08-01      14.7   Switzerland Europe  1900 Aug   -1.36
#     9 1900-09-01      13.8   Switzerland Europe  1900 Sep    1.11
#     10 1900-10-01       8.10  Switzerland Europe  1900 Oct    0.313
#     # … with 1,354 more rows
## Do not modify this line!
sw_residuals<-sw%>%
  add_residuals(sw_mod)

sw_residuals
# 11. Plot the residuals from the model using `sw_residuals`
#     and plot a smoothing line.
#     To do that, you can use:
#     - `ggplot()` to to initialize a ggplot object, setting `aes()`
#       correctly to plot `resid` vs `year`.
#     - `geom_point()` to draw the dots.
#     - `geom_smooth()` to draw the smoothing line.
#       Set `method` to `lm`, `col` to `red`, `size` to `2`, and
#       `se` to `FALSE`.
#     - `labs()` to set:
#       - `title` to `"Residuals from the model"`.
#       - `subtitle` to `"We can notice a small yearly increase"`.
#       - `x` to `"Year"`.
#       - `y` to `"Residuals (°C)"`.
#     - `theme_light()` to set light theme (i.e. a light background).
#     Save the ggplot into `residuals_plot`.
## Do not modify this line!
residuals_plot<-ggplot(data=sw_residuals,aes(x=year,y=resid))+
  geom_point()+
  geom_smooth(method = "lm",col='red',size=2,se=FALSE)+
  labs(title='Residuals from the model',
       subtitle = 'We can notice a small yearly increase',
       x='Year',
       y='Residuals (°C)')+
  theme_light()
residuals_plot


# 12. Improve the previous model by adding a linear effect
#     for the temperature as a function of the year using `sw`.
#     To do that, you can use `lm()` to fit the linear regression
#     model for `temperature` against `year` and `month`.
#     Save the model in `sw_improved_mod`.
#     `sw_improved_mod %>% broom::tidy()` should print to :
#     # A tibble: 13 x 5
#     term        estimate std.error statistic   p.value
#     <chr>          <dbl>     <dbl>     <dbl>     <dbl>
#     1 (Intercept) -24.3      2.55        -9.53 7.04e- 21
#     2 year          0.0115   0.00130      8.82 3.33e- 18
#     3 monthFeb      1.18     0.209        5.64 2.03e-  8
#     4 monthMar      4.78     0.209       22.9  4.97e- 98
#     5 monthApr      8.27     0.209       39.5  8.68e-228
#     6 monthMay     12.9      0.209       61.7  0.
#     7 monthJun     16.3      0.209       77.8  0.
#     8 monthJul     18.4      0.209       87.8  0.
#     9 monthAug     17.9      0.209       85.4  0.
#     10 monthSep     14.6      0.210       69.5  0.
#     11 monthOct      9.63     0.210       45.9  4.51e-278
#     12 monthNov      4.52     0.210       21.6  8.77e- 89
#     13 monthDec      1.06     0.210        5.05 5.00e-  7
## Do not modify this line!
sw_improved_mod<-lm(temperature~year+month,data=sw)



# 13. Compare the `sw_mod` and `sw_improved_model` using `anova()`.
#     The analysis of variance `anova` model tells us what model
#     explains more variance in the data.
#     Save the result to `sw_anova`.
#     `sw_anova %>% broom::tidy()` should print to :
#     # A tibble: 2 x 6
#       res.df   rss    df sumsq statistic   p.value
#        <dbl> <dbl> <dbl> <dbl>     <dbl>     <dbl>
#     1   1352 3564.    NA   NA       NA   NA
#     2   1351 3370.     1  194.      77.9  3.33e-18
## Do not modify this line!
sw_anova<-anova(sw_mod,sw_improved_mod)


# 14. Use nested data and list-columns to fit the same model
#     to every country in the dataset, as well as to add predictions
#     and residuals for each fitted model using `temperature`.
#     Save the result in `by_country`.
#     To do that, you can :
#     - create a function `country_model <- function(df) {#your code}`
#       that takes as input a dataset or tibble df and returns
#       a fitted linear regression model of `temperature`
#       against `year` and month. You can assume that `df` contains
#       variables `temperature`, `year` and month.
#       For instance, `country_model(sw)` should return `sw_improved_mod`.
#     - use `group_by()` to group by `country` and `region`.
#     - use `nest()` to nest the data.
#     - use `mutate()` to add columns.
#     - use `purrr::map()` to apply `country_model()` to each row of `data`.
#       Note that `purrr::map()` conflicts with `map::map()`.
#     - use `map2()` to apply `add_residuals()` and `add_predictions` to
#       `data` and the models we just made.
#     The first rows of `by_country` print should be :
#     # A tibble: 199 x 6
#     # Groups:   country, region [199]
#       country    region            data model residuals    predictions
#       <chr>      <chr>     <list<df[,4> <lis> <list>       <list>
#     1 Afghanist… Asia       [1,364 × 4] <lm>  <tibble [1,… <tibble [1,36…
#     2 Albania    Europe     [1,364 × 4] <lm>  <tibble [1,… <tibble [1,36…
#     3 Algeria    Africa     [1,364 × 4] <lm>  <tibble [1,… <tibble [1,36…
#     4 American … Other      [1,364 × 4] <lm>  <tibble [1,… <tibble [1,36…
#     5 Andorra    Europe     [1,364 × 4] <lm>  <tibble [1,… <tibble [1,36…
#     6 Angola     Africa     [1,364 × 4] <lm>  <tibble [1,… <tibble [1,36…
#     7 Anguilla   Other      [1,364 × 4] <lm>  <tibble [1,… <tibble [1,36…
#     8 Argentina  South Am…  [1,364 × 4] <lm>  <tibble [1,… <tibble [1,36…
#     9 Armenia    Asia       [1,364 × 4] <lm>  <tibble [1,… <tibble [1,36…
#     10 Aruba      South Am…  [1,364 × 4] <lm>  <tibble [1,… <tibble [1,36…
#     # … with 189 more rows
## Do not modify this line!
library(purrr)
country_model <- function(df){
  return(lm(temperature~year+month,data=df))
}
by_country<-temperature%>%
  group_by(country,region)%>%
  nest()%>%
  mutate(model=map(data,country_model))%>%
  mutate(residuals=map2(data,model,add_residuals))%>%
  mutate(predictions=map2(data,model,add_predictions))
by_country


# 15. Filter the data that is in `"Iceland"`, `"Ireland"`, `"Spain"`,
#     `"Sweden"`, and `"Haiti"` in 1910 using `by_country`.
#     To do that, you can use:
#     - `filter()` to filter the countries.
#     - `unnest()` to unnest `predictions`.
#     - `filter()` to filter year.
#     The first rows of `by_country` print should be :
#     # A tibble: 60 x 10
#     # Groups:   country, region [5]
#       country region        data model residuals date       temperature  year
#       <chr>   <chr>  <list<df[,> <lis> <list>    <date>           <dbl> <dbl>
#     1 Haiti   North… [1,365 × 4] <lm>  <tibble … 1910-01-01        23.4  1910
#     2 Haiti   North… [1,365 × 4] <lm>  <tibble … 1910-02-01        23.9  1910
#     3 Haiti   North… [1,365 × 4] <lm>  <tibble … 1910-03-01        24.1  1910
#     4 Haiti   North… [1,365 × 4] <lm>  <tibble … 1910-04-01        25.0  1910
#     5 Haiti   North… [1,365 × 4] <lm>  <tibble … 1910-05-01        25.9  1910
## Do not modify this line!
monthly_pattern<-by_country%>%
  filter(country %in% c("Iceland", "Ireland", "Australia", "Brazil","Haiti"))%>%
  unnest(predictions)%>%
  filter(year==1910)


# 16. Plot the seasonal pattern for the monthly temperature
#     in `"Iceland"`, `"Ireland"`, `"Australia"`, `"Brazil"`, and
#     `"Haiti"` in 1910 using `monthly_pattern`.
#     To do that, you can use:
#     - `filter()` to filter countries and year.
#     - `unnest()` to unnest the prediction column.
#     - `ggplot()` to to initialize a ggplot object.
#       Set `x`, `y` in `aes()` to plot `pred` against `month`.
#       Set `color`, and `group` parameters to `country`.
#       `aes()` correctly.
#     - `geom_point()` to draw the points.
#     - `geom_line()` to draw the line.
#     - `labs()` to set:
#       - `title` to `"The seasonal patterns in the northern/southern hemispheres are inverted"`.
#       - `subtitle` to `"Iceland/Haiti has the lowest/highest temperature"`.
#       - `x` to `"Month (in 1910)"`.
#       - `y` to `"Temperature (°C)"`,
#       - `color` to `"Country"`.
#     - `theme_light()` to set light theme (i.e. a light background).
#     Save the ggplot in `monthly_pattern_plot`.
## Do not modify this line!
monthly_pattern_plot<-ggplot(data=monthly_pattern,aes(x=month,y=pred,color=country,group=country))+
  geom_point()+
  geom_line()+
  labs(title='The seasonal patterns in the northern/southern hemispheres are inverted',
       subtitle='Iceland/Haiti has the lowest/highest temperature',
       x='Month (in 1910)',
       y='Temperature (°C)',
       color='Country')+
  theme_light()


# 17. Find the 10 countries for which the model is the worst using
#     `by_country`.
#     To do that, you can use:
#     - `mutate()` to add a `glance` column which corresponds to
#       the mapping of function `broom::glance()` to `model` column
#       using `purrr::map()`
#     - `unnest()` to unnest `glance`.
#     - `select()` to select `country`, `region`, `adj.r.squared`.
#     - `arrange()` to arrange by `adj.r.squared`.
#     Save the tibble into `worst_predict`, It should print to :
#     # A tibble: 10 x 3
#    # Groups:   country, region [10]
#      country                          region        adj.r.squared
#      <chr>                            <chr>                 <dbl>
#    1 Rwanda                           Africa                0.452
#    2 Colombia                         South America         0.530
#    3 Burundi                          Africa                0.533
#    4 Samoa                            Oceania               0.562
#    5 American Samoa                   Other                 0.601
#    6 Kiribati                         Other                 0.611
#    7 Democratic Republic of the Congo Other                 0.618
#    8 Papua New Guinea                 Oceania               0.627
#    9 Ecuador                          South America         0.627
#   10 Venezuela                        South America         0.637
#
## Do not modify this line!
worst_predict<-by_country%>%
  mutate(glance=map(model,broom::glance))%>%
  unnest(glance)%>%
  select(country,region,adj.r.squared)%>%
  arrange(adj.r.squared)%>%
  head(10)



