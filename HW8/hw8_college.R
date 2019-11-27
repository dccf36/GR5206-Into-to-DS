# HW8: college
#
# Throughout the exercise:
#    - Do NOT use `for`, `while` or `repeat` loops.
#    - Use `%>%` to structure your operations.
#    - Use `my_theme` that you will create in the problem 1
#    for all the plots.
#    - Please specify `dplyr::` when you use `select()`.
#
#
# 1. Load the packages `tidyverse`, `modelr` and `broom`.
#    Use `read_csv()` to read the datasets from data folder:
#       - `college.csv` into a tibble `college`.
#    To check your solution, `college` prints to:
#    # A tibble: 777 x 18
#    Private  Apps Accept Enroll Top10perc Top25perc F.Undergrad P.Undergrad
#    <chr>   <dbl>  <dbl>  <dbl>     <dbl>     <dbl>       <dbl>       <dbl>
#    1 Yes      1660   1232    721        23        52        2885         537
#    2 Yes      2186   1924    512        16        29        2683        1227
#    3 Yes      1428   1097    336        22        50        1036          99
#    4 Yes       417    349    137        60        89         510          63
#    5 Yes      193    146     55        16        44         249         869
#    6 Yes       587    479    158        38        62         678          41
#    7 Yes       353    340    103        17        45         416         230
#    8 Yes      1899   1720    489        37        68        1594          32
#    9 Yes      1038    839    227        30        63         973         306
#    10 Yes       582    498    172        21        44         799          78
#    # … with 767 more rows, and 10 more variables: Outstate <dbl>,
#    #   Room.Board <dbl>, Books <dbl>, Personal <dbl>, PhD <dbl>,
#    #   Terminal <dbl>, S.F.Ratio <dbl>, perc.alumni <dbl>, Expend <dbl>,
#    #   Grad.Rate <dbl>
#
#    Create a theme called `my_theme` that combine `theme_light()` and
#    adjust the titles by adding `theme()`
#    with arguments:
#      - `plot.title = element_text(hjust = 0.5)`
#      - `plot.subtitle = element_text(hjust = 0.5))`.
#
## Do not modify this line!
library(tidyverse)
library(modelr)
library(broom)
college<-read_csv("data/college.csv")
my_theme<-theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# 2. Create a tibble of dim 777 x 4. Replace all the `.` with `_`.
#    The dataset should sum up `Outstate`, `Room_Board`, `Books`,
#    `Personal` and `Expend` costs, and store the sum into a new
#    column called `total_cost`.
#    Then transform `Private` into a factor variable.
#    Finally, choose the
#    `total_cost`, `Private`, `Top10perc` and `PhD`columns.
#    To do this, you can use:
#      - `rename_all()` and `str_replace_all()` to replace `.` with `_`.
#      (hint1: `rename_all()` has been vectorized so it can
#      work like `map()`.
#      hint2: in order to escape the `.`, you can use `\\.`.)
#      - `nest()` to combine `Outstate`, `Room_Board`, `Books`,
#      `Personal` and `Expend` into one column called `total_cost`.
#      - `mutate()` to create `total_cost` and transform `Private`
#      into factors.
#      (hint: you can use `map_dbl()` and `sum()` to calculate the
#      total cost of each row.)
#      - `dplyr::select()` to choose the required columns.
#    Store the returned dataset into `df`.
#    To check your solution, `df` prints to:
#    # A tibble: 777 x 4
#    total_cost Private Top10perc   PhD
#    <dbl> <fct>       <dbl> <dbl>
#    1      20431 Yes            23    70
#    2      31507 Yes            16    29
#    3      25300 Yes            22    53
#    4      38751 Yes            60    92
#    5      24902 Yes            16    76
#    6      27737 Yes            38    67
#    7      29871 Yes            17    90
#    8      31481 Yes            37    89
#    9      32439 Yes            30    79
#    10      25299 Yes            21    40
#    # … with 767 more rows
## Do not modify this line!
df<-college%>%
  rename_all(~str_replace_all(.x,"[.]","_"))%>%
  nest(total_cost=c('Outstate','Room_Board','Books','Personal','Expend'))%>%
  mutate(total_cost=map_dbl(total_cost,sum))%>%
  mutate(Private=factor(Private))%>%
  dplyr::select(total_cost,Private,Top10perc,PhD)

df
# 3. Draw a point plot of `total_cost` vs. `Top10perc`.
#    Name the title as `"Total cost seems to have linear relationship with percent of students`
#    `from top 10 percent high schools"`,
#    To do this, you can use:
#      - `geom_point()` to draw a point plot of
#      `total_cost` vs. `Top10perc`.
#      - `geom_smooth()` to draw a straight line with `lm` method,
#      and turn of the `se`.
#      - `labs()` to name the title as
#        `"Total cost seems to have linear relationship with percent of students`
#        `from top 10 percent high schools"`
#        the x-axis as `"Pct. of new students from top 10% H.S."`,
#        the y-axis as `"Total cost (USD)"`.
#    Store the plot into a variable `g1`.
## Do not modify this line!
g1<-ggplot(data=df,aes(x=Top10perc,y=total_cost))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  labs(title = 'Total cost seems to have linear relationship with percent of students
       from top 10 percent high schools',
       x='Pct. of new students from top 10% H.S.',
       y='Total cost (USD)')+
  my_theme

g1
# 4. Fit a linear regression model of `total_cost` vs. `Top10perc`.
#    Store the model to `m0`.
#    The expected output for `m0 %>% broom::tidy()` should be:
#    # A tibble: 2 x 5
#    term        estimate std.error statistic   p.value
#    <chr>          <dbl>     <dbl>     <dbl>     <dbl>
#    1 (Intercept)   16854.     450.       37.4 7.22e-176
#    2 Top10perc       345.      13.8      25.0 9.21e-102
#    Calculate residuals and predictions of `m0`, name them `resid0`
#    and `pred0` accordingly, and add two new columns to back of `df`.
#    To do this, you can use:
#      - `lm()` to build the model.
#      - `add_residuals()` and `add_predictions()` to add the
#      calculated columns.
#    Store returned dataset to `total_cost_pred`.
#    To check your solution, `total_cost_pred` prints to:
#    # A tibble: 777 x 6
#    total_cost Private Top10perc   PhD resid0  pred0
#    <dbl> <fct>       <dbl> <dbl>  <dbl>  <dbl>
#    1      20431 Yes            23    70 -4347. 24778.
#    2      31507 Yes            16    29  9141. 22366.
#    3      25300 Yes            22    53   867. 24433.
#    4      38751 Yes            60    92  1226. 37525.
#    5      24902 Yes            16    76  2536. 22366.
#    6      27737 Yes            38    67 -2209. 29946.
#    7      29871 Yes            17    90  7160. 22711.
#    8      31481 Yes            37    89  1880. 29601.
#    9      32439 Yes            30    79  5250. 27189.
#    10      25299 Yes            21    40  1210. 24089.
#    # … with 767 more rows
## Do not modify this line!
m0<-lm(total_cost~Top10perc,data=df)
total_cost_pred<-df%>%
  add_residuals(m0)%>%
  add_predictions(m0)%>%
  rename(resid0=resid,pred0=pred)

total_cost_pred

# 5. Draw a point plot of `resid0` vs. `pred0`.
#    Name the title as `"The residuals have a fanning pattern that spread to the right"`
#    To do this, you can use:
#      - `geom_ref_line` to draw reference line with height = 0.
#      - `geom_point()` to draw a point plot of
#      `resid0` vs. `pred0`.
#      - `labs()` to name the title as
#        `"The residuals have a fanning pattern that spread to the right"`,
#        subtitle as `"The data seems to display residual heteroskedasticity"`,
#        the x-axis as `"Prediction of total cost (USD)"`,
#        the y-axis as `"Residuals of m0"`.
#    Store the plot into a variable `g2`.
## Do not modify this line!
g2<-ggplot(data=total_cost_pred,aes(x=pred0,y=resid0))+
  geom_ref_line(h=0)+
  geom_point()+
  labs(title = 'The residuals have a fanning pattern that spread to the right',
       subtitle = 'The data seems to display residual heteroskedasticity',
       x='Prediction of total cost (USD)',
       y='Residuals of m0')+
  my_theme
g2

# 6. Create a new column called `log_total_cost` that takes the log
#    of `total` cost in `df`. You can use `mutate` to finish the work.
#    To check your solution, `df` prints to:
#    # A tibble: 777 x 5
#    total_cost Private Top10perc   PhD log_total_cost
#    <dbl> <fct>       <dbl> <dbl>          <dbl>
#    1      20431 Yes            23    70           9.92
#    2      31507 Yes            16    29          10.4
#    3      25300 Yes            22    53          10.1
#    4      38751 Yes            60    92          10.6
#    5      24902 Yes            16    76          10.1
#    6      27737 Yes            38    67          10.2
#    7      29871 Yes            17    90          10.3
#    8      31481 Yes            37    89          10.4
#    9      32439 Yes            30    79          10.4
#    10      25299 Yes            21    40          10.1
#    # … with 767 more rows
#
#    Create a new column called `log_total_cost` that takes the log
#    of `total` cost in `total_cost_pred`.
#    You can use `mutate` to finish the work.
#    To check your solution, `total_cost_pred` prints to:
#    # A tibble: 777 x 7
#    total_cost Private Top10perc   PhD resid0  pred0 log_total_cost
#    <dbl> <fct>       <dbl> <dbl>  <dbl>  <dbl>          <dbl>
#    1      20431 Yes            23    70 -4347. 24778.           9.92
#    2      31507 Yes            16    29  9141. 22366.          10.4
#    3      25300 Yes            22    53   867. 24433.          10.1
#    4      38751 Yes            60    92  1226. 37525.          10.6
#    5      24902 Yes            16    76  2536. 22366.          10.1
#    6      27737 Yes            38    67 -2209. 29946.          10.2
#    7      29871 Yes            17    90  7160. 22711.          10.3
#    8      31481 Yes            37    89  1880. 29601.          10.4
#    9      32439 Yes            30    79  5250. 27189.          10.4
#    10      25299 Yes            21    40  1210. 24089.          10.1
#    # … with 767 more rows
## Do not modify this line!
df<-df%>%
  mutate(log_total_cost=log(total_cost))
total_cost_pred<-total_cost_pred%>%
  mutate(log_total_cost=log(total_cost))
total_cost_pred


# 7. Here, we want to try if `log_total_cost` vs. `Top10perc` fits better.
#    Fit a linear regression model of `log_total_cost` vs. `Top10perc`.
#    Store the model to `m1`.
#    The expected output for `m1 %>% broom::tidy()` should be:
#    # A tibble: 2 x 5
#    term        estimate std.error statistic  p.value
#    <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#    1 (Intercept)   9.82    0.0162       607.  0.
#    2 Top10perc     0.0111  0.000495      22.5 1.08e-86
#
#    Calculate residuals and predictions of `m1`, name them `resid1`
#    and `pred1` accordingly, and add two new columns to
#    back of `total_cost_pred`.
#    To do this, you can use:
#      - `lm()` to build the model.
#      - `add_residuals()` and `add_predictions()` to add the
#      calculated columns.
#    Store returned dataset to `total_cost_pred`.
#    To check your solution, `total_cost_pred` is a tibble of 777 x 9.
#    The first 10 rows of `resid1` and `pred1` print to:
#    resid1 pred1
#    <dbl> <dbl>
#    1 -0.153  10.1
#    2  0.358  10.00
#    3  0.0723 10.1
#    4  0.0757 10.5
#    5  0.123  10.00
#    6 -0.0138 10.2
#    7  0.294  10.0
#    8  0.124  10.2
#    9  0.232  10.2
#    10  0.0834 10.1
## Do not modify this line!
m1<-lm(log_total_cost~Top10perc,data=df)
total_cost_pred<-total_cost_pred%>%
  add_residuals(m1)%>%
  add_predictions(m1)%>%
  rename(resid1=resid,pred1=pred)
total_cost_pred


# 8. Draw a point plot of `log_total_cost` vs. `Top10perc`.
#    Name the title as `"After taking log of total cost, the log of total cost has a`
#    `more linear relationship with the students from top 10% H.S."`,
#    To do this, you can use:
#      - `geom_point()` to draw a point plot of
#      `log_total_cost` vs. `Top10perc`.
#      - `geom_line()` to draw a line of `pred1` vs. `Top10perc`, make the
#      color as `"red"`.
#      - `labs()` to name the title as
#        `"After taking log of total cost, the log of total cost has better`
#        `linear relationship with top 10 percent colleges"`
#        the x-axis as `"Pct. of new students from top 10% H.S."`,
#        the y-axis as `"Log of total cost (log (USD))"`.
#    Store the plot into a variable `g3`.
## Do not modify this line!
g3<-ggplot(data=total_cost_pred,aes(x=Top10perc,y=log_total_cost))+
  geom_point()+
  geom_line(aes(x=Top10perc,y=pred1),color='red')+
  labs(title = 'After taking log of total cost, the log of total cost has better，
       linear relationship with top 10 percent colleges',
       x='Pct. of new students from top 10% H.S.',
       y='Log of total cost (log (USD))')+
  my_theme
g3


# 9. Draw a point plot of `resid1` vs. `Top10perc`.
#    Name the title as title as `"The residuals don't have obvious pattern"`.
#    To do this, you can use:
#      - `geom_ref_line` to draw reference line with height = 0.
#      - `geom_point()` to draw a point plot of
#      `resid1` vs. `Top10perc`.
#      - `labs()` to name the title as
#        `"The residuals don't have obvious pattern"`,
#        subtitle as `"The Top10perc is a good main effect"`,
#        the x-axis as `"Pct. of new students from top 10% H.S."`,
#        the y-axis as `"Residuals of m1"`.
#    Store the plot into a variable `g4`.
## Do not modify this line!
g4<-ggplot(data=total_cost_pred)+
  geom_ref_line(h=0)+
  geom_point(aes(x=Top10perc,y=resid1))+
  labs(title = "The residuals don't have obvious pattern",
       subtitle = 'The Top10perc is a good main effect',
       x='Pct. of new students from top 10% H.S.',
       y='Residuals of m1')+
  my_theme
g4


# 10. Draw a boxplot of `log_total_cost` vs. `Private`.
#     Name the title as `"Whether a college is private or not can help to differentiate`
#     `the total cost"`
#     To do this, you can use:
#       - `geom_boxplot()` to draw a boxplot of
#       `log_total_cost` vs. `Private`.
#       - `labs()` to name the title as
#         `"Whether a college is private or not can help to differentiate`
#         `the total cost"`
#         the x-axis as `"Private"`,
#         the y-axis as `"Log of total cost (log (USD))"`.
#     Store the plot into a variable `g5`
## Do not modify this line!
g5<-ggplot(data=total_cost_pred)+
  geom_boxplot(aes(x=Private,y=log_total_cost))+
  labs(title = 'Whether a college is private or not can help to differentiate
       the total cost',
       x='Private',
       y='Log of total cost (log (USD))')+
  my_theme
g5


# 11. Fit a linear regression model of `log_total_cost` vs. interaction
#     of `Top10perc` and `Private`. Store the model to `m2`.
#     The expected output for `m2 %>% broom::tidy()` should be:
#     # A tibble: 4 x 5
#     term                 estimate std.error statistic  p.value
#     <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
#     1 (Intercept)           9.72     0.0252      385.   0.
#     2 PrivateYes            0.174    0.0305        5.69 1.82e- 8
#     3 Top10perc             0.00720  0.000902      7.98 5.12e-15
#     4 PrivateYes:Top10perc  0.00370  0.00103       3.59 3.52e- 4
#
#     Calculate residuals and predictions of `m2`, name them `resid2`
#     and `pred2` accordingly, and add two new columns to
#     back of `total_cost_pred`.
#     To do this, you can use:
#       - `lm()` to build the model.
#       - `add_residuals()` and `add_predictions()` to add the
#       calculated columns.
#     Store returned dataset to `total_cost_pred`.
#     To check your solution, `total_cost_pred` is a tibble of 777 x 11.
#     The first 10 rows of `resid2` and `pred2` print to:
#     resid2 pred2
#     <dbl> <dbl>
#     1 -0.224     10.1
#     2  0.285     10.1
#     3  0.000591  10.1
#     4  0.0125    10.6
#     5  0.0502    10.1
#     6 -0.0819    10.3
#     7  0.221     10.1
#     8  0.0556    10.3
#     9  0.162     10.2
#     10  0.0115    10.1
## Do not modify this line!
m2<-lm(log_total_cost~Private*Top10perc,data=total_cost_pred)
total_cost_pred<-total_cost_pred%>%
  add_residuals(m2)%>%
  add_predictions(m2)%>%
  rename(resid2=resid,pred2=pred)

# 12. Draw a point plot of `resid2` vs. `pred2`.
#     Name the title as `"There's no obvious pattern of residuals"`
#     To do this, you can use:
#       - `geom_ref_line` to draw reference line with height = 0.
#       - `geom_point()` to draw a point plot of
#       `resid2` vs. `pred2`.
#       - `labs()` to name the title as
#         `"There's no obvious pattern of residuals"`,
#         names the subtitle as
#         `"The interaction of Private and Top10perc is a good main effect"`,
#         the x-axis as `"Prediction of log of total cost (log (USD))"`,
#         the y-axis as `"Residuals of m2"`.
#     Store the plot into a variable `g6`.
## Do not modify this line!
g6<-ggplot(data=total_cost_pred,aes(x=pred2,y=resid2))+
  geom_ref_line(h=0)+
  geom_point()+
  labs(title = "There's no obvious pattern of residuals",
       subtitle = 'The interaction of Private and Top10perc is a good main effect',
       x='Prediction of log of total cost (log (USD))',
       y='Residuals of m2')+
  my_theme
g6

# 13. Select `log_total_cost`, `pred2`, `Private` and `Top10perc` from
#     `total_cost_pred`. Rename `log_total_cost` as `Data`, `pred2` as
#     `Prediction`. Tidying the dataset by combining `Data` and
#     `Prediction` into one column.
#     To do this, you can use:
#       - `dplyr::select()` to choose required columns.
#       - `rename()` to rename the columns.
#       - `pivot_longer()` to tidy the dataset. Make names as `"Type"`
#       and values as `"log_total_cost"`.
#     Store returned dataset to `stats_m2`.
#     To check your solution, `stats_m2` prints to:
#     # A tibble: 1,554 x 4
#     Private Top10perc Type       log_total_cost
#     <fct>       <dbl> <chr>               <dbl>
#     1 Yes            23 Data                 9.92
#     2 Yes            23 Prediction          10.1
#     3 Yes            16 Data                10.4
#     4 Yes            16 Prediction          10.1
#     5 Yes            22 Data                10.1
#     6 Yes            22 Prediction          10.1
#     7 Yes            60 Data                10.6
#     8 Yes            60 Prediction          10.6
#     9 Yes            16 Data                10.1
#     10 Yes            16 Prediction          10.1
#     # … with 1,544 more rows
## Do not modify this line!
stats_m2<-total_cost_pred%>%
  select(log_total_cost,pred2,Private,Top10perc)%>%
  rename(Data=log_total_cost,Prediction=pred2)%>%
  pivot_longer(cols=c(Data,Prediction),names_to = "Type",values_to = 'log_total_cost')
stats_m2


# 14. Draw a point plot of `log_total_cost` vs. `Top10perc`, faceted
#     on `Private`.
#     Name the title as `"The m2 model seems to fit well on both public colleges and private colleges"`
#     To do this, you can use:
#       - `geom_point()` to draw a boxplot of
#       `log_total_cost` vs. `Top10perc`.
#       - `facet_grid()` to facet on `Private`.
#       - `geom_smooth()` to draw smooth curves with linetype as
#       `Type`. Turn of the `se`.
#       - `labs()` to name the title as
#         `"Whether a college is private or not can help to differentiate`
#         `the total cost"`
#         the x-axis as `"Pct. of new students from top 10% H.S."`,
#         the y-axis as `"Log of total cost (log (USD))"`.
#     Store the plot into a variable `g7`
## Do not modify this line!
g7<-ggplot(data=stats_m2,aes(x=Top10perc,y=log_total_cost))+
  geom_point()+
  facet_grid(~Private)+
  geom_smooth(aes(linetype=Type),se=FALSE)+
  labs(title = 'The m2 model seems to fit well on both public colleges and private colleges',
       x='Pct. of new students from top 10% H.S.',
       y='Log of total cost (log (USD))')+
  my_theme
g7


# 15. Draw a point plot of `resid2` vs. square of `PhD`.
#     Name the title as `"The log of total cost has better`
#     `linear relationship with percent of faculty with PhD"`,
#     To do this, you can use:
#       - `geom_point()` to draw a point plot of
#       `log_total_cost` vs. square of `PhD`.
#       - `geom_smooth` to draw a straight line with `lm` method,
#       and turn of the `se`.
#       - `labs()` to name the title as
#         `"The residuals of m2 seem to have`
#         `linear relationship with squre of pct. of faculty with PhD"`,
#         subtitle as `"Squre of pct. of faculty with PhD is a good predictor after removing the main effect"`,
#         the x-axis as `"Square of pct. of faculty with PhD"`,
#         the y-axis as `"Residuals of m2"`.
#     Store the plot into a variable `g8`.
## Do not modify this line!
g8<-ggplot(data=total_cost_pred,aes(x=PhD^2,y=resid2))+
             geom_point()+
             geom_smooth(se=F,method = "lm")+
             labs(title = 'The residuals of m2 seem to have
                  linear relationship with squre of pct. of faculty with PhD',
                  subtitle = 'Squre of pct. of faculty with PhD is a good predictor after removing the main effect',
                  x='Square of pct. of faculty with PhD',
                  y='Residuals of m2')+
  my_theme
g8


# 16. Fit a linear regression model of `log_total_cost` vs. interaction
#     of `Top10perc` and `Private` plus square of `PhD`.
#     Store the model to `m3`.
#     The expected output for `m3 %>% broom::tidy()` should be:
#     # A tibble: 6 x 5
#     term                 estimate std.error statistic  p.value
#     <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
#     1 (Intercept)           9.78     0.0222      440.   0.
#     2 PrivateYes            0.270    0.0276        9.79 2.16e-21
#     3 Top10perc             0.00376  0.000812      4.63 4.33e- 6
#     4 poly(PhD, 2)1         3.12     0.232        13.4  4.85e-37
#     5 poly(PhD, 2)2         2.07     0.193        10.8  2.75e-25
#     6 PrivateYes:Top10perc  0.00210  0.000904      2.33 2.02e- 2
#
#     Calculate residuals and predictions of `m3`, name them `resid3`
#     and `pred3` accordingly, and add two new columns to
#     back of `total_cost_pred`.
#     To do this, you can use:
#       - `lm()` to build the model.
#       - `add_residuals()` and `add_predictions()` to add the
#       calculated columns.
#     Store returned dataset to `total_cost_pred`.
#     To check your solution, `total_cost_pred` is a tibble of 777 x 13.
#     The first 10 rows of `resid3` and `pred3` print to:
#     resid3 pred3
#     <dbl> <dbl>
#     1 -0.187    10.1
#     2  0.292    10.1
#     3  0.116    10.0
#     4 -0.0423   10.6
#     5 -0.00478  10.1
#     6  0.0541   10.2
#     7 -0.0173   10.3
#     8 -0.0659   10.4
#     9  0.144    10.2
#     10  0.108    10.0
## Do not modify this line!
m3<-lm(log_total_cost~Private*Top10perc+poly(PhD,2),data=total_cost_pred)
m3 %>% broom::tidy()
total_cost_pred<-total_cost_pred%>%
  add_residuals(m3)%>%
  add_predictions(m3)%>%
  rename(resid3=resid,pred3=pred)

# 17. Draw a point plot of `resid3` vs. `pred3`.
#     Name the title as `"There's no obvious pattern of in the residuals"`
#     To do this, you can use:
#       - `geom_ref_line` to draw reference line with height = 0.
#       - `geom_point()` to draw a point plot of
#       `resid3` vs. `PhD^2`.
#       - `labs()` to name the title as
#         `"There's no obvious pattern of in the residuals"`,
#         names the subtitle as
#         `"The m3 fits well"`,
#         the x-axis as `"Prediction of log of total cost (log (USD))"`,
#         the y-axis as `"Residuals of m3"`.
#     Store the plot into a variable `g9`.
## Do not modify this line!
g9<-ggplot(data=total_cost_pred,aes(x=pred3,y=resid3))+
  geom_ref_line(h=0)+
  geom_point()+
  labs(title = "There's no obvious pattern of in the residuals",
       subtitle = 'The m3 fits well',
       x='Prediction of log of total cost (log (USD))',
       y='Residuals of m3')+
  my_theme
g9


# 18. Create a list of `m1`, `m2` and `m3`. Store the list to `model_list`.
#     Calculate R-squared and adjusted r-squared of each model. Finally,
#     tidy the dataset.
#     To do this, you can use:
#       - `map_dfr()` and `glance()` to get the summary of each model.
#       - `dplyr::select()` to get R-squared and adjusted r-squared.
#       - `mutate()` to create a new column called `group` and assign
#       `c('m1', 'm2', 'm3')` to the column.
#       - `pivot_longer()` to combine r-squared and adjusted r-squared
#       values into the same column. Set the names as `Type`, values as
#       `Value`.
#     Store the returned dataset to `r_squared_and_adjusted`.
#     To check your solution, `r_squared_and_adjusted` prints to:
#     # A tibble: 6 x 3
#     group Type          Value
#     <chr> <chr>         <dbl>
#     1 m1    adj.r.squared 0.394
#     2 m1    r.squared     0.395
#     3 m2    adj.r.squared 0.539
#     4 m2    r.squared     0.541
#     5 m3    adj.r.squared 0.653
#     6 m3    r.squared     0.655
## Do not modify this line!
model_list<-list(m1,m2,m3)
r_squared_and_adjusted<-model_list%>%
  map_dfr(glance)%>%
  dplyr::select(adj.r.squared,r.squared)%>%
  mutate(group=c('m1','m2','m3'))%>%
  pivot_longer(cols = c(adj.r.squared,r.squared),names_to = 'Type',values_to = 'Value')
r_squared_and_adjusted

# 19. Draw a barplot of `Value` vs. `group`, colored by `Type`.
#     Name the title as `"R-squared and adjusted r-squared both increase as models`
#     `becoming more complex"`
#     To do this, you can use:
#       - `geom_col()` to draw a barplot of
#       `Value` vs. `group`, colored by `Type`,
#       with position as `dodge`.
#       - `labs()` to name the title as
#         `"R-squared and adjusted r-squared both increase as models`
#         `becoming more complex"`,
#         the x-axis as `"Model"`,
#         the y-axis as `"Value"`.
#     Store the plot into a variable `g10`.
## Do not modify this line!
g10<-ggplot(data=r_squared_and_adjusted)+
  geom_col(aes(x=group,y=Value,fill=Type),position = 'dodge')+
  labs(title = "R-squared and adjusted r-squared both increase as models
       becoming more complex",
       x='Model',
       y='Value')+
  my_theme
g10


# 20. Fit a linear regression model of `total_cost` vs. `1`.
#     Store the model to `mnull`.
#     The expected output for `mnull %>% broom::tidy()` should be:
#     # A tibble: 1 x 5
#     term        estimate std.error statistic p.value
#     <chr>          <dbl>     <dbl>     <dbl>   <dbl>
#     1 (Intercept)     10.1    0.0112      904.       0
#
#     Use `anova()` to test anova of `mnull`, `m1`, `m2` and `m3`.
#     Store the returned values to `anova_models`.
#     To help make sure that you are correct, check that the
#     output of `anova_models %>% broom::tidy()` is
#     # A tibble: 4 x 6
#     res.df   rss    df sumsq statistic    p.value
#     <dbl> <dbl> <dbl> <dbl>     <dbl>      <dbl>
#     776  75.7    NA NA          NA  NA
#     775  45.8     1 29.9       884.  4.94e-130
#     773  34.8     2 11.0       163.  1.00e- 59
#     771  26.1     2  8.67      128.  8.73e- 49
## Do not modify this line!

mnull<-lm(log_total_cost~1,data=total_cost_pred)
anova_models<-anova(mnull,m1,m2,m3)
anova_models %>% broom::tidy()

