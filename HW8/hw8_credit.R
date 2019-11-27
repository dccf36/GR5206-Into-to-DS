# HW8: Credit
#
# In this exercise, you will analyze a credit data set with
# a person's basic information and his/her balance. The goal is to predict
# the person's balance by using the other variables.
#
# 1. Let's first read in the required datasets.
#    Load the `readr` package.
#      - Use `read_csv()` to load the `credit.csv` data set from the `data`
#        folder and assign it to a tibble `credit`.
## Do not modify this line!
library(readr)
credit<-read_csv('data/credit.csv')


# 2. Change `Student`, `Married` and `Ethnicity` to factor variables instead of
#    characters. Further, add a new variable `Limit_per_card` which equals the
#    value of `Limit` over `Cards` and sort the tibble by `Balance` in
#    ascending order. To do that, you need to :
#      - Load the `dplyr` package.
#    You can use:
#      - `mutate()` and `factor()` to change the three columns into factor.
#      - `mutate()` to add the new variable.
#      - `arrange()` to sort the tibble.
#    Save your new tibble into `credit_new` whose first few rows should look
#    like:
#    # A tibble: 400 x 12
#    Income Limit Rating Cards   Age Education Gender Student Married Ethnicity
#    <dbl> <dbl>  <dbl> <dbl> <dbl>     <dbl> <fct>  <chr>   <fct>   <fct>
#    15.0  1311    138     3    64        16 Male   No      No      Caucasian
#    20.1  2525    200     3    57        15 Female No      Yes     African …
#    53.6  3714    286     3    73        17 Female No      Yes     African …
#    20.1  2631    213     3    61        10 Male   No      Yes     African …
#    10.7  1757    156     3    57        15 Female No      No      Caucasian
#    28.9  2733    210     5    43        16 Male   No      Yes     Asian
#    31.4  1829    162     4    30        10 Male   No      Yes     Caucasian
#    20.2  2646    199     2    25        14 Female No      Yes     Asian
#    44.5  2252    205     6    72        15 Male   No      Yes     Asian
#    15.3  1499    138     2    47         9 Female No      Yes     Asian
#    … with 390 more rows, and 2 more variables: Balance <dbl>,
#    Limit_per_card <dbl>
#
#    We want to perform a linear regression on `Balance` for people. Let's
#    first try to find some patterns with visualization.
## Do not modify this line!
library(tidyverse)
credit_new<-credit%>%
  mutate(Gender=factor(Gender),Married=factor(Married),Ethnicity=factor(Ethnicity))%>%
  mutate(Limit_per_card=Limit/Cards)%>%
  arrange(Balance)


# 3. Create a facet for different `Ethnicity` group, within each plot,
#    draw a boxplot of `Balance` aginst `Student`. To do that, you need to :
#      - Load the `ggplot2` package.
#    You can use:
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data` and `mapping`.
#      - `geom_boxplot()` to plot a boxplot for `Balance` against `Student`
#      - `facet_wrap()` to create a facet for `Ethnicity`
#      - `labs()` to format the labels such that:
#        - `title = "Students tend to have more balance than nonstudents"`.
#        - `subtitle = "This difference is more obvious in African American group"`.
#        - `x = "Student"`.
#        - `y = "Balance (USD)"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#        - Set its argument `plot.subtitle` using `element_text(hjust = 0.5)`.
#    Save your plot to `g1`.
## Do not modify this line!
g1<-ggplot(credit_new,aes(x=Student,y=Balance))+
  geom_boxplot()+
  facet_wrap(~Ethnicity)+
  labs(title = "Students tend to have more balance than nonstudents",
       subtitle = "This difference is more obvious in African American group",
       x='Student',
       y='Balance (USD)')+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g1


# 4. Create a plot for `Balance` against `Limit_per_card` using color split by
#    `Student`. Describe their relationship.
#    To do that, you can use:
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data` and `mapping`. Don't forget to specify
#        `color = Student`.
#      - `geom_point()` to do a scatter plot for `Balance`
#         against `Limit_per_card`.
#      - `geom_smooth()` to add the regression line.
#      - `labs()` to format the labels such that:
#        - `title = "The balance goes higher as per card limit goes up"`.
#        - `subtitle = "This relationship is nonlinear"`.
#        - `x = "Limit per card (USD)"`.
#        - `y = "Balance (USD)"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#        - Set its argument `plot.subtitle` using `element_text(hjust = 0.5)`.
#    Save your plot to `g2`.
## Do not modify this line!
g2<-ggplot(data=credit_new,aes(x=Limit_per_card,y=Balance,color=Student))+
  geom_point()+
  geom_smooth()+
  labs(title = "The balance goes higher as per card limit goes up",
       subtitle = "This relationship is nonlinear",
       x="Limit per card (USD)",
       y="Balance (USD)")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g2

# 5. We can see the relationship between `Balance` and `Limit_per_card` is
#    nonlinear, but we are trying to fit a linear model, so let's try some
#    transformation on the variable.
#    Create a new variable `log_limit_per_card` in `credit_new`, which equals
#    the logarithm of `Limit_per_card`.
#    Also remove all the observations if the balance is less or equal to 0,
#    because those mess with our regression.
#    To do that, you can use:
#      - `mutate()` and `log()` to create the new variable.
#      - `filter()` to filter out the observations with balance less
#        or equal to 0
#    Then plot `Blance` against `log_limit_per_card` now, still split by
#    `Student`. What is your finding now?
#    To do that, you can use:
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data` and `mapping`. Don't forget to specify
#        `color = Student`.
#      - `geom_point()` to do a scatter plot for `Balance`
#         against `log_limit_per_card`.
#      - `geom_smooth()` to add the regression line.
#      - `labs()` to format the labels such that:
#        - `title = "The balance goes higher as log of per card limit goes up"`.
#        - `subtitle = "This relationship looks more linear after transformation"`.
#        - `x = "Log of Limit per card (USD)"`.
#        - `y = "Balance (USD)"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#        - Set its argument `plot.subtitle` using `element_text(hjust = 0.5)`.
#    Save your plot to `g3`.
## Do not modify this line!
credit_new<-credit_new%>%
  mutate(log_limit_per_card=log(Limit_per_card))%>%
  filter(Balance>0)

g3<-ggplot(data=credit_new,aes(x=log_limit_per_card,y=Balance,color=Student))+
  geom_point()+
  geom_smooth()+
  labs(title = "The balance goes higher as log of per card limit goes up",
       subtitle = "This relationship looks more linear after transformation",
       x="Log of Limit per card (USD)",
       y="Balance (USD)")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g3

# 6. Now let's fit a simple linear regression model `m1` on `Balance` use
#    `log_limit_per_card`, `Student` and their second order iteration.
#    Based on `credit_new`, save your prediction residuals into a column
#    `resid1`, save your predictions into a column `pred1` and save this new
#    tibble into `credit_pred`.
#    To do this, you need to:
#      - Load `modelr` package
#    You can use:
#      - `lm()` to fit a linear regression, save the model to `m1`.
#      - `add_residuals` to add residuals to the new tibble `credit_pred`.
#      - `add_predictions` to add predictions to the new tibble `credit_pred`.
#    The expected output for `m1 %>% broom::tidy()` should be:
#    # A tibble: 4 x 5
#    term                          estimate std.error statistic  p.value
#    <chr>                            <dbl>     <dbl>     <dbl>    <dbl>
#    (Intercept)                     -1223.     268.      -4.56 7.49e- 6
#    log_limit_per_card                245.      35.2      6.96 2.09e-11
#    StudentYes                      -1113.     665.      -1.67 9.51e- 2
#    log_limit_per_card:StudentYes     192.      89.2      2.16 3.17e- 2
#    The first few lines for `credit_pred` looks like:
#    # A tibble: 310 x 15
#    Income Limit Rating Cards   Age Education Gender Student Married Ethnicity
#    <dbl> <dbl>  <dbl> <dbl> <dbl>     <dbl> <fct>  <chr>   <fct>   <fct>
#    16.3  1160    126     3    78        13 Male   Yes     Yes     African …
#    44.5  3500    257     3    81        16 Female No      No      African …
#    88.8  4952    360     4    86        16 Female No      Yes     Caucasian
#    20.9  1233    128     3    47        18 Female Yes     Yes     Asian
#    33.0  3180    224     2    28        16 Male   No      Yes     African …
#    34.5  3271    250     3    57        17 Female No      Yes     Asian
#    35.0  3327    253     3    54        14 Female No      No      African …
#    15.5  2762    215     3    60        18 Male   No      No      Asian
#    26.5  2910    236     6    58        19 Female No      Yes     Caucasian
#    17.4  2748    228     3    32        14 Male   No      Yes     Caucasian
#    … with 300 more rows, and 5 more variables: Balance <dbl>,
#    Limit_per_card <dbl>, log_limit_per_card <dbl>, resid1 <dbl>, pred1 <dbl>
## Do not modify this line!
library(modelr)
m1<-lm(Balance~log_limit_per_card*Student,data=credit_new)

credit_pred<-credit_new%>%
  add_residuals(m1)%>%
  add_predictions(m1)%>%
  rename(resid1=resid,pred1=pred)


# 7. Let's check if our model captures the relationship we observe in `g3`.
#    To do this, you need to:
#      - Load `tidyr` package.
#    You can start from `credt_pred` and use:
#      - `rename` to change the column name of `Balance` to `Data`,
#        `pred1` to `Predictions`
#      - `pivot_longer` to make the true values `Data` and predicted values
#        in `Predictions` into one column named `Balance` and use `Type` to
#        characterize if it is true or predicted.
#        - set `names_to = "Type`
#        - set `values_to = "Balance"`
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data` and `mapping`. Don't forget to specify
#        `color = Student`.
#      - `geom_point()` to do a scatter plot for `Balance`
#         against `log_limit_per_card`.
#      - `geom_smooth()` to add the regression line for each Student group
#        and for both true and predicted values
#        - set `linetype = Type` in `aes()`
#        - specify `se = FALSE`
#      - `labs()` to format the labels such that:
#        - `title = "In general the model captures the pattern between
#                    Balance and log of limit per card"`.
#        - `subtitle = "The fit is not ideal for nonstudent group"`.
#        - `Log of Limit per card (USD)`.
#        - `y = "Balance (USD)"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#        - Set its argument `plot.subtitle` using `element_text(hjust = 0.5)`.
#    Save your plot to `g32`.
## Do not modify this line!
library(tidyr)
credit_pred1<-credit_pred%>%
  rename(Data=Balance,Predictions=pred1)%>%
  pivot_longer(cols=c(Data,Predictions),names_to = "Type",values_to = "Balance")

g32<-ggplot(data=credit_pred1,aes(x=log_limit_per_card,y=Balance,color=Student))+
  geom_point()+
  geom_smooth(aes(linetype=Type),se=F,size=0.5)+
  labs(title = "In general the model captures the pattern between
      Balance and log of limit per card",
       subtitle = "The fit is not ideal for nonstudent group",
       x='Log of Limit per card (USD)',
       y='Balance (USD)')+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g32


# 8. Now we want to visualize the distribution of residuals to check our model
#    performance.
#    To do this, you can use:
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data` and `mapping`.
#      - `geom_ref_line()` to add a reference line.
#        - specify `h=0` to use the x-axis as the reference line.
#      - `geom_point()` to add the scatter plot of residuals.
#      - `labs()` to format the labels such that:
#        - `title = "The residuals goes up as balance goes up"`.
#        - `subtitle = "The range of residuals is pretty wide"`.
#        - `x = "Balance (USD)"`.
#        - `y = "Residuals (USD)"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#        - Set its argument `plot.subtitle` using `element_text(hjust = 0.5)`.
#    Save your plot to `g4`.
## Do not modify this line!
g4<-ggplot(data=credit_pred1%>%filter(Type=="Data"),aes(x=Balance,y=resid1))+
  geom_ref_line(h=0)+
  geom_point()+
  labs(title = "The residuals goes up as balance goes up",
       subtitle = "The range of residuals is pretty wide",
       x="Balance (USD)",
       y="Residuals (USD)")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g4


# 9. Now let's visualize the predictions and true balance against other
#    variables(e.g. `Rating`) to see if the model is doing well.
#    To do that, you can use:
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data`.
#      - `geom_point()` to do a scatter plot for `Balance` againt `Rating`
#      - `geom_smooth()` to add the regression line for `Balance` and `Rating`
#        - specify `se = FALSE`
#        - use `color="Data"` as your color label
#      - `geom_smooth()` to add the regression line for `Balance` and `pred1`
#        - specify `se = FALSE`
#        - use `color="Predictions"` as your color label
#      - `labs()` to format the labels such that:
#        - `title = "The model did not capture the pattern between Rating and Balance"`.
#        - `x = "Rating"`.
#        - `y = "Balance (USD)"`
#        - `color = "Label"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#    Save your plot to `g5`.
## Do not modify this line!
g5<-ggplot(data=credit_pred1,aes(x=Rating,y=Balance))+
  geom_smooth(aes(color=Type),se=F)+
  geom_point(data=credit_pred1%>%filter(Type=="Data"))+
  labs(title = "The model did not capture the pattern between Rating and Balance",
       x="Rating",
       y='Balance (USD)',
       color="Label")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g5


# 10. Now let's add one more predictor `Rating` in our model.
#    Fit another linear model `m2` using these variables:
#    `log_limit_per_card`, `Student`, their second order iteration, and `Rating`.
#    Again, save your prediction residuals into a column
#    `resid2`, save your predictions into a column `pred2` into `credit_pred`.
#    Then visualize the distribution of residuals.
#    To do this, you can use:
#      - `lm()` to fit a linear regression, save the model to `m2`.
#      - `add_residuals` to add residuals to the new tibble `credit_pred`.
#      - `add_predictions` to add predictions to the new tibble `credit_pred`.
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data` and `mapping`.
#      - `geom_ref_line()` to add a reference line.
#        - specify `h=0` to use the x-axis as the reference line.
#      - `geom_point()` to add the scatter plot of residuals.
#      - `labs()` to format the labels such that:
#        - `title = "The residual goes up as balance goes up"`.
#        - `subtitle = "But the slope and the range residuals has decreased a lot"`.
#        - `x = "Balance (USD)"`.
#        - `y = "Residuals (USD)"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#        - Set its argument `plot.subtitle` using `element_text(hjust = 0.5)`.
#    Save your plot to `g6`.
#    And your expected output for `m2 %>% broom::tidy()` shoule be:
#    # A tibble: 5 x 5
#    term                          estimate std.error statistic  p.value
#    <chr>                            <dbl>     <dbl>     <dbl>    <dbl>
#    (Intercept)                    -207.     163.        -1.27 2.05e- 1
#    log_limit_per_card              -25.0     23.5       -1.06 2.88e- 1
#    StudentYes                     -502.     391.        -1.28 2.01e- 1
#    Rating                            2.52     0.104     24.2  8.14e-73
#    log_limit_per_card:StudentYes   120.      52.4        2.29 2.30e- 2
## Do not modify this line!
m2<-lm(Balance~log_limit_per_card*Student+Rating,data=credit_pred)
credit_pred<-credit_pred%>%
  add_residuals(m2)%>%
  add_predictions(m2)%>%
  rename(resid2=resid,pred2=pred)

g6<-ggplot(data=credit_pred,aes(x=Balance,y=resid2))+
  geom_ref_line(h=0)+
  geom_point()+
  labs(title = "The residual goes up as balance goes up",
       subtitle = "But the slope and the range residuals has decreased a lot",
       x="Balance (USD)",
       y="Residuals (USD)")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g6


# 11. After including `Rating` as a predictor, let's do exercise 8 again
#     and check if our new model captures the relationship between `Rating`
#     and `Balance` and compare the new model with our previous model.
#    To do that, you can use:
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data`.
#      - `geom_point()` to do a scatter plot for `Balance` againt `Rating`
#      - `geom_smooth()` to add the regression line for `Balance` and `Rating`
#        - specify `se = FALSE`
#        - use `color="Data"` as your color label
#      - `geom_smooth()` to add the regression line for `Balance` and `pred1`
#        - specify `se = FALSE`
#        - use `color="Predictions 1"` as your color label
#      - `geom_smooth()` to add the regression line for `Balance` and `pred2`
#        - specify `se = FALSE`
#        - use `color="Predictions 2"` as your color label
#      - `labs()` to format the labels such that:
#        - `title = "The 2nd model did much better!"`.
#        - `x = "Rating"`.
#        - `y = "Balance (USD)"`
#        - `color = "Label"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#    Save your plot to `g7`.
## Do not modify this line!
g7<-ggplot(data=credit_pred,aes(x=Rating,y=Balance))+
  geom_smooth(aes(color="Data"),se=F)+
  geom_smooth(aes(color="Predictions 1",y=pred1),se=F)+
  geom_smooth(aes(color="Predictions 2",y=pred2),se=F)+
  geom_point()+
  labs(title = "The 2nd model did much better!",
       x="Rating",
       y="Balance (USD)",
       color="Label")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g7


# 12. By iterating the process above, we can include more variables which
#    improves our model.
#    Let's fit our final model `m3` using these variables:
#    `log_limit_per_card`, `Student`, their second order interaction,
#    `Rating`, `Income`, `Limit` and `Cards`.
#    Again, save your prediction residuals into a column
#    `resid3`, save your predictions into a column `pred3` into `credit_pred`.
#    Then visualize the distribution of residuals.
#    To do this, you can use:
#      - `lm()` to fit a linear regression, save the model to `m3`.
#      - `add_residuals` to add residuals to the new tibble `credit_pred`.
#      - `add_predictions` to add predictions to the new tibble `credit_pred`.
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data` and `mapping`.
#      - `geom_ref_line()` to add a reference line.
#        - specify `h=0` to use the x-axis as the reference line.
#      - `geom_point()` to add the scatter plot of residuals.
#      - `labs()` to format the labels such that:
#        - `title = "The residuals are randomly distributed around x-axis"`.
#        - `subtitle = "Our model is pretty good!"`.
#        - `x = "Balance (USD)"`.
#        - `y = "Residuals (USD)"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#        - Set its argument `plot.subtitle` using `element_text(hjust = 0.5)`.
#    Save your plot to `g8`.
#    And your expected output for `m3 %>% broom::tidy()` should be:
#    # A tibble: 8 x 5
#    term                           estimate std.error  statistic   p.value
#    <chr>                             <dbl>     <dbl>      <dbl>     <dbl>
#    (Intercept)                   -807.      50.6      -15.9     5.92e- 42
#    log_limit_per_card               7.48     6.54       1.14    2.54e-  1
#    StudentYes                     503.      38.2       13.2     1.41e- 31
#    Rating                          -0.190    0.112     -1.70    9.06e-  2
#    Income                         -10.1      0.0567  -179.      2.97e-308
#    Limit                            0.338    0.00782   43.3     1.48e-131
#    Cards                           27.9      2.35      11.9     5.37e- 27
#    log_limit_per_card:StudentYes    0.0195   5.10       0.00382 9.97e-  1
## Do not modify this line!
m3<-lm(Balance~log_limit_per_card*Student+Rating+Income+Limit+Cards,data=credit_pred)
credit_pred<-credit_pred%>%
  add_residuals(m3)%>%
  add_predictions(m3)%>%
  rename(resid3=resid,pred3=pred)

g8<-ggplot(data=credit_pred)+
  geom_ref_line(h=0)+
  geom_point(aes(x=Balance,y=resid3))+
  labs(title = "The residuals are randomly distributed around x-axis",
       subtitle ="Our model is pretty good!",
       x="Balance (USD)",
       y="Residuals (USD)")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g8


# 13. Let's fit the previous model within every `Ethnicity` group, and save
#    the model information into `credit_model`.
#    To do this, you need to:
#      - Load the `purrr` package.
#      - Create a function `f1` that takes an input tibble `df` and perform a
#      linear regression of `Balance` against  `log_limit_per_card`, `Student`,
#      their second order interaction, `Rating`, `Income`, `Limit`, and `Cards`
#      within the input `df`. In other words, `f1(credit_new)`
#      should return `m3`.
#    You can use:
#      - `lm()` to fit a linear regression with formula `Balance ~
#        log_limit_per_card * Student + Rating + Income + Limit + Cards`.
#      - `group_by()` to group `credit_new` by `Ethnicity`
#      - `nest()` to let each row represents a group
#      - `map()` to map the model `m2` to every group
#      - `mutate()`, `map2()` and `add_residuals()` to add the prediction
#       residuals to each group
#      - `unnest()` to unnest the residuals
#    Your final output `credit_model` is a tibble containing the model
#    information for each group and the first few rows should look like:
#    A tibble: 310 x 16
#    Groups:   Ethnicity [3]
#    Ethnicity      data model Income Limit Rating Cards   Age Education Gender
#    <fct>     <list<df> <lis>  <dbl> <dbl>  <dbl> <dbl> <dbl>     <dbl> <fct>
#    African … [78 × 12] <lm>    16.3  1160    126     3    78        13 Male
#    African … [78 × 12] <lm>    44.5  3500    257     3    81        16 Female
#    African … [78 × 12] <lm>    33.0  3180    224     2    28        16 Male
#    African … [78 × 12] <lm>    35.0  3327    253     3    54        14 Female
#    African … [78 × 12] <lm>    50.7  3977    304     2    84        17 Female
#    African … [78 × 12] <lm>    54.7  4116    314     2    70         8 Female
#    African … [78 × 12] <lm>    58.1  4221    304     3    50         8 Male
#    African … [78 × 12] <lm>    29.7  3536    270     2    52        15 Female
#    African … [78 × 12] <lm>    19.1  3291    269     2    75        13 Female
#    African … [78 × 12] <lm>    26.1  3388    266     4    74        17 Female
#    … with 300 more rows, and 6 more variables: Student <chr>, Married <fct>,
#    Balance <dbl>, Limit_per_card <dbl>, log_limit_per_card <dbl>, resid <dbl>
## Do not modify this line!
f1<-function(df) lm(Balance~log_limit_per_card*Student+Rating+Income+Limit+Cards,data=df)
credit_model<-credit_new%>%
  group_by(Ethnicity)%>%
  nest()%>%
  mutate(model=map(data,f1))%>%
  mutate(resid=map2(data,model,add_residuals))%>%
  unnest(resid)
credit_model


# 14. Create a residual plot of `resid` against `Balance`
#     for all the three models. Use different colors
#     to show residuals from different models.
#     To do this, you can use:
#      - `ggplot()` to initialize a ggplot object.
#        Set its arguments `data` and `mapping`.
#      - `geom_point()` to add the scatter plot of residuals.
#        - set `color = Ethnicity` in `aes()`
#      - `geom_smooth()` to add a regression line for residuals for
#        different groups
#        - set `color = Ethnicity` in `aes()`
#        - specify `se = FALSE`
#        - specify `method = "lm"`
#      - `labs()` to format the labels such that:
#        - `title = "For all three groups, the residuals are randomly
#          distributed around x-axis"`.
#        - `subtitle = "Our model is pretty good!"`.
#        - `x = "Balance (USD)"`.
#        - `y = "Residuals (USD)"`.
#      - `theme_light()` to change the theme of plots.
#      - `theme()` to change the title and subtitle to the middle of the plot.
#        - Set its argument `plot.title` using `element_text(hjust = 0.5)`.
#        - Set its argument `plot.subtitle` using `element_text(hjust = 0.5)`.
#    Save your plot to `g9`.
## Do not modify this line!
g9<-ggplot(data=credit_model)+
  geom_point(aes(x=Balance,y=resid,color=Ethnicity))+
  geom_smooth(aes(x=Balance,y=resid,color=Ethnicity),se=F,method="lm")+
  labs(title = "For all three groups, the residuals are randomly
       distributed around x-axis",
       subtitle = "Our model is pretty good!",
       x="Balance (USD)",
       y="Residuals (USD)")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g9


# 15. We want to evaluate the three models by adjusted r-squared.
#    Create a tibble `credit_model_glance` with the adjusted r-squared for
#    each model and sort them by ascending order.
#    To do this, you need to:
#      - Load the `broom` package.
#    You can use:
#      - `nest()` to nest the residuals and feature information back into
#        `resid` in the `credit_model` tibble.
#      - `glance()` to calculate the summary statistics for each model .
#      - `mutate()` and `map()` to save the summary statistics.
#      - `unnest()` to unnest the all the summary statistics.
#      - `arrange()` to sort the tibble.
#    Save your output to `credit_model_glance` whose first few rows should
#    look like:
#    A tibble: 3 x 15
#     Groups:   Ethnicity [3]
#    Ethnicity       data model      resid r.squared adj.r.squared sigma statistic
#    <fct>     <list<df[> <lis> <list<df[>     <dbl>         <dbl> <dbl>     <dbl>
#    Caucasian [158 × 12] <lm>  [158 × 13]     0.997         0.997  20.7     7955.
#    African …  [78 × 12] <lm>   [78 × 13]     0.998         0.998  21.0     5023.
#    Asian      [74 × 12] <lm>   [74 × 13]     0.998         0.998  18.2     5721.
#    … with 7 more variables: p.value <dbl>, df <int>, logLik <dbl>, AIC <dbl>,
#    BIC <dbl>, deviance <dbl>, df.residual <int>A tibble: 3 x 15
## Do not modify this line!
library(broom)
credit_model_glance<-credit_model%>%
  group_by(Ethnicity)%>%
  nest(resid = -c(Ethnicity, data, model))%>%
  mutate(glance=map(model,glance))%>%
  unnest(glance)%>%
  arrange(adj.r.squared)
credit_model_glance

