# HW8: King County House Sales
#
# In this exercise, we will walk you through a complete process of modeling data.
# We suggest the functions you can use to create the tibbles and the plots, but
# you are free to use the methods you are the most comfortable with.
# Make sure that the outputs look exactly like the ones you are supposed to create.
#
# Throughout the exercise:
#    - Do NOT use `for`, `while` or `repeat` loops.
#    - Use `%>%` to structure your operations.
#    - Use `theme_light()` for the plots.
#    - We will not check the comparision figures created by `grid.arrange()`
#      but we hope they will be helpful for you to see the difference directly.
#
# 1. Load the packages `tidyverse`, `GGally`, `gridExtra`, `modelr`, `broom`
#    and `lubridate`.
#    Use `read_csv()` to read the dataset `kc_house_data.csv` (located in the
#    directory `data/`) into a tibble `data`.
#    This dataset has information about house sales in King County between
#    May 2014 and May 2015. You can find a detailed dictionary for the dataset
#    at https://rdrr.io/cran/moderndive/man/house_prices.html. In this dataset,
#    each observation is a house sale, identified by `id` and `date`.
#    In this homework, we are going to explore how `price` of a house depends on
#    other factors.
## Do not modify this line!
library(tidyverse)
library(GGally)
library(gridExtra)
library(modelr)
library(broom)
library(lubridate)
data<-read_csv("data/kc_house_data.csv")


# 2. We will start with data cleaning and aggregation.
#    (1) Check that the dataset has no missing value for each column.
#        To do this, you can use:
#           - `map_lgl()` with `is.na()` and `any()`.
#        Store the result into a variable `check_na`.
#        Your output should be a named logical vector corresponding to the 21
#        columns, with all values as `FALSE`.
#    (2) Create a new tibble `data_clean` of size 21613 x 20 with:
#           - All columns except `yr_built`, `yr_renovated`, `id` and `date`
#             from `data`.
#           - `waterfront` recoded as `factor`, where `"0"` is coded as `"No"`
#              and `"1"` as `"Yes"`.
#           - Three aggregated columns:
#             - `quarter`, the quarter when the house was sold.
#             - `age`, the age of the house in years calculated by
#               `2019 - yr_built`.
#             - `renovated`, an indicator of whether the house has been
#               renovated, i.e., whether `yr_renovated != 0`.
#        To do this, you can use:
#           - `mutate()` to create the new columns
#             - `quarter()` to get the quarter of `date`.
#             - `factor()` to create the desired factors.
#             - `row_number()` to generate the row number.
#           - `dplyr::select()` to select the desired columns.
#        To check your result, `data_clean` prints to:
#        # A tibble: 21,613 x 20
#           price bedrooms bathrooms sqft_living sqft_lot floors waterfront
#           <dbl>    <int>     <dbl>       <int>    <int>  <dbl> <fct>
#        1 221900        3      1           1180     5650      1 No
#        2 538000        3      2.25        2570     7242      2 No
#        3 180000        2      1            770    10000      1 No
#        4 604000        4      3           1960     5000      1 No
#        5 510000        3      2           1680     8080      1 No
#        # … with 2.161e+04 more rows, and 13 more variables: view <int>,
#        #   condition <int>, grade <int>, sqft_above <int>,
#        #   sqft_basement <int>, zipcode <int>, lat <dbl>, long <dbl>,
#        #   sqft_living15 <int>, sqft_lot15 <int>, quarter <fct>, age <dbl>,
#        #   renovated <fct>
## Do not modify this line!
check_na<-map_lgl(data,~any(is.na(.x)))

data_clean<-data%>%
  mutate(quarter=factor(quarter(date)),
         age=2019-yr_built,
         renovated = factor(ifelse(yr_renovated != 0, "Yes", "No")),
         waterfront= fct_recode(factor(waterfront),"No"="0","Yes"="1"))%>%
  select(-yr_built,-yr_renovated,-id,-date)
data_clean

# 3. Before getting deep, we will do some visualization to get a feel for how
#    features are correlated. First, draw and store horizontal boxplots for
#    `price` vs. `waterfront` as `g1` and `price` vs. `renovated` as `g2`.
#    To do this, you can use:
#       - `ggplot()` to initialize a ggplot object and specify variables to plot.
#       - `geom_boxplot()` to plot the boxplots.
#       - `coord_flip()` to make the boxplots horizontal.
#       - `labs()` to format the labels such that:
#          - For `g1`:
#            - `title = "House Price Increases With A View To Waterfront"`
#            - `x = "View to waterfront"`
#            - `y = "Price ($)"`
#          - For `g2`:
#            - `title = "House Price Increases With A Renovation"`
#            - `x = "Renovation"`
#            - `y = "Price ($)"`
#    To facilitate comparison, you can use `grid.arrange()` to arrange the plots
#    side-by-side.
## Do not modify this line!
g1<-ggplot(data=data_clean,aes(x=waterfront,y=price))+
  geom_boxplot()+
  coord_flip()+
  labs(title="House Price Increases With A View To Waterfront",
       x="View to waterfront",
       y="Price ($)")+
  theme_light()

g2<-ggplot(data=data_clean,aes(x=renovated,y=price))+
  geom_boxplot()+
  coord_flip()+
  labs(title="House Price Increases With A Renovation",
       x="Renovation",
       y="Price ($)")+
  theme_light()

# 4. Create a pair plot for `price`,`sqft_lot`, `sqft_above`, `sqft_living` and
#    `sqft_basement`.
#    To do this, you can use:
#       - `ggpairs()` to initialize a ggplot object and specify variables to plot.
#       - `labs()` to format the labels such that:
#         - `title = "House Price Is Correlated With Areas"`
#         - `subtitle = "Collinearity of sqft_living and sqft_above/sqft_basement is seen."`
#    Store the plot into a `ggplot` object `g3`.
#    Remark:
#       - We will exclude `sqft_lot`, `sqft_above` and `sqft_basement` from
#         modeling, for either low correlation or collinearity.
#       - Note that the distribution of `price` is highly skewed. Thus, we will
#         apply a log-transformation on `price` when fitting the model.
## Do not modify this line!

g3<-ggpairs(data=data_clean,columns = c(1,5,11,4,12))+
  labs(title="House Price Is Correlated With Areas",
       subtitle = "Collinearity of sqft_living and sqft_above/sqft_basement is seen.")+
  theme_light()

# 5. We may now start to fit a multiple linear regression model.
#    (1) For our interest, create a new tibble `data_model` of size 21613 x 13
#        to exclude columns `quarter`, `zipcode`, `sqft_above`, `sqft_basement`,
#        `sqft_living15` and `sqft_lot15` from `data_clean`, using methods such
#        as `dplyr::select()`.
#    (2) Use `lm()` to create a linear model `m1` for `log10(price)` against all
#        other features in `data_model`. (hint: use `.` to include all features).
#        To check your result, `m1 %>% broom::tidy()` prints to:
#        # A tibble: 13 x 5
#           term             estimate  std.error statistic   p.value
#           <chr>               <dbl>      <dbl>     <dbl>     <dbl>
#         1 (Intercept)   -20.8       0.776         -26.8  4.98e-156
#         2 bedrooms       -0.00520   0.00105        -4.97 6.59e-  7
#         3 bathrooms       0.0289    0.00178        16.2  1.31e- 58
#         4 sqft_living     0.0000781 0.00000170     45.9  0.
#         5 floors          0.0246    0.00178        13.8  2.50e- 43
#         6 waterfrontYes   0.157     0.00964        16.3  3.85e- 59
#         7 view            0.0289    0.00116        25.0  6.95e-136
#         8 condition       0.0282    0.00129        21.8  1.33e-104
#         9 grade           0.0782    0.00112        69.7  0.
#        10 lat             0.589     0.00576       102.   0.
#        11 long            0.0206    0.00612         3.37 7.56e-  4
#        12 age             0.00143   0.0000400      35.9  1.68e-273
#        13 renovatedYes    0.0279    0.00405         6.88 5.95e- 12
#    (3) Create a tibble `fit_m1` of size 21613 x 2 with columns `resids` and
#        `preds`, which are the residuals and predictions after fitting `m1`
#        on `data_model`.
#        To do this, you can use:
#           - `add_residuals()` to add residuals.
#           - `add_predictions()` to add predictions.
#           - `dplyr::select()` to select the desired columns.
#        To check your result, `fit_m1` prints to:
#        # A tibble: 21,613 x 2
#            resids preds
#             <dbl> <dbl>
#        1   -0.129   5.48
#        2   -0.0694  5.80
#        3   -0.280   5.54
#        4    0.147   5.63
#        5    0.0657  5.64
#        # … with 2.161e+04 more rows
## Do not modify this line!
data_model<-data_clean%>%
  select(-quarter, -zipcode,-sqft_above,-sqft_basement,-sqft_living15,-sqft_lot15,-sqft_lot)

m1<-lm(log10(price)~.,data=data_model)
m1 %>% broom::tidy()


fit_m1<-data_model%>%
  add_residuals(m1)%>%
  add_predictions(m1)%>%
  rename(resids=resid,preds=pred)%>%
  select(resids,preds)
fit_m1
# 6. We will check model assumptions and quality by some diagnostic plots.
#    Draw a scatterplot of `resids` vs. `preds` with a horizontal line of
#    `y = 0` as a reference.
#    To do this, you can use:
#       - `ggplot()` to initialize a ggplot object and specify variables to plot.
#       - `geom_point()` to draw a scatterplot, setting `alpha = 0.3`.
#       - `geom_hline()` to add the line `y = 0`, setting `color = "red"`.
#       - `labs()` to format the labels such that:
#         - `title = "Residuals Almost Have No Pattern"`
#         - `subtitle = "Potential outliers spotted."`
#         - `x = "Fitted Prices ($)"`
#         - `y = "Residuals ($)"`
#    Store the plot into a ggplot object `g4`.
## Do not modify this line!
g4<-ggplot(data=fit_m1,aes(x=preds,y=resids))+
  geom_point(alpha=0.3)+
  geom_hline(yintercept =0,color="red")+
  labs(title = "Residuals Almost Have No Pattern",
       subtitle = "Potential outliers spotted.",
       x="Fitted Prices ($)",
       y="Residuals ($)")+
  theme_light()



# 7. Draw a qq-plot of `resids` to check the normality assumption, with a diagonal
#    qq-line as a reference.
#    To do this, you can use:
#       - `ggplot()` to initialize a ggplot object and specify variables to plot.
#       - `stat_qq()` to draw a qq-plot.
#       - `stat_qq_line()` to add a qq-line.
#       - `labs()` to format the labels such that:
#         - `title = "Residuals Are Almost Normal"`
#         - `subtitle = "Potential outliers are seen at the heavy tail."`
#         - `x = "Theoretical Quantiles"`
#         - `y = "Sample Quantiles"`
#    Store the plot into a ggplot object `g5`.
## Do not modify this line!
g5<-ggplot(data=fit_m1,aes(sample=resids))+
  stat_qq()+
  stat_qq_line()+
  labs(title = "Residuals Are Almost Normal",
       subtitle = "Potential outliers are seen at the heavy tail.",
       x="Theoretical Quantiles",
       y="Sample Quantiles")+
  theme_light()

g5

# 8. Overall, our model has satisfied the assumptions for linear regression.
#    To further improve, we would like to remove the outliers from the dataset.
#    (1) Identify the outliers using Cook's distance with a threshold of `4/n`,
#        where `n` is the total number of observations. Create a tibble `outliers`
#        of size 1249 x 1 with a column `key`, which is a new surrogate key
#        column for the outliers.
#        To do this, you can use:
#           - `augment()` to get `.cooksd` from `m1`.
#           - `mutate()` to generate a `key` column by `row_number()`.
#           - `filter()` to filter points with `.cooksd >= 4 / nrow(.)`.
#           - `dplyr::select()` to select the desired columns.
#        To check your result, `outliers` prints to:
#        # A tibble: 1,249 x 1
#            key
#          <int>
#        1     6
#        2    22
#        3    37
#        4    50
#        5    66
#        # … with 1,244 more rows
#    (2) Create a function `remove_outliers()` that
#        - Takes the data frames `df` and `outliers` as input.
#        - Modifies `df` by:
#           - Creating a new surrogate `key` column using row numbers.
#           - Removing the records for outliers.
#           - Removing the `key` column.
#        To do this, you can use:
#           - `mutate()` to generate a `key` column by `row_number()`.
#           - `anti_join()` to join `df` with `outliers` by `key`.
#           - `dplyr::select()` to select the desired columns.
#    (3) Create a tibble `data_no_outlier` of size 20364 x 13 by applying the
#        function `remove_outliers()` to `data_model`.
#        To check your result, `data_no_outlier` prints to:
#        # A tibble: 20,364 x 13
#           price bedrooms bathrooms sqft_living floors waterfront  view
#           <dbl>    <int>     <dbl>       <int>  <dbl> <fct>      <int>
#        1 221900        3      1           1180      1 No             0
#        2 538000        3      2.25        2570      2 No             0
#        3 180000        2      1            770      1 No             0
#        4 604000        4      3           1960      1 No             0
#        5 510000        3      2           1680      1 No             0
#        # … with 2.036e+04 more rows, and 6 more variables: condition <int>,
#        #   grade <int>, lat <dbl>, long <dbl>, age <dbl>, renovated <fct>
## Do not modify this line!
library(broom)
outliers<-augment(m1)%>%
  mutate(key=row_number())%>%
  filter(.cooksd>=4/nrow(.))%>%
  select(key)

outliers

remove_outliers<-function(df,outliers){
  output<-df%>%
    mutate(key=row_number())%>%
    anti_join(outliers,by="key")%>%
    select(-key)
  return(output)
  
}

data_no_outlier<-remove_outliers(data_model,outliers)

# 9. We will add an interaction term into our model to fit on the new dataset.
#    (1) Create a model `m2` for `log10(price)` against all other features,
#        plus an interaction term of `lat*long` in `data_no_outlier`.
#        To check your result, `m2 %>% broom::tidy()` prints to:
#        # A tibble: 14 x 5
#           term             estimate  std.error statistic   p.value
#           <chr>               <dbl>      <dbl>     <dbl>     <dbl>
#           1 (Intercept)   3104.        226.             13.7  8.44e- 43
#           2 bedrooms        -0.00751     0.000996       -7.54 4.99e- 14
#           3 bathrooms        0.0331      0.00163        20.3  8.83e- 91
#           4 sqft_living      0.0000834   0.00000161     51.8  0.
#           5 floors           0.0253      0.00160        15.8  1.21e- 55
#           6 waterfrontYes    0.161       0.0128         12.6  3.18e- 36
#           7 view             0.0282      0.00111        25.4  2.39e-140
#           8 condition        0.0253      0.00116        21.8  5.60e-104
#           9 grade            0.0758      0.00102        73.9  0.
#           10 lat            -65.1         4.75          -13.7  1.49e- 42
#           11 long            25.6         1.85           13.8  2.02e- 43
#           12 age              0.00152     0.0000366      41.6  0.
#           13 renovatedYes     0.0253      0.00410         6.16 7.43e- 10
#           14 lat:long        -0.538       0.0389        -13.8  2.69e- 43
#    (2) Follow the same procedure in question 5 to generate a tibble `fit_m2`
#        of size 20364 x 2 to store the residuals and predictions by `m2`.
#        To check your result, `fit_m2` prints to:
#        # A tibble: 20,364 x 2
#           resids preds
#            <dbl> <dbl>
#        1  -0.122   5.47
#        2  -0.0843  5.82
#        3  -0.282   5.54
#        4   0.155   5.63
#        5   0.0738  5.63
#        # … with 2.036e+04 more rows
## Do not modify this line!
m2<-lm(log10(price)~.+lat*long,data=data_no_outlier)
m2 %>% broom::tidy()

fit_m2<-data_no_outlier%>%
  add_residuals(m2)%>%
  add_predictions(m2)%>%
  rename(resids=resid,preds=pred)%>%
  select(resids,preds)

fit_m2
# 10. We will observe the improvements in two ways:
#     (1) A higher R-squared.
#         Create a tibble `glance` of size 2 x 12, with columns of statistics
#         obtained from `glance()` and an additional column indicating the model.
#         To do this, you can use:
#            - `glance()` to draw model statistics from `m1` and `m2`.
#            - `bind_rows()` to bind the two tibbles after calling `glance()`,
#              setting `.id = "model"`.
#         To check your result, `glance` prints to:
#         # A tibble: 2 x 12
#           model r.squared adj.r.squared  sigma statistic p.value    df logLik
#           <chr>     <dbl>         <dbl>  <dbl>     <dbl>   <dbl> <int>  <dbl>
#         1 1         0.761         0.761 0.112      5730.       0    13 16682.
#         2 2         0.794         0.794 0.0955     6045.       0    14 18934.
#         # … with 4 more variables: AIC <dbl>, BIC <dbl>, deviance <dbl>,
#         #   df.residual <int>
#     (2) Better residual plots.
#         Reproduce the residual vs. fitted plot and the qq-plot following the same
#         procedure in question 6 and 7. Store the corresponding plots into ggplot
#         objects `g6` and `g7`, respectively. Modify the labels such that:
#           - For `g6`:
#             - `title = "Residuals Have No Pattern"`
#             - `subtitle = "No outliers are found."`
#           - For `g7`:
#             - `title = "Residuals Are Normal"`
#             - `subtitle = "No more deviations at tails."`
#         To facilitate comparison, you can use `grid.arrange()` to compare `g4`
#         to `g6` and `g5` to `g7` side-by-side.
## Do not modify this line!
glance<-bind_rows(list(glance(m1),glance(m2)),.id = "model")
glance

g6<-ggplot(data=fit_m2,aes(x=preds,y=resids))+
  geom_point(alpha=0.3)+
  geom_hline(yintercept =0,color="red")+
  labs(title = "Residuals Have No Pattern",
       subtitle = "No outliers are found.",
       x="Fitted Prices ($)",
       y="Residuals ($)")+
  theme_light()


g7<-ggplot(data=fit_m2,aes(sample=resids))+
  stat_qq()+
  stat_qq_line()+
  labs(title = "Residuals Are Normal",
       subtitle = "No more deviations at tails.",
       x="Theoretical Quantiles",
       y="Sample Quantiles")+
  theme_light()

# 11. So far, we haven't taken the date of sale (`quarter`) into account. Will the
#     model still capture the relationship between `price` and other features
#     for different quarters?
#     To answer this question, create a tibble `by_quarter` of size 4 x 2 with
#     columns `quarter` and `data`, where `quarter` is the quarter when the house
#     was sold and `data` is a list-column for the other features. This dataset
#     should contain only the columns of interest in question 5 and rows without
#     outliers.
#     To do this, you can use:
#       - `dplyr::select()` to exclude columns `zipcode`, `sqft_above`,
#         `sqft_basement`, `sqft_living15` and `sqft_lot15` from `data_clean`.
#       - `remove_outliers()` to remove outliers from the dataset.
#       - `group_by()` to group the data by `quarter`.
#       - `arrange()` to sort the data by ascending `quarter`.
#       - `nest()` to nest the other columns.
#     To check your result, `by_quarter` prints to:
#     # A tibble: 4 x 2
#     # Groups:   quarter [4]
#       quarter            data
#       <fct>   <list<df[,13]>>
#     1 1          [3,836 × 13]
#     2 2          [6,427 × 13]
#     3 3          [5,612 × 13]
#     4 4          [4,489 × 13]
## Do not modify this line!
by_quarter<-data_clean%>%
  select(-zipcode,-sqft_above,-sqft_basement, -sqft_living15,-sqft_lot15,-sqft_lot)%>%
  remove_outliers(outliers)%>%
  group_by(quarter)%>%
  arrange(quarter)%>%
  nest()
by_quarter


# 12. Create a function `f` that takes an input data frame `df` and fits
#     the same linear model as `m2` (i.e., a regression of the `log10(price)`
#     on all the other variables plus the interaction between `lat` and
#     `long`) to `df`.
#     To check your result, `f(data_no_outlier)` outputs `m2`.
#     Next, update the tibble `by_quarter` to have size 4 x 5 with three
#     additional list-columns `model`, `resids` and `preds` after fitting `f`.
#     To do this, you can use:
#        - `mutate()` to create the columns
#          - `model` that maps `f()` to `data`.
#          - `resids` that maps `add_residuals()` to `data` and `model`.
#          - `preds` that maps `add_predictions()` to `data` and `model`.
#     To check your result, `by_quarter` prints to:
#     # A tibble: 4 x 5
#     # Groups:   quarter [4]
#       quarter            data model  resids             preds
#       <fct>   <list<df[,13]>> <list> <list>             <list>
#     1 1          [3,836 × 13] <lm>   <tibble [3,836 × … <tibble [3,836 × …
#     2 2          [6,427 × 13] <lm>   <tibble [6,427 × … <tibble [6,427 × …
#     3 3          [5,612 × 13] <lm>   <tibble [5,612 × … <tibble [5,612 × …
#     4 4          [4,489 × 13] <lm>   <tibble [4,489 × … <tibble [4,489 × …
## Do not modify this line!
f<-function(df) lm(log10(price)~.+lat*long,data=df)

by_quarter<-by_quarter%>%
  mutate(model=map(data,f))%>%
  mutate(resids=map2(data,model,add_residuals))%>%
  mutate(preds=map2(data,model,add_predictions))
by_quarter


# 13. Create a new tibble `by_quarter_fit` of size 20364 x 4 with columns `quarter`,
#     `key`, `resid` and `pred` to store the residuals and predictions, where
#     `key` is a surrogate key column for each record.
#     Note that due to runtime issues, we can not unnest `resids` and `preds`
#     from `by_quarter` in a single command. Instead, we will create two smaller
#     tibbles to unnest `resids` and `preds` one at each time and join them.
#     The two tibbles you need to create, each of size 20364 x 3, are:
#        - `by_quarter_resid`, with columns `quarter`, `key` and `resid`.
#        - `by_quarter_pred`, with columns `quarter`, `key` and `pred`.
#     To do this, you can use:
#        - `unnest()` to unnest `resids`/`preds`.
#        - `mutate()` to generate a `key` column by `row_number()`.
#        - `dplyr::select()` to select the desired columns.
#     Lastly, join `by_quarter_resid` and `by_quarter_pred` as `by_quarter_fit`,
#     using methods such as `inner_join()` with keys `c("quarter", "key")`.
#     To check your result,
#        - `by_quarter_resid` prints to:
#           # A tibble: 20,364 x 3
#           # Groups:   quarter [4]
#             quarter   key   resid
#             <fct>   <int>   <dbl>
#           1 1           1 -0.294
#           2 1           2  0.0641
#           3 1           3  0.0679
#           4 1           4  0.0389
#           5 1           5 -0.0127
#           # … with 2.036e+04 more rows
#        - `by_quarter_pred` prints to:
#           # A tibble: 20,364 x 3
#           # Groups:   quarter [4]
#             quarter   key   resid
#             <fct>   <int> <dbl>
#           1 1           1  5.55
#           2 1           2  5.64
#           3 1           3  5.40
#           4 1           4  5.47
#           5 1           5  5.74
#           # … with 2.036e+04 more rows
#        - `by_quarter_fit` prints to:
#           # A tibble: 20,364 x 4
#           # Groups:   quarter [4]
#             quarter   key   resid  pred
#             <fct>   <int>   <dbl> <dbl>
#           1 1           1 -0.294   5.55
#           2 1           2  0.0641  5.64
#           3 1           3  0.0679  5.40
#           4 1           4  0.0389  5.47
#           5 1           5 -0.0127  5.74
#           # … with 2.036e+04 more rows
## Do not modify this line!
by_quarter_resid<-by_quarter%>%
  unnest(resids)%>%
  mutate(key=row_number())%>%
  select(quarter,key,resid)

by_quarter_pred<-by_quarter%>%
  unnest(preds)%>%
  mutate(key=row_number())%>%
  select(quarter,key,pred)

by_quarter_fit<-inner_join(by_quarter_resid,by_quarter_pred,by=c("quarter", "key"))


# 14. Finally, generate the residuals plots for each of the `quarter`s.
#     To do this, you can use:
#        - `ggplot()` to initialize a ggplot object and specify variables to plot.
#        - `geom_point()` to draw a scatterplot, setting `alpha = 0.3`.
#        - `geom_hline()` to add a line `y = 0`, setting `color = "red"`.
#        - `facet_wrap()` to facet the plots by `quarter`.
#        - `labs()` to format the labels such that:
#          - `title = "Residuals Have No Pattern"`
#          - `subtitle = "All quarters have good fit and safe model assumptions."`
#          - `x = "Fitted Prices ($)"`
#          - `y = "Residuals ($)"`
#     Store the plot into a ggplot object `g8`.
## Do not modify this line!

g8<-ggplot(data=by_quarter_fit,aes(x=pred,y=resid))+
  geom_point(alpha=0.3)+
  geom_hline(yintercept = 0,col="red")+
  facet_wrap(~quarter)+
  labs(title = "Residuals Have No Pattern",
       subtitle = "All quarters have good fit and safe model assumptions.",
       x="Fitted Prices ($)",
       y="Residuals ($)")+
  theme_light()
g8


