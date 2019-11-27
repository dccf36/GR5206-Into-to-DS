# HW8: Fit different polynomial models
#
# 1. Load the `readr` and `dplyr` package.
#    Read in the file at `"data/polyfit.tsv"` and add an `id` column that
#    contains each row's number and assign the resulting tibble to variable
#    `data_with_id`.
#    To do so, you can do the following:
#    - read in the file at `"data/polyfit.tsv"` using `read_tsv`,
#    - call `mutate()` to create the `id` column using the `row_numbers()`
#      function.
#    `data_with_id` should print to:
#    # A tibble: 200 x 3
#    x      y    id
#    <dbl>  <dbl> <int>
#    1 0.897  0.855     1
#    2 0.266  7.79      2
#    3 0.372  4.52      3
#    4 0.573  3.08      4
#    5 0.908 -1.09      5
#    6 0.202  9.20      6
#    7 0.898 -1.21      7
#    8 0.945  1.63      8
#    9 0.661  2.75      9
#    10 0.629  4.98     10
#    # … with 190 more rows
## Do not modify this line!
library(readr)
library(dplyr)
data_with_id<-read_tsv('data/polyfit.tsv')
data_with_id<-mutate(data_with_id,id=row_number())

# 2. We will now separate the data into two datasets: the train dataset and the
#    validation dataset. The train dataset will be used to fit linear models
#    to our data and the validation dataset will be used to compute the root
#    mean square error (RMSE) and assess the model's ability to generalize to
#    unseen data (unseen in the sense that the data points in the validation
#    dataset were not used to compute the optimal coefficients of the fitted
#    model).
#    To create these datasets, do the following:
#    - first, set the random seed to `0` using `set.seed()` to reproduce
#      identical deterministic results in the following, and save the seed to
#      a vector `seed` using `seed <- .Random.seed`,
#    - then, compute the `train_data` tibble by (randomly!) sampling 80 % of
#      the data in `data_with_id` and add a `split` column containing the
#      string value `"train"` for all rows. To do so, you can:
#          - use `sample_frac()` to shuffle the data and keep only 80 % of the
#          rows,
#          - then use `mutate()` to create the constant `split` column with
#          value `"train"` for all rows
#    - then, compute the `test_data` tibble, containing all the remaining data
#      (20 % of the original rows) and a `split` column with constant value
#      `"test"`. The `id` column computed in question 3. now comes in handy.
#      You can do the following:
#          - start with `data_with_id`,
#          - use `anti_join` on the `train_data` tibble by the `id` key to keep
#            only rows that have `id`s not in `train_data`'s `id`s,
#          - then use `mutate()` to create the constant `split` column with
#            value `"test"` for all rows
#    - finally, concatenate `train_data` and `test_data` row-wise, in this
#      order, and assign the resulting tibble to `all_data`. (you can use
#      `bind_rows()` to do so.)
#    `train_data` should print to:
#    # A tibble: 160 x 4
#    x     y    id split
#    <dbl> <dbl> <int> <chr>
#    1 0.605 1.58    180 train
#    2 0.861 0.180    53 train
#    3 0.347 5.42     74 train
#    4 0.732 1.34    113 train
#    5 0.741 1.94    179 train
#    6 0.724 1.64     40 train
#    7 0.391 4.21    175 train
#    8 0.191 8.99    183 train
#    9 0.454 2.96    127 train
#    10 0.640 4.72    121 train
#    # … with 150 more rows
#    `test_data` should print to:
#    # A tibble: 40 x 4
#    x      y    id split
#    <dbl>  <dbl> <int> <chr>
#    1 0.897  0.855     1 test
#    2 0.908 -1.09      5 test
#    3 0.992  0.495    19 test
#    4 0.380  5.01     20 test
#    5 0.935  0.205    22 test
#    6 0.267 10.1      26 test
#    7 0.382  3.57     29 test
#    8 0.870  0.207    30 test
#    9 0.477  3.32     49 test
#    10 0.732  1.22     50 test
#    # … with 30 more rows
#    `all_data` should print to:
#    # A tibble: 200 x 4
#    x     y    id split
#    <dbl> <dbl> <int> <chr>
#    1 0.605 1.58    180 train
#    2 0.861 0.180    53 train
#    3 0.347 5.42     74 train
#    4 0.732 1.34    113 train
#    5 0.741 1.94    179 train
#    6 0.724 1.64     40 train
#    7 0.391 4.21    175 train
#    8 0.191 8.99    183 train
#    9 0.454 2.96    127 train
#    10 0.640 4.72    121 train
#    # … with 190 more rows
## Do not modify this line!
set.seed(0)
seed<-.Random.seed
train_data<-data_with_id%>%
  sample_frac(0.8)%>%
  mutate(split='train')

test_data<-data_with_id%>%
  anti_join(train_data,by='id')%>%
  mutate(split='test')
all_data<-bind_rows(train_data,test_data)
# 3. Load the `modelr` package.
#    Fit a quadratic model to the `train_data` and assign the result to
#    `model_quad`.
#    You should use `lm()` and the `poly()` function, setting its `degree` to
#    `2`.
#    Add the residuals of `model_quad` to `all_data` using `add_residuals()`
#    and assign the resulting tibble to `data_with_quad_resid`.
#    `data_with_quad_resid` should print to:
#     # A tibble: 200 x 5
#     x     y    id split  resid
#     <dbl> <dbl> <int> <chr>  <dbl>
#     1 0.605 1.58    180 train -2.03
#     2 0.861 0.180    53 train -0.255
#     3 0.347 5.42     74 train  0.338
#     4 0.732 1.34    113 train -0.910
#     5 0.741 1.94    179 train -0.197
#     6 0.724 1.64     40 train -0.713
#     7 0.391 4.21    175 train -0.743
#     8 0.191 8.99    183 train  3.85
#     9 0.454 2.96    127 train -1.73
#     10 0.640 4.72    121 train  1.44
#     # … with 190 more rows
## Do not modify this line!
library(modelr)
model_quad<-lm(y~poly(x,2,raw=TRUE),data=train_data)
  
data_with_quad_resid<-add_residuals(all_data,model_quad)

# 4. Load the `ggplot2` package.
#    Use `theme_set()` to set the theme to `theme_light()`.
#     It will automatically apply `theme_light()` to every plot you draw.
#    Generate a residual scatter plot (the x-axis should be `x` and the y-axis
#    should be `resid`) with a horizontal line of equation `resid = 0` and
#    assign it to `quad_resid_plot`.
#    To do so, you should do the following:
#    - call `ggplot()` on `data_with_quad_resid` using the `x` and `resid`
#      columns
#    - add a horizontal line at 0 (using `geom_ref_line()` for example),
#    - add a call to `geom_point()` to add the residuals
#    - use `labs()` to set the following labels:
#      - `y = "Residuals"`,
#      - `title = "The residuals show a secondary pattern that is not captured"`
#    - assign the result to `quad_resid_plot`
#    You should notice that the residuals are visibly not independent of
#    another, and that there is a clear pattern in their distribution according
#    to `x` that is far from uniform noise around 0.
#    We will find a better model to fit the data to capture this pattern in the
#    following.
## Do not modify this line!
library(ggplot2)
theme_set(theme_light())
quad_resid_plot<-ggplot(data_with_quad_resid,aes(x=x,y=resid))+
  geom_ref_line(h=0)+
  geom_point()+
  labs(title='The residuals show a secondary pattern that is not captured',
       y='Residuals')
quad_resid_plot
# 5. Load the `purrr` and `tidyr` package.
#    Create a `my_model()` function that takes in an argument `k` and returns a
#    fitted linear model on top of polynomials of `x` up to degree `k` with
#    respect to `y`, on the `train_data`.
#    You can use `lm()` and `poly(x, k, raw = TRUE)`
#    Then generate a tibble containing the entire original data, as well as
#    predictions and residuals for `k`-degree polynomial models for `k`
#    between 1 and 10 (both included).
#    To do so, you can:
#    - create a tibble with a column `k = 1:10`,
#    - add a `model` column containing the `k` order model (computed using
#      `map()` and `my_model()`) using `mutate()`,
#    - add a `data` column that contains the data, predictions and residuals
#      for the model of the same row, computed using `map()`,
#      `add_predictions()` and `add_residuals()` - using `mutate()` again,
#    - unnest the `data` column using `unnest()`
#    - keep all columns except for `model` using `select()`
#    `model_pred_resid` should print to:
#     # A tibble: 2,000 x 7
#            k     x     y    id split  pred    resid
#        <int> <dbl> <dbl> <int> <chr> <dbl>    <dbl>
#      1     1 0.605 1.58    180 train  2.84 -1.26
#      2     1 0.861 0.180    53 train  1.19 -1.01
#      3     1 0.347 5.42     74 train  4.50  0.920
#      4     1 0.732 1.34    113 train  2.02 -0.680
#      5     1 0.741 1.94    179 train  1.96 -0.0235
#      6     1 0.724 1.64     40 train  2.07 -0.436
#      7     1 0.391 4.21    175 train  4.22 -0.00553
#      8     1 0.191 8.99    183 train  5.50  3.49
#      9     1 0.454 2.96    127 train  3.81 -0.857
#     10     1 0.640 4.72    121 train  2.61  2.10
#     # … with 1,990 more rows
## Do not modify this line!
library(purrr)
library(tidyr)
my_model<-function(k){
  return(lm(y~poly(x,k,raw=TRUE),data=train_data))
}
model_pred_resid<-tibble(k=1:10)%>%
  mutate(model=map(k,my_model))%>%
  mutate(data=map(model,~add_residuals(add_predictions(all_data,.),.)))%>%
  unnest(data)%>%
  select(-model)


model_pred_resid
# 6. Create a function `mse` that computes the Mean Squared Error between two
#    vectors `y` and `pred`.
#    Create a tibble `model_mse` that contains the MSE for each of the
#    polynomial models, on each split (`train` and `test`).
#    To do so, you can:
#    - group `model_pred_resid` by `split` and `k` using `group_by()`,
#    - compute the MSE on each group using `mse()` and `summarize()`
#    `model_mse` should print to:
#     # A tibble: 20 x 3
#     # Groups:   split [2]
#        split     k   mse
#        <chr> <int> <dbl>
#      1 test      1 5.42
#      2 test      2 4.73
#      3 test      3 2.81
#      4 test      4 1.90
#      5 test      5 1.82
#      6 test      6 1.36
#      7 test      7 1.02
#      8 test      8 1.04
#      9 test      9 0.992
#     10 test     10 0.994
#     11 train     1 4.97
#     12 train     2 4.17
#     13 train     3 2.95
#     14 train     4 2.01
#     15 train     5 1.78
#     16 train     6 1.42
#     17 train     7 1.02
#     18 train     8 1.01
#     19 train     9 0.987
#     20 train    10 0.987
## Do not modify this line!
mse<-function(y,pred){
  return(sum((y-pred)^2)/length(y))
}
model_mse<-model_pred_resid%>%
  group_by(split,k)%>%
  summarize(mse=mse(y,pred))
model_mse
# 7. Create a plot showing how the train and test error vary with the
#    polynomial degree of the fitted model and assign it to variable
#    `plot_mse`.
#    To do so, you can do the following - in the same order:
#    - call `ggplot()` on `model_mse` with aesthetic containing `k` for
#      x-values, `mse` for y-values and color by `split`
#    - add a line plot  using `geom_line()`,
#    - enforce the x-axis to show ticks at integer values from 1 to 10
#      using `scale_x_continuous()` and setting its `breaks` argument to
#     `1:10`,
#    - use `labs()` to add the following labels:
#      - `x = "Polynomial Degree"`,
#      - `y = "RMSE"`,
#      - `title = "Train and test error vary with the polynomial degree"`,
#      - `subtitle = "Test error first decreases, then increases"`
## Do not modify this line!
plot_mse<-ggplot(model_mse,aes(x=k,y=mse,color=split))+
  geom_line()+
  scale_x_continuous(breaks = 1:10)+
  labs(title = "Train and test error vary with the polynomial degree",
       subtitle = "Test error first decreases, then increases",
       x="Polynomial Degree",
       y="MSE")
plot_mse


# 8. Find the degree with the smallest validation error and assign it to
#    variable `best_deg`.
#    To do so you can modify `model_mse` and do the following:
#    - filter only the rows containing validation errors and in which the
#      error is equal to the minimum error using `filter()` on the `split`
#      column to keep the errors on the validation set,
#    - filter on `mse` column, with `min()` used in the filtering condition
#      on `mse`,
#    - then, keep only the value of `k` using `pull()`
## Do not modify this line!
best_deg<-model_mse%>%
  filter(split=='test')%>%
  filter(mse==min(mse))%>%
  pull(k)

best_deg
# 9. Generate a `residual_plot` figure by plotting the residuals of the best
#    polynomial model in terms of lowest test error with a horizontal reference
#    line at 0.
#    To do so you can:
#    - filter on `k` to keep only residuals for `k == best_deg` using
#      `filter()` on `model_pred_resid`,
#    - call `ggplot()` with `x` on the x-axis and `resid` on the y-axis,
#    - add points using `geom_point()`,
#    - add a horizontal line at 0 using `geom_hline()` (make its size be 1),
#    - use `labs()` to add the following labels:
#      - `y = "Residuals"`,
#      - `title = "No clear pattern in the residuals"`,
#      - `subtitle = "The model did its job!"`
## Do not modify this line!
residual_plot<-model_pred_resid%>%
  filter(k==best_deg)%>%
  ggplot(mapping=aes(x=x,y=resid))+
  geom_point()+
  geom_hline(yintercept = 0,size=1)+
  labs(title = 'No clear pattern in the residuals',
       subtitle = 'The model did its job!',
       y='Residuals')
residual_plot


# 10. Generate a scatter plot of all the data with two models - the best degree
#     and `k=7` - overlayed as lines and assign it to variable
#     `prediction_plot`.
#     To do so, you can:
#     - filter on `k` to keep only residuals for the two models using
#       `filter()` on `model_pred_resid`,
#     - transform `k` to a factor column using `mutate()` and `factor()`,
#     - call `ggplot()` with the aesthetic `x = x` and `y = y`,
#     - add the scatter plot with `geom_point()`,
#     - add the prediction line with a new aesthetic with `y = pred` and
#       `color = k`, with lines of `size = 1` using `geom_line()`
#     - use `labs()` to add the following labels:
#      - `color = "Polynomial Degree"`,
#      - `title = "Both models fit the data quite well"`,
#      - `subtitle = "Except close to the boundaries, is the model with k = 9 really better?"`
## Do not modify this line!
prediction_plot<-model_pred_resid%>%
  filter(k==7 |k==best_deg)%>%
  mutate(k=factor(k))%>%
  ggplot(aes(x=x,y=y))+
  geom_point()+
  geom_line(aes(y=pred,color=k),size=1)+
  labs(title = 'Both models fit the data quite well',
       subtitle = 'Except close to the boundaries, is the model with k = 9 really better?',
       color='Polynomial Degree')
prediction_plot


# 11. We want to force the model to choose the smallest degree possible
#     among the models that have similar performances.
#     Create a function `mse_penalized()` with arguments `y`, `pred`, `k` and
#     `lambda` (`lambda` should have a default value of `0`) to compute a
#     penalized MSE that adds returns the `MSE(y,pred) + lambda * k`.
#     You can assume that `k` is an integer, and that `y` and `pred`
#     are numeric vectors of same length.
#     To calculate the MSE, you can calculate the mean of the squared difference.
#     Then, create a function `my_mse_penalized()` with arguments `df`, `k` and
#     `lambda` that returns a tibble with two columns:
#     - `loss` should contain the output of `mse_penalized()` computed
#       on the `y` and `pred` columns of `df`, and `k` and `lambda`,
#     - `lambda` should contain the value of `lambda`
#     You can assume that `df` is a dataframe containing two columns `y` and
#     `pred` of same length.
#     Use `my_mse_penalized()` to create a tibble `model_mse_penalized` from
#     `model_pred_resid` with values of the penalized penalized for each value
#     of `k`, as well as for each value of `lambda` in `(0, 0.1, 0.4)`.
#     To do so, you can:
#     - group `model_pred_resid` by `k` and `split`,
#     - nest the groups using `nest()`,
#     - add an `errors` column using `map()` on `data` to apply
#       `my_mse_penalized()` with the values of `lambda` given above and the
#       value of `k` of each group, using `mutate()`,
#     - unnest `errors` using `unnest()`,
#     - keep all columns except `data` using `select()`
#     `model_mse_penalized` should print to:
#     # A tibble: 60 x 4
#     # Groups:   k, split [20]
#            k split  loss lambda
#        <int> <chr> <dbl>  <dbl>
#      1     1 train  4.97    0
#      2     1 train  5.07    0.1
#      3     1 train  5.37    0.4
#      4     1 test   5.42    0
#      5     1 test   5.52    0.1
#      6     1 test   5.82    0.4
#      7     2 train  4.17    0
#      8     2 train  4.37    0.1
#      9     2 train  4.97    0.4
#     10     2 test   4.73    0
#     # … with 50 more rows
## Do not modify this line!
mse_penalized<-function(y,pred,k,lambda=0){
  return(mse(y,pred) + lambda * k)
}
my_mse_penalized<-function(df,k,lambda){
  return(tibble(loss=mse_penalized(df$y,df$pred,k,lambda),
                lambda=lambda))
}
model_mse_penalized<-model_pred_resid%>%
  group_by(k,split)%>%
  nest()%>%
  mutate(errors=map(data,my_mse_penalized,k=k,lambda=c(0,0.1,0.4)))%>%
  unnest(errors)%>%
  select(-data)
model_mse_penalized

# 12. Create a `mse_penalized_plot` from `model_mse_penalized` that shows the
#     variation of the `loss` with `k` for each value of `lambda` on each
#     `split`.
#     To do so, you can:
#     - transform the `lambda` column of `model_mse_penalized` to a factor
#       column,
#     - call `ggplot()` with an aesthetic that maps `k` to the x-axis, `loss`
#       to the y-axis, `split` to the color and `lambda` to `linetype`,
#     - add lines using `geom_line()`,
#     - use `labs()` to add the following labels:
#      - `color = "Split"``,
#      - `x = "Polynomial Degree"``,
#      - `y = "Loss"`,
#      - `linetype = expression(lambda)`,
#      - `title = "A stronger penalization implies a smaller optimal degree"`,
#      - `subtitle = "expression(paste(lambda, " = 0 corresponds to the unpenalized case"))"`.
## Do not modify this line!
mse_penalized_plot<-model_mse_penalized%>%
  mutate(lambda=factor(lambda))%>%
  ggplot(aes(x=k,y=loss,color=split,linetype=lambda))+
  geom_line()+
  labs(title='A stronger penalization implies a smaller optimal degree',
       subtitle = expression(paste(lambda, " = 0 corresponds to the unpenalized case")),
       x='Polynomial Degree',
       y='Loss',
       color='Split',
       linetype=expression(lambda))

mse_penalized_plot
# 13. Find the best degree according to penalized loss on the `test` split for
#     each value of the penalization parameter (`lambda`, which is in
#     `(0, 0.1, 0.4)`) and assign the tibble containing rows from
#     `model_mse_penalized` with the best degree to `best_deg_penalized`.
#     To do so, you can:
#     - keep only the `test` rows of `model_mse_penalized` using `filter()`,
#     - group the result by `lambda` using `group_by()`,
#     - keep only the rows with minimal loss using `filter()` and `min()`
#     Join `best_deg_penalized` to `model_pred_resid` to keep the data,
#     predictions and residuals for the polynomial models of degrees selected
#     by the penalized models and assign the resulting tibble to
#     `model_pred_resid_best`.
#     To do so, you can:
#     - left join `best_deg_penalized` to `model_pred_resid_best` on `k`,
#     - remove NA values using `drop_na()`
#     `best_deg_penalized` should print to:
#     # A tibble: 3 x 4
#     # Groups:   lambda [3]
#           k split  loss lambda
#       <int> <chr> <dbl>  <dbl>
#     1     4 test  3.50     0.4
#     2     7 test  1.72     0.1
#     3     9 test  0.992    0
#     `model_pred_resid_best` should print to:
#     # A tibble: 600 x 10
#            k     x     y    id split.x  pred   resid split.y  loss lambda
#        <int> <dbl> <dbl> <int> <chr>   <dbl>   <dbl> <chr>   <dbl>  <dbl>
#      1     4 0.605 1.58    180 train    2.25 -0.675  test     3.50    0.4
#      2     4 0.861 0.180    53 train    1.26 -1.08   test     3.50    0.4
#      3     4 0.347 5.42     74 train    6.37 -0.944  test     3.50    0.4
#      4     4 0.732 1.34    113 train    1.69 -0.347  test     3.50    0.4
#      5     4 0.741 1.94    179 train    1.67  0.265  test     3.50    0.4
#      6     4 0.724 1.64     40 train    1.70 -0.0656 test     3.50    0.4
#      7     4 0.391 4.21    175 train    5.55 -1.34   test     3.50    0.4
#      8     4 0.191 8.99    183 train    7.38  1.62   test     3.50    0.4
#      9     4 0.454 2.96    127 train    4.36 -1.40   test     3.50    0.4
#     10     4 0.640 4.72    121 train    2.00  2.71   test     3.50    0.4
#     # … with 590 more rows
## Do not modify this line!
best_deg_penalized<-model_mse_penalized%>%
  filter(split=='test')%>%
  group_by(lambda)%>%
  filter(loss==min(loss))

model_pred_resid_best<-model_pred_resid%>%
  left_join(best_deg_penalized,by=c('k'))%>%
  drop_na()

# 14. Generate residual plots for the three different values of `k` retained
#     by the penalized criterion with reference lines at 0 and assign the plot
#     to `residual_plot_best`.
#     To do so, you can:
#     - use `ggplot()` on `model_pred_resid_best`, with `x` on the x-axis and
#       `"Residuals"` on the y-axis
#     - add points using `geom_point()`,
#     - add a horizontal line at 0 using `geom_hline()` (make its size be 1),
#     - facet the plot by `k` using `facet_wrap()`
#     - use `labs()` to add the following labels:
#       - `y = "Residuals"`,
#       - `title = "No clear pattern in the residuals, except maybe for k = 6"`,
#       - `subtitle = "Models with k = 7 and k = 9 seem to have done their job!"`
#
## Do not modify this line!

residual_plot_best<-ggplot(data=model_pred_resid_best,aes(x=x,y=resid))+
  geom_point()+
  geom_hline(yintercept = 0,size=1)+
  facet_wrap(~k)+
  labs(title = 'No clear pattern in the residuals, except maybe for k = 6',
       subtitle = 'Models with k = 7 and k = 9 seem to have done their job!',
       y='Residuals')
residual_plot_best

