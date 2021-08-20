# ------------------------------------------------------------------------------
# Machine Learning Workshop NYR 2021
# https://github.com/topepo/2021-nyr-workshop

# ------------------------------------------------------------------------------
# Part 1


library(tidymodels)
tidymodels_prefer()
thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)


# ------------------------------------------------------------------------------

mtcars <- mtcars[order(mtcars$cyl),]
mtcars0 <- mtcars[, "mpg", drop = FALSE]


# ------------------------------------------------------------------------------

mtcars$mp        # matches incomplete arg
mtcars[, "mpg"]  # a vector

# ------------------------------------------------------------------------------

mtcars %>%
  arrange(cyl) %>%
  select(mpg)

# ------------------------------------------------------------------------------

tb_cars <- as_tibble(mtcars)
tb_cars$mp        # fails
tb_cars[, "mpg"]  # A tibble

# ------------------------------------------------------------------------------

data("Chicago")
dim(Chicago)
stations

# ------------------------------------------------------------------------------
# Part 2

chi_split <- initial_time_split(Chicago, prop = 1 - (14/nrow(Chicago)))
chi_split

chi_train <- training(chi_split)
chi_test  <- testing(chi_split)

c(training = nrow(chi_train), testing = nrow(chi_test))

simple_lm <- lm(ridership ~ Clark_Lake + humidity, data = chi_train)


# ------------------------------------------------------------------------------

simple_lm_values <- augment(simple_lm)
names(simple_lm_values)

# ------------------------------------------------------------------------------

spec_lin_reg <- linear_reg()
spec_lin_reg

spec_lm <- spec_lin_reg %>% set_engine("lm")
spec_lm


# ------------------------------------------------------------------------------

fit_lm <- fit(
  spec_lm,
  ridership ~ Clark_Lake + humidity,
  data = chi_train
)

fit_lm


# ------------------------------------------------------------------------------

spec_stan <- 
  spec_lin_reg %>%
  # Engine specific arguments are 
  # passed through here
  set_engine("stan", chains = 4, iter = 1000)

# Otherwise, looks exactly the same!
fit_stan <- fit(
  spec_stan,
  ridership ~ Clark_Lake + humidity,
  data = chi_train
)


# ------------------------------------------------------------------------------

coef(fit_stan$fit)

coef(fit_lm$fit)


# ------------------------------------------------------------------------------

reg_wflow <- 
  workflow() %>%    # attached with the tidymodels package
  add_model(spec_lm) %>% 
  add_formula(ridership ~ Clark_Lake + humidity) # or add_recipe() or add_variables()

reg_fit <- fit(reg_wflow, data = Chicago)
reg_fit


# ------------------------------------------------------------------------------

stan_wflow <- 
  reg_wflow %>% 
  update_model(spec_stan)

set.seed(21)
stan_fit <- fit(stan_wflow, data = Chicago)
stan_fit


# ------------------------------------------------------------------------------

# generate some bogus data (instead of using the training or test sets)
set.seed(3)
shuffled_data <- map_dfc(Chicago, ~ sample(.x, size = 10))

predict(stan_fit, shuffled_data) %>% slice(1:3)
predict(stan_fit, shuffled_data, type = "pred_int") %>% slice(1:3)

# ------------------------------------------------------------------------------

pred_results <- 
  predict(stan_fit, shuffled_data) %>% 
  bind_cols(shuffled_data)

# Data was randomized; these results should be bad
pred_results %>% rmse(truth = ridership, estimate = .pred)


# ------------------------------------------------------------------------------

reg_metrics <- metric_set(rmse, rsq, mae, ccc)

# A tidy format of the results
pred_results %>% reg_metrics(truth = ridership, estimate = .pred)


# ------------------------------------------------------------------------------

glance(reg_fit)
tidy(reg_fit)


# ------------------------------------------------------------------------------

augment(reg_fit, shuffled_data %>% select(Clark_Lake, humidity, ridership))

# ------------------------------------------------------------------------------
# Part 3

# ------------------------------------------------------------------------------

chi_rec <- 
  recipe(ridership ~ ., data = chi_train) %>% 
  step_date(date, features = c("dow", "month", "year")) %>% 
  step_holiday(date) %>% 
  update_role(date, new_role = "id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.9) 


lm_spec <- linear_reg() 

chi_wflow <- 
  workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(chi_rec)

chi_wflow


# ------------------------------------------------------------------------------

chi_fit <- chi_wflow %>% fit(chi_train)
chi_fit


# ------------------------------------------------------------------------------

predict(chi_fit, chi_test)


# ------------------------------------------------------------------------------

tidy(chi_rec)


# ------------------------------------------------------------------------------

chi_fit %>% 
  extract_recipe() %>% 
  tidy(number = 5) # For step normalize


# ------------------------------------------------------------------------------
# Part 4

# ------------------------------------------------------------------------------

set.seed(2453)

cv_splits <- vfold_cv(chi_train) #10-fold is default

cv_splits


# ------------------------------------------------------------------------------

cv_splits$splits[[1]]


# ------------------------------------------------------------------------------

cv_splits$splits[[1]] %>% 
  analysis() %>%
  dim()


# ------------------------------------------------------------------------------

cv_splits$splits[[1]] %>% 
  assessment() %>%
  dim()


# ------------------------------------------------------------------------------

chi_rs <-
  chi_train %>%
  sliding_period(
    index = "date",  
    period = "week",
    lookback = 52 * 15,
    assess_stop = 2,
    step = 2 
  )


# ------------------------------------------------------------------------------

chi_rs$splits[[1]] %>% assessment() %>% pluck("date") %>% range()
chi_rs$splits[[2]] %>% assessment() %>% pluck("date") %>% range()


# ------------------------------------------------------------------------------

chi_rs


# ------------------------------------------------------------------------------

lm_spec <- linear_reg() # Use the default `lm` engine

chi_rec <- 
  recipe(ridership ~ ., data = chi_train) %>% 
  step_date(date, features = c("dow", "year")) %>% 
  step_holiday(date) %>% 
  update_role(date, new_role = "id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.9)

chi_wflow <- 
  workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(chi_rec)


# ------------------------------------------------------------------------------

ctrl <- control_resamples(save_pred = TRUE)

chi_res <-
  chi_wflow %>% 
  fit_resamples(resamples = chi_rs, control = ctrl)
chi_res


# ------------------------------------------------------------------------------

collect_metrics(chi_res)


# ------------------------------------------------------------------------------

chi_pred <- collect_predictions(chi_res)
chi_pred %>% slice(1:4)


# ------------------------------------------------------------------------------

chi_pred %>% 
  ggplot(aes(.pred, ridership)) + 
  geom_abline(lty = 2, col = "green") +
  geom_point(alpha = 0.3, cex = 2) +
  coord_obs_pred()


# ------------------------------------------------------------------------------

# Optional
library(shinymodels)
explore(chi_res, hover_cols = c(date, ridership))


# ------------------------------------------------------------------------------

# Add a date column to time series resampling object metrics
add_date_to_metrics <- function(x, date_col, value = min, ...) {
  res <- collect_metrics(x, summarize = FALSE, ...) 
  x %>% 
    mutate(
      # Get the assessment set
      holdout = purrr::map(splits, assessment),
      # Keep the date colum
      holdout = purrr::map(holdout, ~ select(.x, all_of(date_col))),
      # Find a date to represent the range
      date = purrr::map(holdout, ~ value(.x[[date_col]]))
    ) %>% 
    # date is a nested tibble so unnest then merge with results
   unnest(c(all_of(date_col))) %>% 
    select(id, all_of(date_col)) %>% 
    full_join(res, by = "id")
}


# ------------------------------------------------------------------------------

chi_res %>%
  add_date_to_metrics("date") %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = date, y = .estimate)) +
  geom_point() +
  labs(y = "RMSE") +
  scale_x_date(date_breaks = "2 months")

# ------------------------------------------------------------------------------
# Part 5


lm_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") 

chi_rec <- 
  recipe(ridership ~ ., data = chi_train) %>% 
  step_date(date, features = c("dow", "year")) %>% 
  step_holiday(date) %>% 
  update_role(date, new_role = "id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_ns(temp, deg_free = tune()) %>%  
  step_normalize(all_numeric_predictors())

chi_wflow <- 
  workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(chi_rec)


# ------------------------------------------------------------------------------

set.seed(2)
grid <-
  chi_wflow %>%
  parameters() %>%
  grid_latin_hypercube(size = 25)

grid %>%
  ggplot(aes(penalty, mixture, col = deg_free)) +
  geom_point(cex = 4) +
  scale_x_log10()

# ------------------------------------------------------------------------------
# optional for parallel processing:

# Windows:
# library(doParallel)
# cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
# registerDoParallel(cl)

# macOS or linux
# library(doMC)
# registerDoMC(cores = parallel::detectCores(logical = FALSE))

# ------------------------------------------------------------------------------

ctrl <- control_grid(save_pred = TRUE)

set.seed(9)
chi_res <- 
  chi_wflow %>% 
  tune_grid(resamples = chi_rs, grid = grid, control = ctrl) # 'grid' = integer for automatic grids

chi_res

# ------------------------------------------------------------------------------

autoplot(chi_res)

# ------------------------------------------------------------------------------

autoplot(chi_res, metric = "rmse")  +
  ylim(c(1.7, 1.85))

# ------------------------------------------------------------------------------

collect_metrics(chi_res)
collect_metrics(chi_res, summarize = FALSE)


# ------------------------------------------------------------------------------

chi_res %>%
  add_date_to_metrics("date") %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = date, y = .estimate,
             group = .config, col = .config)) +
  geom_line(show.legend = FALSE, alpha = .3) +
  labs(y = "RMSE")


# ------------------------------------------------------------------------------

explore(chi_res, hover_cols = "date")

# ------------------------------------------------------------------------------

show_best(chi_res, metric = "rmse")

smallest_rmse <- select_best(chi_res, metric = "rmse")
smallest_rmse

# ------------------------------------------------------------------------------

chi_wflow <-
  chi_wflow %>% 
  finalize_workflow(smallest_rmse)

test_res <- 
  chi_wflow %>% 
  last_fit(split = chi_split)

test_res


# ------------------------------------------------------------------------------

final_chi_wflow <- 
  test_res$.workflow[[1]]


# ------------------------------------------------------------------------------

collect_metrics(test_res)

# Resampling results
show_best(chi_res, metric = "rmse", n = 1)


# ------------------------------------------------------------------------------

test_res %>%
  collect_predictions() %>%
  ggplot(aes(ridership, .pred)) +
  geom_abline(col = "green", lty = 2) +
  geom_point(cex = 3, alpha = 0.5) +
  coord_obs_pred()

# ------------------------------------------------------------------------------

library(lubridate)
test_values <-
  final_chi_wflow %>%
  augment(testing(chi_split)) %>%
  mutate(day = wday(date, label = TRUE))

test_values %>%
  ggplot(aes(ridership, .pred, col = day)) +
  geom_abline(col = "green", lty = 2) +
  geom_point(cex = 3, alpha = 0.5) +
  coord_obs_pred() +
  scale_color_brewer(palette = "Dark2")

# ------------------------------------------------------------------------------

test_values %>%
  ggplot(aes(date, ridership)) +
  geom_point(aes(col = day),
             cex = 3, alpha = 0.5) +
  geom_line(aes(y = .pred)) +
  scale_color_brewer(palette = "Dark2")
