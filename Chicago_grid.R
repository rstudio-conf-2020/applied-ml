library(tidymodels)
library(tune)

library(doMC)
registerDoMC(cores = 8)

# ------------------------------------------------------------------------------

set.seed(7898)
data_folds <- rolling_origin(Chicago, initial = 364 * 15, assess = 7 * 4, skip = 7 * 4, cumulative = FALSE)

# ------------------------------------------------------------------------------

library(stringr)
us_hol <- 
  timeDate::listHolidays() %>% 
  str_subset("(^US)|(Easter)")

chi_rec <-
  recipe(ridership ~ ., data = Chicago) %>%
  step_holiday(date, holidays = us_hol) %>%
  step_date(date) %>%
  step_rm(date) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

mars_rec <- 
  chi_rec %>% 
  step_normalize(one_of(!!stations)) %>% 
  step_pca(one_of(!!stations), num_comp = tune("pca comps"))

mars_mod <-  
  mars(num_terms = tune("mars terms"), prod_degree = tune(), prune_method = "none") %>% 
  set_engine("earth") %>% 
  set_mode("regression")

chi_wflow <-
  workflow() %>%
  add_recipe(mars_rec) %>%
  add_model(mars_mod)

chi_grid <- 
  expand.grid(
    `pca comps`  = 0:20,
    prod_degree = 1:2,
    `mars terms` = 2:100
  )

all_res <-
  tune_grid(
    chi_wflow,
    resamples = data_folds,
    grid = chi_grid,
    control = control_grid(verbose = TRUE)
  )

print(all_res$.notes[[1]])

complete_mars_grid <-
  all_res %>% 
  collect_metrics() %>% 
  dplyr::filter(.metric == "rmse")

save(complete_mars_grid, file = "complete_mars_grid.RData")

q("no")


