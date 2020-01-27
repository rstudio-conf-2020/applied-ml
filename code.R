
# ------------------------------------------------------------------------------
# Applied Machine Learning - RStudio::conf, 2020
# Max Kuhn (max@rstudio.com) and Davis Vaughan (davis@rstudio.com)

# ------------------------------------------------------------------------------
# Part 1

library(tidymodels)

# ------------------------------------------------------------------------------

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
# Some Example Data Manipulation Code (slide 11)

library(tidyverse)

ames_prices <- "http://bit.ly/2whgsQM" %>%
  read_delim(delim = "\t", guess_max = 2000) %>%
  rename_at(vars(contains(' ')), list(~gsub(' ', '_', .))) %>%
  dplyr::rename(Sale_Price = SalePrice) %>%
  dplyr::filter(!is.na(Electrical)) %>%
  dplyr::select(-Order, -PID, -Garage_Yr_Blt)

ames_prices %>%
  group_by(Alley) %>%
  summarize(
    mean_price = mean(Sale_Price / 1000),
    n = sum(!is.na(Sale_Price))
  )
# ------------------------------------------------------------------------------
# Examples of purrr::map* (slide 12)

# purrr loaded with tidyverse or tidymodels package

mini_ames <- ames_prices %>%
  dplyr::select(Alley, Sale_Price, Yr_Sold) %>%
  dplyr::filter(!is.na(Alley))

head(mini_ames, n = 5)

by_alley <- split(mini_ames, mini_ames$Alley)
# map(.x, .f, ...)
map(by_alley, head, n = 2)


# ------------------------------------------------------------------------------
# Examples of purrr::map* (slide 13)

map(by_alley, nrow)

map_int(by_alley, nrow)

map(
  by_alley, 
  ~summarise(.x, max_price = max(Sale_Price))
)

# ------------------------------------------------------------------------------
# purrr and list-columns (slide 14)

ames_lst_col <- nest(mini_ames, data = c(Sale_Price, Yr_Sold))
ames_lst_col

ames_lst_col %>%
  mutate(
    n_row = map_int(data, nrow),
    max   = map_dbl(data, ~ max(.x$Sale_Price))
  )

# ------------------------------------------------------------------------------
# Hands-on: Quick Data Investigation (slide 15)

library(tidyverse)
library(AmesHousing)
ames <- make_ames()

theme_set(theme_bw())

# outliers
ggplot(ames, aes(x = Lot_Area)) + 
  geom_histogram()

ggplot(ames, aes(x = Lot_Area)) + 
  geom_histogram() + 
  scale_x_log10()

str(ames$Condition_1)

ggplot(ames, aes(x = Condition_1)) + 
  geom_bar() + 
  coord_flip() 

grep("SF$", names(ames), value = TRUE)

ggplot(ames, aes(x = Total_Bsmt_SF, y = First_Flr_SF)) + 
  geom_point(alpha = .3)

ggplot(ames, aes(x = Lot_Area)) + 
  geom_histogram() + 
  facet_wrap(~Lot_Shape) + 
  scale_x_log10()

ggplot(ames, aes(x = Gr_Liv_Area, Sale_Price)) + 
  geom_point(alpha = .3) + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~Bldg_Type) + 
  geom_smooth(method = lm)


# ------------------------------------------------------------------------------
# Part 2

# ------------------------------------------------------------------------------
# Ames Housing Data (slide 5)

ames <- 
  make_ames() %>% 
  # Remove quality-related predictors
  dplyr::select(-matches("Qu"))
nrow(ames)

# resample functions
# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

nrow(ames_train)/nrow(ames)

# ------------------------------------------------------------------------------
# Ames Housing Data (slide 6)

data_split

training(data_split)

# ------------------------------------------------------------------------------
# A Linear Regression Model (slide 11)

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)

simple_lm_values <- augment(simple_lm)
names(simple_lm_values)

# ------------------------------------------------------------------------------
# parsnip in Action (slide 13)

spec_lin_reg <- linear_reg()
spec_lin_reg

lm_mod <- set_engine(spec_lin_reg, "lm")
lm_mod

lm_fit <- fit(
  lm_mod,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

lm_fit

# ------------------------------------------------------------------------------
# Different interfaces (slide 14)

ames_train_log <- ames_train %>%
  mutate(Sale_Price_Log = log10(Sale_Price))

fit_xy(
  lm_mod,
  y = ames_train_log$Sale_Price_Log,
  x = ames_train_log %>% dplyr::select(Latitude, Longitude)
)

# ------------------------------------------------------------------------------
# Alternative Engines (slide 15)

spec_stan <- 
  spec_lin_reg %>%
  # Engine specific arguments are passed through here
  set_engine("stan", chains = 4, iter = 1000)

# Otherwise, looks exactly the same!
fit_stan <- fit(
  spec_stan,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

coef(fit_stan$fit)

coef(lm_fit$fit)

# ------------------------------------------------------------------------------
# Different models (slide 16)

fit_knn <- 
  nearest_neighbor(mode = "regression", neighbors = 5) %>%
  set_engine("kknn") %>% 
  fit(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)
fit_knn


# ------------------------------------------------------------------------------
# Predictions (slide 18)

# Numeric predictions always in a df
# with column `.pred`

test_pred <- 
  lm_fit %>%
  predict(ames_test) %>%
  bind_cols(ames_test) %>%
  mutate(log_price = log10(Sale_Price))

test_pred %>% 
  dplyr::select(log_price, .pred) %>% 
  slice(1:3)

# ------------------------------------------------------------------------------
# Estimating Performance (slide 19)

# yardstick loaded by tidymodels

perf_metrics <- metric_set(rmse, rsq, ccc)

# A tidy result back:
test_pred  %>% 
  perf_metrics(truth = log_price, estimate = .pred)

# ------------------------------------------------------------------------------
# Part 3

# ------------------------------------------------------------------------------
# Recipes (slide 12)

mod_rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
  step_log(Sale_Price, base = 10)

# ------------------------------------------------------------------------------
# Recipes and Categorical Predictors (slide 13)

mod_rec <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood, 
  data = ames_train
) %>%
  step_log(Sale_Price, base = 10) %>%
  
  # Lump factor levels that occur in 
  # <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

# ------------------------------------------------------------------------------
# Preparing the Recipe (slide 15)

mod_rec_trained <- prep(mod_rec, training = ames_train, verbose = TRUE)

# ------------------------------------------------------------------------------
# Preparing the Recipe (slide 16)

mod_rec_trained

# ------------------------------------------------------------------------------
# Getting the Values (slide 17)

ames_test_dummies <- bake(mod_rec_trained, new_data = ames_test)
names(ames_test_dummies)


# ------------------------------------------------------------------------------
# Hands-On: Zero-Variance Filter(slide 20)

# Instead of using `step_other()`, take 10 minutes and research how to eliminate 
# any zero-variance predictors using the recipe reference site.
# (https://tidymodels.github.io/recipes/reference/index.html)

# Re-run the recipe with this step.

# What were the results?

# Do you prefer either of these approaches to the other?

# ------------------------------------------------------------------------------
# Interactions (slide 22)

price_breaks <- (1:6)*(10^5)

ggplot(
  ames_train, 
  aes(x = Year_Built, y = Sale_Price)
) + 
  geom_point(alpha = 0.4) +
  scale_y_log10() + 
  geom_smooth(method = "loess")

# ------------------------------------------------------------------------------
# Interactions (slide 23)

library(MASS) # to get robust linear regression model

ggplot(
  ames_train, 
  aes(x = Year_Built, 
      y = Sale_Price)
) + 
  geom_point(alpha = 0.4) +
  scale_y_log10() + 
  facet_wrap(~ Central_Air, nrow = 2) +
  geom_smooth(method = "rlm") 

# ------------------------------------------------------------------------------
# Interactions (slide 24)

mod1 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air,                          data = ames_train)
mod2 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air + Year_Built:Central_Air, data = ames_train)
anova(mod1, mod2)

# ------------------------------------------------------------------------------
# Interactions in Recipes (slide 25)

recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  step_log(Sale_Price) %>%
  step_dummy(Central_Air) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  prep(training = ames_train) %>%
  juice() %>%
  # select a few rows with different values
  slice(153:157)

# ------------------------------------------------------------------------------
# Bivariate Data for PCA

data(segmentationData, package = "caret")

segmentationData <- segmentationData[, c("EqSphereAreaCh1", "PerimCh1", "Class", "Case")]
names(segmentationData)[1:2] <- paste0("Predictor", LETTERS[1:2])

segmentationData$Class <- factor(ifelse(segmentationData$Class == "PS", "One", "Two"))

bivariate_data_train <- subset(segmentationData, Case == "Train")
bivariate_data_test  <- subset(segmentationData, Case == "Test")

bivariate_data_train$Case <- NULL
bivariate_data_test$Case  <- NULL

# ------------------------------------------------------------------------------
# A Bivariate Example (slide 27)

library(ggthemes)
ggplot(bivariate_data_test, 
       aes(x = PredictorA, 
           y = PredictorB,
           color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() 

# ------------------------------------------------------------------------------
# A Bivariate Example (slide 27)

bivariate_rec <- recipe(Class ~ PredictorA + PredictorB, 
                        data = bivariate_data_train) %>%
  step_BoxCox(all_predictors())

bivariate_rec <- prep(bivariate_rec, training = bivariate_data_train, verbose = FALSE)

inverse_test <- bake(bivariate_rec, new_data = bivariate_data_test, everything())

ggplot(inverse_test, 
       aes(x = 1/PredictorA, 
           y = 1/PredictorB,
           color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlab("1/A") + ylab("1/B") 


# ------------------------------------------------------------------------------
# Back to the Bivariate Example - Recipes (slide 347)

bivariate_pca <- 
  recipe(Class ~ PredictorA + PredictorB, data = bivariate_data_train) %>%
  step_BoxCox(all_predictors()) %>%
  step_normalize(all_predictors()) %>% # center and scale
  step_pca(all_predictors()) %>%
  prep(training = bivariate_data_test, verbose = FALSE)

pca_test <- bake(bivariate_pca, new_data = bivariate_data_test)

# Put components axes on the same range
pca_rng <- extendrange(c(pca_test$PC1, pca_test$PC2))

ggplot(pca_test, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(alpha = .2, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlim(pca_rng) + ylim(pca_rng) + 
  xlab("Principal Component 1") + ylab("Principal Component 2") 

# ------------------------------------------------------------------------------
# See pca_rotation.R for slide 37

# ------------------------------------------------------------------------------
# Longitude (slide 39)

ggplot(ames_train, 
       aes(x = Longitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  ) + 
  scale_y_log10()

# ------------------------------------------------------------------------------
# Latitude(slide 40)

ggplot(ames_train, 
       aes(x = Latitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::ns(x, df = 5), 
    se = FALSE
  ) + 
  scale_y_log10()

# ------------------------------------------------------------------------------
# Linear Models Again (slide 41)

ames_rec <- 
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  step_ns(Longitude, Latitude, deg_free = 5)

# ------------------------------------------------------------------------------
# Combining the Recipe with a Model (slide 42)

ames_rec <- prep(ames_rec)

lm_fit <- 
  lm_mod %>% 
  fit(Sale_Price ~ ., data = juice(ames_rec))   # The recipe puts Sale_Price on the log scale

glance(lm_fit$fit)

holdout_data <- bake(ames_rec, ames_test, all_predictors())

# but let's not do this
# predict(lm_fit, new_data = holdout_data)

# ------------------------------------------------------------------------------
# An example (slide 45)

ames_wfl <- 
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(lm_mod)
ames_wfl

# ------------------------------------------------------------------------------
# 1-Step fitting and predicting (slide 46)

ames_wfl_fit <- fit(ames_wfl, ames_train)
predict(ames_wfl_fit, ames_test %>% slice(1:5))

# ------------------------------------------------------------------------------
# Part 4

# ------------------------------------------------------------------------------
# Cross-Validating Using {rsample} (slide 10)

set.seed(2453)
cv_splits <- vfold_cv(ames_train) #10-fold is default
cv_splits

cv_splits$splits[[1]]

cv_splits$splits[[1]] %>% analysis() %>% dim()
cv_splits$splits[[1]] %>% assessment() %>% dim()

# ------------------------------------------------------------------------------
# Resampling a 5-NN model (slide 13)

knn_mod <- 
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

knn_wfl <- 
  workflow() %>% 
  add_model(knn_mod) %>% 
  add_formula(log10(Sale_Price) ~ Longitude + Latitude)

## fit(knn_wfl, data = ames_train)

# ------------------------------------------------------------------------------
# Resampling a 5-NN model (slide 14)

knn_res <-
  cv_splits %>%
  mutate( workflows = map(splits, ~ fit( knn_wfl, data = analysis(.x)) ) ) 
knn_res

# ------------------------------------------------------------------------------
# Compute Overall RMSE estimate (slide 22)

knn_pred <-                                              
  map2_dfr(knn_res$workflows, knn_res$splits,     
           ~ predict(.x, assessment(.y)),                
           .id = "fold")                                 

prices <-  
  map_dfr(knn_res$splits,  
          ~ assessment(.x) %>% dplyr::select(Sale_Price)) %>%  
  mutate(Sale_Price = log10(Sale_Price))

rmse_estimates <- 
  knn_pred %>%  
  bind_cols(prices) %>% 
  group_by(fold) %>% 
  do(rmse = rmse(., Sale_Price, .pred)) %>% 
  unnest(cols = c(rmse)) 

mean(rmse_estimates$.estimate)

# ------------------------------------------------------------------------------
# Easy resampling using the {tune} package (slide 26)

easy_eval <- fit_resamples(knn_wfl, resamples = cv_splits, control = control_resamples(save_pred = TRUE))
easy_eval

# ------------------------------------------------------------------------------
# Getting the statistics and predictions (slide 27)

collect_predictions(easy_eval) %>% 
  arrange(.row) %>% 
  slice(1:5) 

collect_metrics(easy_eval)

collect_metrics(easy_eval, summarize = FALSE) %>% 
  slice(1:10)

# ------------------------------------------------------------------------------
# Making Regular Grids (slide 35)

penalty()
mixture()

glmn_param <- parameters(penalty(), mixture())

glmn_param

glmn_grid <- 
  grid_regular(glmn_param, levels = c(10, 5))

glmn_grid %>% slice(1:4)

# ------------------------------------------------------------------------------
# Non-Regular Grids (slide 36)

set.seed(7454)
glmn_sfd <- grid_max_entropy(glmn_param, size = 50)
glmn_sfd %>% slice(1:4)

# ------------------------------------------------------------------------------
# Modifying Parameter Sets (slide 37)

glmn_set <- parameters(lambda = penalty(), mixture())

# The ranges can also be set by their name:
glmn_set <- 
  update(glmn_set, lambda = penalty(c(-5, -1)))

# Some parameters depend on data dimensions:
mtry()

rf_set <- parameters(mtry(), trees())
rf_set

# Sets the range of mtry to be the number of predictors
finalize(rf_set, mtcars %>% dplyr::select(-mpg))

# ------------------------------------------------------------------------------
# Hands-On: K-NN Grids

# ------------------------------------------------------------------------------
# Tagging Tuning parameters (slide 40)

library(tune)
knn_mod <- 
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

parameters(knn_mod)

# ------------------------------------------------------------------------------
# Tagging Tuning parameters (slide 41)

nearest_neighbor(neighbors = tune("K"), weight_func = tune("weights")) %>% 
  set_engine("kknn") %>% 
  set_mode("regression") %>% 
  parameters()

# ------------------------------------------------------------------------------
# Grid Search (slide 42)

set.seed(522)
knn_grid <- knn_mod %>% parameters() %>% grid_regular(levels = c(15, 5))
ctrl <- control_grid(verbose = TRUE)

knn_tune <- 
  tune_grid(ames_rec, model = knn_mod, resamples = cv_splits, grid = knn_grid, control = ctrl)

# ------------------------------------------------------------------------------
# The Results (slide 44)

knn_tune

# results for the first fold:
knn_tune$.metrics[[1]]

# ------------------------------------------------------------------------------
# Resampled Performance Estimates (slide 45)

show_best(knn_tune, metric = "rmse", maximize = FALSE)

# ------------------------------------------------------------------------------
# Part 5

# ------------------------------------------------------------------------------
# Hands-On: Explore the Data (slide 4)

data(Chicago)

# ------------------------------------------------------------------------------
# A Recipe (slide 15)

library(stringr)

# define a few holidays

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
# step_normalize(one_of(!!stations))  #<<
# step_pca(one_of(!!stations), num_comp = tune()) #<<

# ------------------------------------------------------------------------------
# Resampling (slide 16)

chi_folds <- rolling_origin(Chicago, initial = 364 * 15, assess = 7 * 4, skip = 7 * 4, cumulative = FALSE)
chi_folds %>% nrow()

# ------------------------------------------------------------------------------
# Linear Regression Analysis (slide 20)

lm(ridership ~ . - date, data = Chicago)

# ------------------------------------------------------------------------------
# Tuning the Model (slide 26)

glmn_grid <- expand.grid(penalty = 10^seq(-3, -1, length = 20), mixture = (0:5)/5)

# ------------------------------------------------------------------------------
# Tuning the Model (slide 27)

glmn_rec <- chi_rec %>% step_normalize(all_predictors())

glmn_mod <-
  linear_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet")

# Save the assessment set predictions
ctrl <- control_grid(save_pred = TRUE)

glmn_tune <-
  tune_grid(
    glmn_rec,
    model = glmn_mod,
    resamples = chi_folds,
    grid = glmn_grid,
    control = ctrl
  )

# ------------------------------------------------------------------------------
# Plotting the Resampling Profile (slide 30)

rmse_vals <-
  collect_metrics(glmn_tune) %>%
  filter(.metric == "rmse")

rmse_vals %>%
  mutate(mixture = format(mixture)) %>%
  ggplot(aes(x = penalty, y = mean, col = mixture)) +
  geom_line() +
  geom_point() +
  scale_x_log10()

# There is `autoplot(glmn_tune)` but the grid
# structure works better with the code above.

# ------------------------------------------------------------------------------
# Capture the Best Values (slide 31)

show_best(glmn_tune, metric = "rmse", maximize = FALSE)

best_glmn <-
  select_best(glmn_tune, metric = "rmse", maximize = FALSE)
best_glmn

# ------------------------------------------------------------------------------
# Residual Analysis (slide 32)

glmn_pred <- collect_predictions(glmn_tune)
glmn_pred

# ------------------------------------------------------------------------------
# Observed Versus Predicted Plot (slide 33)

# Keep the best model
glmn_pred <-
  glmn_pred %>%
  inner_join(best_glmn, by = c("penalty", "mixture"))

ggplot(glmn_pred, aes(x = .pred, y = ridership)) +
  geom_abline(col = "green") +
  geom_point(alpha = .3) +
  coord_equal()

# ------------------------------------------------------------------------------
# Which training set points had the worst results? (slide 34)

large_resid <- 
  glmn_pred %>% 
  mutate(resid = ridership - .pred) %>% 
  arrange(desc(abs(resid))) %>% 
  slice(1:4)

library(lubridate)
Chicago %>% 
  slice(large_resid$.row) %>% 
  dplyr::select(date) %>% 
  mutate(day = wday(date, label = TRUE)) %>% 
  bind_cols(large_resid)

# ------------------------------------------------------------------------------
# Creating a Final Model (slide 35)

glmn_rec_final <- prep(glmn_rec)

glmn_mod_final <- finalize_model(glmn_mod, best_glmn)
glmn_mod_final

glmn_fit <- 
  glmn_mod_final %>% 
  fit(ridership ~ ., data = juice(glmn_rec_final))

glmn_fit

# ------------------------------------------------------------------------------
# Using the glmnet Object (slide 36)

library(glmnet)
plot(glmn_fit$fit, xvar = "lambda")

# predict(object$fit) Noooooooooooooo!

# ------------------------------------------------------------------------------
# A glmnet Coefficient Plot (slide 37)

library(ggrepel)

# Get the set of coefficients across penalty values
tidy_coefs <-
  broom::tidy(glmn_fit) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::select(-step, -dev.ratio)

# Get the lambda closest to tune's optimal choice
delta <- abs(tidy_coefs$lambda - best_glmn$penalty)
lambda_opt <- tidy_coefs$lambda[which.min(delta)]

# Keep the large values
label_coefs <-
  tidy_coefs %>%
  mutate(abs_estimate = abs(estimate)) %>%
  dplyr::filter(abs_estimate >= 1.1) %>%
  distinct(term) %>%
  inner_join(tidy_coefs, by = "term") %>%
  dplyr::filter(lambda == lambda_opt)

# plot the paths and highlight the large values
tidy_coefs %>%
  ggplot(aes(x = lambda, y = estimate, group = term, col = term, label = term)) +
  geom_vline(xintercept = lambda_opt, lty = 3) +
  geom_line(alpha = .4) +
  theme(legend.position = "none") +
  scale_x_log10() +
  geom_text_repel(data = label_coefs, aes(x = .005))

# ------------------------------------------------------------------------------
# glmnet Variable Importance (slide 39)

library(vip)

vip(glmn_fit, num_features = 20L, 
    # Needs to know which coefficients to use
    lambda = best_glmn$penalty)

# ------------------------------------------------------------------------------
# MARS in via {parsnip} and {tune} (slide 53)

mars_mod <-  mars(prod_degree = tune())

# We'll decide via search:
mars_mod <-  
  mars(num_terms = tune("mars terms"), prod_degree = tune(), prune_method = "none") %>% 
  set_engine("earth") %>% 
  set_mode("regression")

mars_rec <- 
  chi_rec %>% 
  step_normalize(one_of(!!stations)) %>% 
  step_pca(one_of(!!stations), num_comp = tune("pca comps"))

# ------------------------------------------------------------------------------
# Parameter Ranges (slide 69)

chi_wflow <-
  workflow() %>%
  add_recipe(mars_rec) %>%
  add_model(mars_mod)

chi_set <-
  parameters(chi_wflow) %>%
  update(
    `pca comps`  =  num_comp(c(0, 20)), # 0 comps => PCA is not used 
    `mars terms` = num_terms(c(2, 100)))

# ------------------------------------------------------------------------------
# Running the Optimization (slide 70)

library(doMC)
registerDoMC(cores = 8)

ctrl <- control_bayes(verbose = TRUE, save_pred = TRUE)

# Some defaults:
#   - Uses expected improvement with no trade-off. See ?exp_improve().
#   - RMSE is minimized
set.seed(7891)
mars_tune <-
  tune_bayes(
    chi_wflow,
    resamples = chi_folds,
    iter = 25,
    param_info = chi_set,
    metrics = metric_set(rmse),
    initial = 4,
    control = ctrl
  )


# ------------------------------------------------------------------------------
# Performance over iterations (slide 72)

autoplot(mars_tune, type = "performance")


# ------------------------------------------------------------------------------
# Performance versus parameters (slide 73)

autoplot(mars_tune, type = "marginals")

# ------------------------------------------------------------------------------
# Parameters over iterations (slide 74)

autoplot(mars_tune, type = "parameters")

# ------------------------------------------------------------------------------
# Results (slide 75)

show_best(mars_tune, maximize = FALSE)

# ------------------------------------------------------------------------------
# Assessment Set Results (Again) (slide 79)

mars_pred <-
  mars_tune %>%
  collect_predictions() %>%
  inner_join(
    select_best(mars_tune, maximize = FALSE),
    by = c("mars terms", "prod_degree", "pca comps")
  )

ggplot(mars_pred, aes(x = .pred, y = ridership)) +
  geom_abline(col = "green") +
  geom_point(alpha = .3) +
  coord_equal()

# ------------------------------------------------------------------------------
# Finalizing the recipe and model (slide 80)

best_mars <- select_best(mars_tune, "rmse", maximize = FALSE)
best_mars

final_mars_wfl <- finalize_workflow(chi_wflow, best_mars)

# No formula is needed since a recipe is embedded in the workflow
final_mars_wfl <- fit(final_mars_wfl, data = Chicago)

# ------------------------------------------------------------------------------
# Variable importance (slide 81)

final_mars_wfl %>% 
  # Pull out the model
  pull_workflow_fit() %>%
  vip(num_features = 20L, type = "gcv")


# ------------------------------------------------------------------------------
# Part 6

transp <- 
  element_rect(fill = "transparent", colour = NA)

thm <- theme_bw() + 
  theme(
    panel.background = transp, 
    plot.background = transp,
    legend.background = transp,
    legend.key = transp,
    legend.position = "top"
  )

theme_set(thm)

# ------------------------------------------------------------------------------
# Illustrative Example (slide 5)

two_class_example %>% head(4)

# ------------------------------------------------------------------------------
# Class Prediction Metrics (slide 6)

two_class_example %>% 
  conf_mat(truth = truth, estimate = predicted)

two_class_example %>% 
  accuracy(truth = truth, estimate = predicted)

# ------------------------------------------------------------------------------
# The Receiver Operating Characteristic (ROC) Curve (slide 10)

roc_obj <- 
  two_class_example %>% 
  roc_curve(truth, Class1)

two_class_example %>% roc_auc(truth, Class1)

autoplot(roc_obj) + thm

# ------------------------------------------------------------------------------
# Amazon Review Data (slide 14)

library(modeldata)
data(small_fine_foods)

# ------------------------------------------------------------------------------
# Optional step: remove zero-variance predictors (slide 36)

library(textrecipes)

count_to_binary <- function(x) {
  factor(ifelse(x != 0, "present", "absent"),
         levels = c("present", "absent"))
}
text_rec <-
  recipe(score ~ product + review, data = training_data) %>%
  update_role(product, new_role = "id") %>%
  step_mutate(review_raw = review) %>%
  step_textfeature(review_raw) %>%
  step_tokenize(review) %>%
  step_stopwords(review)  %>%
  step_stem(review) %>%
  step_texthash(review, signed = FALSE, num_terms = 1024) %>%
  step_mutate_at(starts_with("review_hash"), fn = count_to_binary) %>%
  step_zv(all_predictors())

# ------------------------------------------------------------------------------
# Resampling and Analysis Strategy (slide 26)

set.seed(8935)
text_folds <- vfold_cv(training_data, strata = "score")

# ------------------------------------------------------------------------------
# {recipe} and {parsnip} objects (slide 34)

library(textfeatures)
library(textrecipes)

tree_rec <-
  recipe(score ~ product + review, data = training_data) %>%
  update_role(product, new_role = "id") %>%
  step_mutate(review_raw = review) %>%
  step_textfeature(review_raw) %>%               
  step_tokenize(review) %>%
  step_stopwords(review)  %>%
  step_stem(review) %>%
  step_texthash(review, signed = FALSE, num_terms = tune()) %>%
  step_zv(all_predictors()) 

# and 

cart_mod <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

ctrl <- control_grid(save_pred = TRUE)

# ------------------------------------------------------------------------------
# Model tuning (slide 35)

cart_wfl <- 
  workflow() %>% 
  add_recipe(tree_rec) %>% 
  add_model(cart_mod)

set.seed(2553)
cart_tune <-
  tune_grid(
    cart_wfl,
    text_folds,
    grid = 10,
    metrics = metric_set(roc_auc),
    control = ctrl
  )
show_best(cart_tune, metric = "roc_auc")

# ------------------------------------------------------------------------------
# Parameter profiles (slide 36)

autoplot(cart_tune)

# ------------------------------------------------------------------------------
# Plotting ROC curves (slide 37)

cart_pred <- collect_predictions(cart_tune)
cart_pred %>% slice(1:5)

cart_pred %>%
  inner_join(select_best(cart_tune)) %>%
  group_by(id) %>%
  roc_curve(score, .pred_great) %>%
  autoplot()

# ------------------------------------------------------------------------------
# A single (but approximate) ROC curve (slide 38)

auc_curve_data <- function(x) {
  collect_predictions(x) %>% 
    inner_join(select_best(x, "roc_auc")) %>% 
    roc_curve(score, .pred_great)
}

approx_roc_curves <- function(...) {
  curves <- map_dfr(list(...), auc_curve_data, .id = "model")
  default_cut <- 
    curves %>% 
    group_by(model) %>% 
    arrange(abs(.threshold - .5)) %>% 
    slice(1)
  ggplot(curves) +
    aes(y = sensitivity, x = 1 - specificity, col = model) +
    geom_abline(lty = 3) + 
    geom_step(direction = "vh") + 
    geom_point(data = default_cut) + 
    coord_equal()
}

# Use named arguments for better labels
approx_roc_curves(CART = cart_tune)

# ------------------------------------------------------------------------------
# Hands-On: Down-Sampling (slide 39)

# Looking at the ROC curve, the default cutoff may not be optimal if FP and FN errors are about equal.

# We could pick a better cutoff or fit another model using sub-class sampling.

# The latter approach would balance the data prior to model fitting.

# The most common method would be to down-sample the data.

# This is fairly controversial (at least in statistical circles).

# Let's take 20m and refit the model code above with a recipe that includes downsampling.

# link to recipes documentation: https://tidymodels.github.io/recipes/reference/index.html


# ------------------------------------------------------------------------------
# C5.0 (slide 50)

C5_mod <- 
  boost_tree(trees = tune(), min_n = tune()) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

C5_wfl <- update_model(cart_wfl, C5_mod)

# We will just modify our CART grid and add 
# a new parameter: 
set.seed(5793)
C5_grid <- 
  collect_metrics(cart_tune) %>% 
  dplyr::select(min_n, num_terms) %>% 
  mutate(trees = sample(1:100, 10))

C5_tune <-
  tune_grid(
    C5_wfl,
    text_folds,
    grid = C5_grid,
    metrics = metric_set(roc_auc),
    control = ctrl
  )

# ------------------------------------------------------------------------------
# Comparing models (slide 51)

approx_roc_curves(CART = cart_tune, C5 = C5_tune)

show_best(C5_tune)

autoplot(C5_tune)


# ------------------------------------------------------------------------------
# Finalizing the recipe and model (slide 52)

best_C5 <- select_best(C5_tune)
best_C5

# no prep-juice calls!
C5_wfl_final <- 
  C5_wfl %>%
  finalize_workflow(best_C5) %>% 
  fit(data = training_data)

C5_wfl_final


# ------------------------------------------------------------------------------
# Predicting the test set (slide 53)

test_probs <- 
  predict(C5_wfl_final, testing_data, type = "prob") %>% 
  bind_cols(testing_data %>% dplyr::select(score)) %>% 
  bind_cols(predict(C5_wfl_final, testing_data))

roc_auc(test_probs, score, .pred_great)

conf_mat(test_probs, score, .pred_class)

roc_values <- 
  roc_curve(test_probs, score, .pred_great)

autoplot(roc_values)

# ------------------------------------------------------------------------------
# Extra Slides


# ------------------------------------------------------------------------------
# Naive Bayes recipe and fit (slide 66)

count_to_binary <- function(x) {
  factor(ifelse(x != 0, "present", "absent"),
         levels = c("present", "absent"))
}

nb_rec <- 
  tree_rec %>%
  step_mutate_at(starts_with("review_hash"), fn = count_to_binary)

library(discrim)

nb_mod <- naive_Bayes() %>% set_engine("klaR")

nb_tune <-
  tune_grid(
    nb_rec,
    nb_mod,
    text_folds,
    grid = tibble(num_terms = floor(2^seq(8, 12, by = 0.5))),
    metrics = metric_set(roc_auc),
    control = ctrl
  )

# ------------------------------------------------------------------------------
# Naive Bayes results (slide 67)

autoplot(nb_tune) +
  scale_x_continuous(trans = log2_trans())

approx_roc_curves(CART = cart_tune, C5 = C5_tune, 
                  "Naive Bayes" = nb_tune)

# ------------------------------------------------------------------------------
# {tidypredict} and {modeldb} (slide 69)

library(tidypredict)
library(dbplyr)

lin_reg_fit <- lm(Sepal.Width ~ ., data = iris)

# R code
tidypredict_fit(lin_reg_fit)

# SQL code
tidypredict_sql(lin_reg_fit, con = simulate_dbi())

# ------------------------------------------------------------------------------
# Multiclass Metrics With yardstick (slide 70)

library(emo)

up <- ji("white_check_mark")
down <- ji("rage")

prec_example <- tibble(
  truth = factor(c(up, down, up, down, down), levels = c(up, down)),
  estimate = factor(c(up, down, up, up, down), levels = c(up, down))
)

prec_example

precision(prec_example, truth, estimate)

# ------------------------------------------------------------------------------
# Macro Averaging (slide 72)

precision(prec_multi, truth, estimate)

# ------------------------------------------------------------------------------
# Caveats (slide 73)

precision(prec_multi, truth, estimate, estimator = "macro_weighted")

