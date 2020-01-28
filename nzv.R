library(tidymodels)
library(AmesHousing)
ames <- make_ames() %>% 
  dplyr::select(-matches("Qu"))
set.seed(333)
data_split <- initial_split(ames, strata = "Sale_Price")
ames_train <- training(data_split)
ames_test  <- testing(data_split)
lm_mod <- linear_reg() %>% 
  set_engine("lm")
perf_metrics <- metric_set(rmse, rsq, ccc)

# -------

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

mod_rec_prepped <- prep(mod_rec, training = ames_train)

juice(mod_rec_prepped)

# -------

mod_rec_dummy <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood, 
  data = ames_train
) %>%
  step_log(Sale_Price, base = 10) %>%
  step_dummy(all_nominal())

mod_rec_dummy_prepped <- prep(mod_rec_dummy, training = ames_train)

train_dummy_data <- juice(mod_rec_dummy_prepped)

train_dummy_data

train_dummy_data %>%
  select(starts_with("Neighborhood_")) %>%
  tidyr::pivot_longer(everything()) %>%
  group_by(name, value) %>%
  count() %>%
  tidyr::pivot_wider(names_from = value, values_from = n) %>%
  rename(one = `1`, zero = `0`) %>%
  filter(one < 20 | is.na(one)) %>%
  mutate(zero / one)

# -------

mod_rec_zv <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood, 
  data = ames_train
) %>%
  step_log(Sale_Price, base = 10) %>%
  step_dummy(all_nominal()) %>% 
  step_zv(
    starts_with("Neighborhood_")
  )

mod_rec_zv_prepped <- prep(mod_rec_zv, training = ames_train)

mod_rec_zv_prepped

juice(mod_rec_zv_prepped)

# -------

mod_rec_nzv <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood, 
  data = ames_train
) %>%
  step_log(Sale_Price, base = 10) %>%
  step_dummy(all_nominal()) %>% 
  step_nzv(
    starts_with("Neighborhood_"), 
    freq_cut = 200/1
  )

mod_rec_nzv_prepped <- prep(mod_rec_nzv, training = ames_train)

mod_rec_nzv_prepped

juice(mod_rec_nzv_prepped)


# -------

mod_rec_nzv2 <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood, 
  data = ames_train
) %>%
  step_log(Sale_Price, base = 10) %>%
  step_dummy(all_nominal()) %>% 
  step_nzv(
    starts_with("Neighborhood_"), 
    freq_cut = 1e10/1,
    unique_cut = 2
  )

mod_rec_nzv2_prepped <- prep(mod_rec_nzv2, training = ames_train)

mod_rec_nzv2_prepped

juice(mod_rec_nzv2_prepped)


