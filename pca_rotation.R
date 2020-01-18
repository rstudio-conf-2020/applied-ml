library(tidymodels)
library(gganimate)
library(ggthemes)

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

data(segmentationData, package = "caret")

segmentationData <- segmentationData[, c("EqSphereAreaCh1", "PerimCh1", "Class", "Case")]
names(segmentationData)[1:2] <- paste0("Predictor", LETTERS[1:2])

segmentationData$Class <- factor(ifelse(segmentationData$Class == "PS", "One", "Two"))

bivariate_data_train <- subset(segmentationData, Case == "Train")
bivariate_data_test  <- subset(segmentationData, Case == "Test")

bivariate_data_train$Case <- NULL
bivariate_data_test$Case  <- NULL

# ------------------------------------------------------------------------------

bc_rec <- 
  recipe(Class ~ PredictorA + PredictorB, data = bivariate_data_train) %>%
  step_BoxCox(all_predictors()) %>% 
  prep()

bc_test <- bake(bc_rec, bivariate_data_test)

ggplot(bc_test, 
       aes(x = PredictorA, 
           y = PredictorB,
           color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlab("1/A") + ylab("1/B")

scaled_rec <- 
  bc_rec %>% 
  step_normalize(all_predictors()) %>% 
  prep()

# ------------------------------------------------------------------------------

bc_scaled <- bake(scaled_rec, bivariate_data_test)

ggplot(bc_scaled, 
       aes(x = PredictorA, 
           y = PredictorB,
           color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlab("1/A") + ylab("1/B")

# ------------------------------------------------------------------------------

rotate <- function(df, angle) {
  mat <- as.matrix(df[, 1:2])
  rot <- diag(rep(1, 2))
  theta <- pi * angle/180
  rot[1, 1] <- cos(theta)
  rot[2, 2] <- cos(theta)
  rot[1, 2] <- -sin(theta)
  rot[2, 1] <- sin(theta)
  result <- mat %*% rot
  colnames(result) <- colnames(df)[1:2]
  result <- as_tibble(result)
  result$Class <- df$Class
  result$angle <- angle
  result
}

# ------------------------------------------------------------------------------

rotations <- map_dfr(seq(1, 360, length = 60), ~ rotate(bc_scaled, .x), .id = "state")

ranges <- 
  rotations %>% 
  group_by(angle) %>% 
  summarize(x_min = min(PredictorA), x_max = max(PredictorA)) %>% 
  ungroup() %>% 
  mutate(y_min = min(rotations$PredictorB) - .25, y_max = min(rotations$PredictorB) - .25)

# ------------------------------------------------------------------------------

rot_anime <- 
  ggplot(rotations, aes(x = PredictorA, y = PredictorB)) + 
  # geom_vline(data = ranges, aes(xintercept = x_min), alpha = .3) + 
  # geom_vline(data = ranges, aes(xintercept = x_max), alpha = .3) + 
  geom_segment(data = ranges, aes(x = x_min, xend = x_max, y = y_min, yend = y_max)) +
  geom_point(aes(col = Class), alpha = .3) + 
  transition_states(
    angle,
    transition_length = 2,
    state_length = 2
  ) + 
  xlab("New Component #1")+ 
  ylab("New Component #2") +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')  +
  scale_colour_calc()  

anim <- gganimate::animate(rot_anime, nframes = 50, fps = 5, detail = 5, width = 1000, height = 1000, res = 200, bg = "#fbf9f4")

anim_save("images/rotate.gif")
