library(tidymodels)
library(rpart)

set.seed(111221)

n <- nrow(two_class_dat)

grid <- expand.grid(A = seq(0, 4, length.out = 200), B = seq(0, 4, length.out = 200))

two_class_dat <- 
  two_class_dat %>% 
  mutate(
    pred = Class,
    wts = 1/n,
    wts_show = 1.5,
    in_fit = runif(n) < 3/4
  )

boost_plots <- vector(length = 5, mode = "list")

for (i in 1:5) {
  fit_dat <- two_class_dat %>% dplyr::filter(in_fit)
  
  rp <- rpart(Class ~ A + B, data = fit_dat, weights = fit_dat$wts)
  
  pred_grid <- grid
  pred_grid$probs <- predict(rp, grid)[,1]
  pred_grid$pred <- predict(rp, grid, type = "class")
  
  p <- 
    ggplot(two_class_dat, aes(x = A, y = B)) + 
    geom_contour(data = pred_grid, aes(z = probs), breaks = .5, col = "black") + 
    geom_raster(data = pred_grid, aes(fill = pred), alpha = .1) +
    ggtitle(paste("Iteration", i)) + 
    guides(size = "none") + 
    xlim(c(0, 3.9)) + 
    ylim(c(0.3, 4.1)) + 
    scale_size(range = c(.25, 4))
  
  if (i == 0) {
    p <- 
      p +  
      geom_point(aes(col = Class), alpha = .5) 
  } else {
    p <- 
      p +  
      geom_point(aes(col = Class, size = wts), alpha = .5) 
  }
  
  
  two_class_dat <- 
    two_class_dat %>% 
    mutate(
      pred = predict(rp, two_class_dat, type = "class"),
      wts = ifelse(pred == Class, wts / 1.2, wts * 1.2),
      wts = wts/sum(wts),
      wts_show = (wts - min(wts))/(max(wts) - min(wts)) + .5,
      in_fit = runif(n) < 3/4
    )
  boost_plots[[i]] <- p
}

