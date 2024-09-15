# Load necessary libraries
library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)
library(magick)

# Step 1: Generate synthetic data
set.seed(123)
x <- rnorm(100, mean = 5, sd = 2)
y <- 3 + 0.5 * x + rnorm(100)

# Calculate the means of x and y
x_mean <- mean(x)
y_mean <- mean(y)

# Step 2: Function to calculate rotated regression line and R-squared
rotate_line_r2 <- function(angle) {
  # Convert angle to radians
  theta <- angle * pi / 180
  
  # Generate rotated line points
  slope <- tan(theta)
  
  # Calculate predicted y based on the current slope
  y_pred <- y_mean + slope * (x - x_mean)
  
  # Calculate R-squared
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_pred)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  return(list(y_pred = y_pred, r_squared = r_squared, slope = slope))
}

# Step 3: Create data frame to store the rotation angles, slopes, intercepts, and R-squared values
angles <- seq(0, 360, by = 5)
results <- data.frame(angle = angles, r_squared = NA, slope = NA, intercept = NA)

# Step 4: Generate rotated lines and calculate R-squared
for (i in seq_along(angles)) {
  result <- rotate_line_r2(angles[i])
  results$r_squared[i] <- result$r_squared
  results$slope[i] <- result$slope
  results$intercept[i] <- y_mean - results$slope[i] * x_mean
}

# Step 5: Create a list of plots, one for each angle
plots <- list()
for (i in seq_along(angles)) {
  r2 <- results$r_squared[i]
  slope <- results$slope[i]
  intercept <- results$intercept[i]
  
  p <- ggplot() +
    geom_point(aes(x, y), color = "#198038", size = 3) +
    geom_abline(slope = slope, intercept = intercept, color = "#1192e8") +
    labs(title = paste("R-squared:", round(r2, 3)),
         subtitle = paste("Angle:", angles[i], "degrees"),
         x = "X", y = "Y") +
    theme_minimal(base_size = 20)
  
  # Change the title color if R-squared is negative
  if (r2 < 0) {
    p <- p + theme(plot.title = element_text(color = "red"))
  } else {
    p <- p + theme(plot.title = element_text(color = "black"))
  }
  
  plots[[i]] <- p
}

image_list <- lapply(plots, function(plot) {
  # Convert ggplot to an image using ggplotGrob
  plot_image <- magick::image_graph(width = 800, height = 800, res = 96)
  print(plot)
  dev.off()
  plot_image
})

# Step 7: Combine the images into a GIF
gif <- image_animate(image_join(image_list), fps = 10)
image_write(gif, "rotating_regression_custom.gif")