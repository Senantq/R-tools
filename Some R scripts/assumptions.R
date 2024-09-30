library(ggplot2)
library(grid)
library(gtable)
library(e1071)
library(gridExtra)

plot_diagnostics <- function(model) {
  fitted_vals <- fitted(model)
  residuals_vals <- residuals(model, type = "pearson")
  resid_data <- data.frame(residuals_vals)
  sqrt_abs_resid <- sqrt(abs(residuals_vals))
  fits_residuals_data <- data.frame(fitted_vals, residuals_vals)
  
  model_constrained <- lm(sqrt_abs_resid ~ 1)
  model_augmented <- lm(sqrt_abs_resid ~ 1 + fitted_vals)
  BF10 <- exp((-1 / 2) * (BIC(model_augmented) - BIC(model_constrained)))
  SSE_full <- sum(residuals(model_augmented)^2)
  SSE_reduced <- sum(residuals(model_constrained)^2)
  R2_partial <- (SSE_reduced - SSE_full) / SSE_reduced
  
  skew_val <- skewness(residuals_vals)
  kurt_val <- kurtosis(residuals_vals)
  skew_kurt_text <- paste("Skewness:", sprintf("%.3f", skew_val), "| Kurtosis:", sprintf("%.3f", kurt_val))
  bf10_r2_text <- paste("BF10:", formatC(BF10, format = "e", digits = 3), "| R² Partial:", sprintf("%.3f", R2_partial))
  
  p1 <- ggplot(resid_data, aes(x = residuals_vals)) +
    geom_histogram(binwidth = 0.08, fill = "skyblue", color = "black") +
    labs(x = "Residuals", y = "Frequency", title = "Histogram of Residuals") +
    theme(plot.title = element_text(size = 14, face = "bold"), axis.title = element_text(size = 12), axis.text = element_text(size = 10))
  
  g1 <- ggplotGrob(p1)
  g1 <- gtable_add_rows(g1, unit(1, "lines"), -1)
  g1 <- gtable_add_grob(g1, textGrob(skew_kurt_text, gp = gpar(fontsize = 10, fontface = "italic")), t = nrow(g1), l = 4, r = ncol(g1))
  
  n <- length(residuals_vals)
  theoretical_quantiles <- qnorm(ppoints(n))
  sample_quantiles <- sort(residuals_vals)
  z_val <- qnorm(1 - 0.05 / 2)
  phi <- dnorm(theoretical_quantiles)
  ci_upper <- theoretical_quantiles + z_val * sqrt(ppoints(n) * (1 - ppoints(n))) / (sqrt(n) * phi)
  ci_lower <- theoretical_quantiles - z_val * sqrt(ppoints(n) * (1 - ppoints(n))) / (sqrt(n) * phi)
  
  p_qq <- ggplot(data.frame(theoretical_quantiles, sample_quantiles, ci_lower, ci_upper), aes(x = theoretical_quantiles, y = sample_quantiles)) +
    geom_point(color = "blue") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "red", alpha = 0.2) +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "QQ Plot of Residuals") +
    theme(plot.title = element_text(size = 14, face = "bold"), axis.title = element_text(size = 12), axis.text = element_text(size = 10))
  
  p2 <- ggplot(data.frame(fitted_vals, sqrt_abs_resid), aes(x = fitted_vals, y = sqrt_abs_resid)) +
    geom_point(color = "darkred", alpha = 0.6, size = 1) + geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(x = "Fitted Values", y = "Sqrt(|Residuals|)", title = "Spread-Location Plot") +
    theme(plot.title = element_text(size = 14, face = "bold"), axis.title = element_text(size = 12), axis.text = element_text(size = 10), plot.margin = margin(t = 10, r = 10, b = 10, l = 10))
  
  g2 <- ggplotGrob(p2)
  g2 <- gtable_add_rows(g2, unit(1, "lines"), -1)
  g2 <- gtable_add_grob(g2, textGrob(bf10_r2_text, gp = gpar(fontsize = 10, fontface = "italic")), t = nrow(g2), l = 4, r = ncol(g2))
  
  p3 <- ggplot(fits_residuals_data, aes(x = fitted_vals, y = residuals_vals)) +
    geom_point(color = "darkred", alpha = 0.6, size = 1) + geom_smooth(se = TRUE, color = "blue") +
    labs(x = "Fitted Values", y = "Residuals", title = "Fitted Values vs Residuals") +
    theme(plot.title = element_text(size = 14, face = "bold"), axis.title = element_text(size = 12), axis.text = element_text(size = 10))
  
  grid.arrange(g1, p_qq, g2, p3, nrow = 2, ncol = 2)}
