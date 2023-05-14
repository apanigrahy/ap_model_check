# This function is used to perform model diagnostics on simple/multivariate linear regression models
# Requires dplyr, ggplot2, patchwork

lm_model_check <- function(model){
  model_data <- data.frame("student_residuals" = rstudent(model))
  model_data <- model_data |>
                dplyr::mutate(student_rank = rank(student_residuals),
                       student_percentile = (student_rank - 0.5)/length(student_rank),
                       student_z_score = qnorm(student_percentile),
                       fitted_values = fitted.values(model))
  
  # QQ-plot
  p1 <- ggplot2::ggplot(model_data, ggplot2::aes(x = student_z_score, y=student_residuals)) + 
        ggplot2::geom_abline(intercept = 0, slope = 1, linewidth = 1) +
        ggplot2::geom_point(fill = "red", color = "black", size = 2, alpha = 0.6, pch = 21) +
        ggplot2::theme_classic() +
        ggplot2::ggtitle("Q-Q Plot") + 
        ggplot2::xlab("Theoretical Quantiles") +
        ggplot2::ylab("Studentized Residuals") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14), 
              axis.text.x = ggplot2::element_text(size = 10, color = "black", angle = 0),
              axis.text.y = ggplot2::element_text(size = 10, color = "black"),
              axis.title.y = ggplot2::element_text(size = 12, vjust = 2),
              axis.title.x = ggplot2::element_text(size = 12, vjust = 0.1),
              strip.text = ggplot2::element_text(size=14, color = "black"),
              legend.title.align = 0.5, 
              legend.title= ggplot2::element_text(size=14), 
              legend.text = ggplot2::element_text(size = 12))
  
  # Residual histogram plot
  p2 <- ggplot2::ggplot(model_data, ggplot2::aes(x = student_residuals)) +
        ggplot2::geom_histogram(ggplot2::aes(student_residuals, ggplot2::after_stat(density)), bins = 15, 
                                fill = "light blue", color = "black") +
        ggplot2::stat_function(fun = dnorm, args = list(mean = mean(model_data$student_residuals),
                                               sd = sd(model_data$student_residuals)),
                      ggplot2::aes(color = "red"), linewidth = 1,
                      show.legend = FALSE) +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::theme_classic() +
        ggplot2::ggtitle("Residual Histogram Plot") + 
        ggplot2::ylab("Density") +
        ggplot2::xlab("Studentized Residuals") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14), 
              axis.text.x = ggplot2::element_text(size = 10, color = "black", angle = 0),
              axis.text.y = ggplot2::element_text(size = 10, color = "black"),
              axis.title.y = ggplot2::element_text(size = 12, vjust = 2),
              axis.title.x = ggplot2::element_text(size = 12, vjust = 0.1),
              strip.text = ggplot2::element_text(size=14, color = "black"),
              legend.title.align = 0.5, 
              legend.title= ggplot2::element_text(size=14), 
              legend.text = ggplot2::element_text(size = 12))
  
  # Residuals vs fitted plot
  p3 <- ggplot2::ggplot(model_data, ggplot2::aes(x = fitted_values, y = student_residuals)) +
        ggplot2::geom_point(fill = "purple", alpha = 0.75, pch = 21, size = 2) +
        ggplot2::theme_classic() +
        ggplot2::geom_abline(intercept = 0, slope = 0, linewidth = 0.5) +
        ggplot2::ggtitle("Residuals vs Fitted Values") + 
        ggplot2::ylab("Studentized Residuals") +
        ggplot2::xlab("Fitted Values") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14), 
                  axis.text.x = ggplot2::element_text(size = 10, color = "black", angle = 0),
                  axis.text.y = ggplot2::element_text(size = 10, color = "black"),
                  axis.title.y = ggplot2::element_text(size = 12, vjust = 2),
                  axis.title.x = ggplot2::element_text(size = 12, vjust = 0.1),
                  strip.text = ggplot2::element_text(size=14, color = "black"),
                  legend.title.align = 0.5, 
                  legend.title= ggplot2::element_text(size=14), 
                  legend.text = ggplot2::element_text(size = 12))
  
  # Create data frame of model summary statistics
  results <- broom::glance(model)
  results <- results |>
             dplyr::select(nobs, df.residual, AIC, BIC, r.squared, adj.r.squared) |>
             dplyr::mutate(AIC = round(AIC, 1),
                           BIC = round(BIC, 1),
                           r.squared = round(r.squared, 4),
                           adj.r.squared = round(adj.r.squared, 4)) |>
             dplyr::mutate_if(is.numeric, as.character) |>
             dplyr::rename("Num Obs" = "nobs",
                           "DF Residual" = "df.residual",
                           'R Squared' = "r.squared",
                           "Adj R Squared" = "adj.r.squared") |>
             tidyr::pivot_longer(tidyr::everything(),
                                 names_to = "Statistic", values_to = "Value")
  
  # Create plot of model summary statistics 
  p4 <- ggplot2::ggplot(data = data.frame(x = c(0,1), y = c(0,1)), ggplot2::aes(x = x, y = y)) +
        ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
        ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
        ggplot2::theme_void() +
        ggplot2::annotate(geom = "text", x = 0.35, y = 1.00, label = results[1,1]) + 
        ggplot2::annotate(geom = "text", x = 0.65, y = 1.00, label = results[1,2]) +
        ggplot2::annotate(geom = "text", x = 0.35, y = 0.80, label = results[2,1]) + 
        ggplot2::annotate(geom = "text", x = 0.65, y = 0.80, label = results[2,2]) +
        ggplot2::annotate(geom = "text", x = 0.35, y = 0.60, label = results[3,1]) + 
        ggplot2::annotate(geom = "text", x = 0.65, y = 0.60, label = results[3,2]) + 
        ggplot2::annotate(geom = "text", x = 0.35, y = 0.40, label = results[4,1]) + 
        ggplot2::annotate(geom = "text", x = 0.65, y = 0.40, label = results[4,2]) +
        ggplot2::annotate(geom = "text", x = 0.35, y = 0.20, label = results[5,1]) + 
        ggplot2::annotate(geom = "text", x = 0.65, y = 0.20, label = results[5,2]) +
        ggplot2::annotate(geom = "text", x = 0.35, y = 0.00, label = results[6,1]) + 
        ggplot2::annotate(geom = "text", x = 0.65, y = 0.00, label = results[6,2]) +
        ggplot2::coord_cartesian(clip = "off")
  
  # Create model diagnostics figure
  figure <- patchwork::wrap_plots(p1, p2, p3, p4, nrow = 2) + 
            patchwork::plot_layout(widths = c(1, 1), heights = c(0.5, 0.5)) + 
            patchwork::plot_annotation(title = "Linear Regression Model Diagnostics",
                                       theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))
  
  return(figure)
  
}



