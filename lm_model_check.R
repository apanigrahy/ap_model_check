# This function is used to perform model diagnostics on linear regression models

lm_model_check <- function(model){
  model_data <- data.frame("student_residuals" = rstudent(model))
  model_data <- model_data |>
                dplyr::mutate(student_rank = rank(student_residuals),
                       student_percentile = (student_rank - 0.5)/length(student_rank),
                       student_z_score = qnorm(student_percentile),
                       fitted_values = fitted.values(model))
  
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
  
  p2 <- ggplot2::ggplot(model_data, ggplot2::aes(x = student_residuals)) +
        ggplot2::geom_histogram(ggplot2::aes(student_residuals, ggplot2::after_stat(density)), bins = 15, 
                                fill = "light blue", color = "black") +
        ggplot2::stat_function(fun = dnorm, args = list(mean = mean(model_data$student_residuals),
                                               sd = sd(model_data$student_residuals)),
                      ggplot2::aes(color = "red"), size = 1,
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
}