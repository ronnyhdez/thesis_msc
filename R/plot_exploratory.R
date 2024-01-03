#' @import dplyr
#' @import ggplot2
#' @import tidyquant
NULL

#' @title Plot exploratory trends plot
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description To plot a default exploratory plot for trends using the x axis
#' for dates and the y axis for the variable of interest.
#' 
#' @param data The data we want to plot.
#' @param var The variable to be plotted in the y axis
#' @param ma A boolean to plot trends with line specified by function SMA from
#' the tidyquant package
#' 
#' @example 
#' \dontrun{
#' plot_trend(reflectance_250, ndvi, ma = TRUE) + labs(x = "hola")
#'}
plot_trend <- function(data, var, ma = FALSE) {
  if (ma == TRUE) {
    data %>% 
      ggplot(aes(x = date, y = {{var}})) +
      geom_point(size = 1.5, alpha = 0.6, color = "#FF5500") +
      geom_ma(ma_fun = SMA, n = 7, color = "black", linetype = 1,
              size = 1, alpha = 0.6) +
      theme_light() +
      scale_x_date(date_labels = "%b%Y", breaks = "months") +
      theme(axis.text.x = element_text(angle = 90, h = 1))
  } else {
    data %>% 
      ggplot(aes(x = date, y = {{var}})) +
      geom_point(size = 1.5, alpha = 0.6, color = "#FF5500") +
      theme_light() +
      scale_x_date(date_labels = "%b%Y", breaks = "months") +
      theme(axis.text.x = element_text(angle = 90, h = 1)) 
  }
}

#' @title Plot indices gpp relation residuals
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This will plot the residuals from the relation of the indices
#' with GPP for the selected site.
#' 
#' @details To obtain the data needed to build this plot, I need to use the
#' function:
#'  `obtain_residuals_dataset(model_ndvi_monthly_bartlett, bartlett_monthly_500)`
#' 
#' @param data The data we want to plot.
#' 
#' @example 
#' \dontrun{
#' plot_residuals(bartlett_ndvi_500_residuals)
#'}
plot_residuals <- function(data) {
  data %>%
    mutate(residuals = round(residuals, digits = 3)) %>%
    mutate(date = as.Date(date)) %>% 
    # mutate(month = lubridate::month(month, label = TRUE)) %>% select(month, date) %>% View()
    ggplot(aes(x = date, y = residuals, label = residuals)) +
    geom_hline(yintercept = 0, color = "#4E5C68",
               linewidth = 0.7, linetype = "dashed") +
    scale_x_date(date_labels = "%b-%Y",  date_breaks = "2 month") +
    # geom_point()
    geom_segment(aes(y = 0,
                     x = date,
                     yend = residuals,
                     xend = date),
                 color = ifelse(
                   data$residuals > 2.5 |
                     data$residuals < -2.5, 
                   "#FF5500", "#5A86A5"),
                   size = ifelse(
                     data$residuals > 2.5 |
                       data$residuals < -2.5, 
                     1.6, 1.1
                   )) +
    geom_point( 
      color = ifelse(
        data$residuals > 2.5 |
          data$residuals < -2.5, 
        "#FF5500", "#5A86A5"), 
        size = ifelse(
          data$residuals > 2.5 |
            data$residuals < -2.5, 
          14, 10
        ))  +
    geom_text(color = "white", size = 2) +
    labs(y = "Residuals GPP ~ MODIS_NDVI",
         x = "Date") +
    scale_y_continuous(breaks = seq(-6, 6, by = 1)) +
    theme_light(base_size = 12) +
    # theme(axis.text.x = element_text(angle = 90, h = 1)) +
    coord_flip()
}

#' @title Plot indices gpp models residuals distributions
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This will plot the residuals distributions from the relation of
#'  the indices with GPP for the selected site.
#'  
#' @param data The data we want to plot.
#' @param site Site category we want to plot.
#' 
#' @example 
#' \dontrun{
#' plot_residuals(bartlett_ndvi_500_residuals)
#'}
create_residuals_distribution_plot <- function(data, site) {
  data %>% 
    filter(site == {{site}}) %>% 
    ggplot(aes(x = residuals, y = index, fill = index)) +
    geom_density_ridges() +
    geom_vline(xintercept = 0, colour = "#FF5500", 
               linewidth = 0.7, linetype = "dashed") +
    scale_fill_viridis_d() +
    # scale_x_continuous(limits = c(-30, 20), n.breaks = 10) +
    labs(title = {{site}}) +
    theme_ridges() + 
    theme(legend.position = "none")
}


#' @title Plot random forest predicted values
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This will plot the predicted values from the fitted objects
#' created with the tidymodels workflow.
#' 
#' @param fitted_object Result object from `last_fit()` function.
#' @param metrics_obj Result object from `collect_metrics()` function.
#' @param rmse_x_pos x axis coordinate for text label.
#' @param rsq_x_pos x axis coordinate for text label.
#' @param rmse_y_pos y axis coordinate for text label.
#' @param rsq_y_pos y axis coordinate for text label.
#' 
#' @example 
#' \dontrun{
#' plot_predictions(daily_500_fit, metrics, 3, 3, 23, 22)
#'}
plot_predictions_rf <- function(fitted_object, metrics_obj, 
                             rmse_x_pos, rsq_x_pos,
                             rmse_y_pos, rsq_y_pos) {
  broom::augment(fitted_object) %>% 
    ggplot(aes(x = .pred, y = gpp_dt_vut_ref, color = site)) +
    geom_text(aes(x = rmse_x_pos, y = rmse_y_pos,
                  label = paste("RMSE:", round(metrics_obj[1, 3], 2))),
              stat = "unique",
              color = "black",
              size = 3.5) +
    geom_text(aes(x = rsq_x_pos, y = rsq_y_pos,
                  label = paste("R2:", round(metrics_obj[2, 3], 2))),
              stat = "unique",
              color = "black",
              size = 3.5) +
    scale_color_viridis_d() +
    geom_point(alpha = 0.8, size = 3, aes(shape = site)) +
    geom_abline(lty = 1,  color = "#E20D6A", linewidth = 2) +
    scale_x_continuous(breaks = seq(0, 38, by = 2)) +
    scale_y_continuous(breaks = seq(0, 38, by = 2)) +
    labs(x = expression(Predicted~GPP~(gC~m^{"-2"}~d^-1)), 
         y = expression(Observed~GPP~(gC~m^{"-2"}~d^-1))) +
    coord_fixed() +
    theme_classic(base_size = 10)
}

#' @title Plot autoML predicted values
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This will plot the predicted values from the fitted objects
#' created with the tidymodels workflow.
#' 
#' @param fitted_object Result object from `readRDS("models/predictions_automl.rds")`
#' @param rmse Object saved from the autml model
#' @param rsq Object saved from the autml model
#' @param rmse_x_pos x axis coordinate for text label.
#' @param rsq_x_pos x axis coordinate for text label.
#' @param rmse_y_pos y axis coordinate for text label.
#' @param rsq_y_pos y axis coordinate for text label.
#' 
#' @example 
#' \dontrun{
#' plot_predictions_automl(predictions_automl, rmse, rsq, 2, 2, 15, 16)
#'}
plot_predictions_automl <- function(fitted_object, rmse, rsq, 
                                    rmse_x_pos, rsq_x_pos,
                                    rmse_y_pos, rsq_y_pos) {
  fitted_object %>% 
    ggplot(aes(x = predict, y = gpp_dt_vut_ref, color = site)) +
    geom_text(aes(x = rmse_x_pos, y = rmse_y_pos,
                  label = paste("rmse:", round(rmse, 2))),
              stat = "unique",
              color = "black") +
    geom_text(aes(x = rsq_x_pos, y = rsq_y_pos,
                  label = paste("rsq:", round(rsq, 2))),
              stat = "unique",
              color = "black") +
    scale_color_viridis_d() +
    geom_point(alpha = 0.8, size = 4, aes(shape = site)) +
    geom_abline(lty = 1,  color = "#E20D6A", linewidth = 2) +
    scale_x_continuous(breaks = seq(0, 38, by = 2)) +
    scale_y_continuous(breaks = seq(0, 38, by = 2)) +
    labs(x = expression(Predicted-GPP~(gC~m^{"-2"}~d^-1)), 
         y = expression(Observed-GPP~(gC~m^{"-2"}~d^-1))) +
    coord_fixed() +
    theme_classic(base_size = 12) 
}
