
#' @title  Create the summaries for daily, weekly and monthly modis reflectance
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description The function will take the MODIS reflectance datasets and
#' create the indices summaries. 
#' 
#' @details Dataset should have the indices values from MODIS reflectance dataset
#' 
#' @param reflectance_dataset Dataset with the indices.
#' @param summary_date If the dataset has to be summarized by daily, weekly or monthly.
#' @param resolution If it is the 250 or 500 m resolution dataset.
#' @param sensor If the dataset corresponds to MODIS or Sentinel
#' 
#' @example 
#' \dontrun{
#' summarise_indices(reflectance_dataset = reflectance_500, 
#'                   summary_date = "monthly",
#'                   resolution = 500)
#'}
#'
summarise_indices <- function(reflectance_dataset, summary_date, resolution, sensor) {
  
  if (sensor == "MODIS") {
    
    if (resolution == 500) {
      
      if (summary_date == "daily") {
        
        indices_summarized <- reflectance_dataset %>% 
          group_by(date) %>% 
          summarise(
            ndvi_mean = mean(ndvi, na.rm = TRUE),
            nirv_mean = mean(nirv, na.rm = TRUE),
            evi_mean =  mean(evi, na.rm = TRUE),
            kndvi_mean =  mean(kndvi, na.rm = TRUE),
            cci_mean =  mean(cci, na.rm = TRUE),
            ndvi_median = median(ndvi, na.rm = TRUE),
            nirv_median = median(nirv, na.rm = TRUE),
            evi_median =  median(evi, na.rm = TRUE),
            kndvi_median =  median(kndvi, na.rm = TRUE),
            cci_median =  median(cci, na.rm = TRUE),
            sur_refl_b01_mean = mean(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_mean = mean(sur_refl_b02, na.rm = TRUE),
            sur_refl_b03_mean = mean(sur_refl_b03, na.rm = TRUE),
            sur_refl_b04_mean = mean(sur_refl_b04, na.rm = TRUE),
            sur_refl_b05_mean = mean(sur_refl_b05, na.rm = TRUE),
            sur_refl_b06_mean = mean(sur_refl_b06, na.rm = TRUE),
            sur_refl_b07_mean = mean(sur_refl_b07, na.rm = TRUE),
            sur_refl_b01_median = median(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_median = median(sur_refl_b02, na.rm = TRUE),
            sur_refl_b03_median = median(sur_refl_b03, na.rm = TRUE),
            sur_refl_b04_median = median(sur_refl_b04, na.rm = TRUE),
            sur_refl_b05_median = median(sur_refl_b05, na.rm = TRUE),
            sur_refl_b06_median = median(sur_refl_b06, na.rm = TRUE),
            sur_refl_b07_median = median(sur_refl_b07, na.rm = TRUE),
            total_obs = n()
          ) %>% 
          ungroup()
        
      } else if (summary_date == "weekly") {
        indices_summarized <- reflectance_dataset %>% 
          mutate(week = week(date),
                 year = year(date)) %>% 
          group_by(year, week) %>% 
          summarise(
            ndvi_mean = mean(ndvi, na.rm = TRUE),
            nirv_mean = mean(nirv, na.rm = TRUE),
            evi_mean =  mean(evi, na.rm = TRUE),
            kndvi_mean =  mean(kndvi, na.rm = TRUE),
            cci_mean =  mean(cci, na.rm = TRUE),
            ndvi_median = median(ndvi, na.rm = TRUE),
            nirv_median = median(nirv, na.rm = TRUE),
            evi_median =  median(evi, na.rm = TRUE),
            kndvi_median =  median(kndvi, na.rm = TRUE),
            cci_median =  median(cci, na.rm = TRUE),
            sur_refl_b01_mean = mean(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_mean = mean(sur_refl_b02, na.rm = TRUE),
            sur_refl_b03_mean = mean(sur_refl_b03, na.rm = TRUE),
            sur_refl_b04_mean = mean(sur_refl_b04, na.rm = TRUE),
            sur_refl_b05_mean = mean(sur_refl_b05, na.rm = TRUE),
            sur_refl_b06_mean = mean(sur_refl_b06, na.rm = TRUE),
            sur_refl_b07_mean = mean(sur_refl_b07, na.rm = TRUE),
            sur_refl_b01_median = median(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_median = median(sur_refl_b02, na.rm = TRUE),
            sur_refl_b03_median = median(sur_refl_b03, na.rm = TRUE),
            sur_refl_b04_median = median(sur_refl_b04, na.rm = TRUE),
            sur_refl_b05_median = median(sur_refl_b05, na.rm = TRUE),
            sur_refl_b06_median = median(sur_refl_b06, na.rm = TRUE),
            sur_refl_b07_median = median(sur_refl_b07, na.rm = TRUE),
            total_obs = n()
          ) %>% 
          ungroup()
      } else if (summary_date == "monthly") {
        indices_summarized <- reflectance_dataset %>% 
          mutate(date = zoo::as.yearmon(date)) %>% 
          group_by(date) %>% 
          summarise(
            ndvi_mean = mean(ndvi, na.rm = TRUE),
            nirv_mean = mean(nirv, na.rm = TRUE),
            evi_mean =  mean(evi, na.rm = TRUE),
            kndvi_mean =  mean(kndvi, na.rm = TRUE),
            cci_mean =  mean(cci, na.rm = TRUE),
            ndvi_median = median(ndvi, na.rm = TRUE),
            nirv_median = median(nirv, na.rm = TRUE),
            evi_median =  median(evi, na.rm = TRUE),
            kndvi_median =  median(kndvi, na.rm = TRUE),
            cci_median =  median(cci, na.rm = TRUE),
            sur_refl_b01_mean = mean(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_mean = mean(sur_refl_b02, na.rm = TRUE),
            sur_refl_b03_mean = mean(sur_refl_b03, na.rm = TRUE),
            sur_refl_b04_mean = mean(sur_refl_b04, na.rm = TRUE),
            sur_refl_b05_mean = mean(sur_refl_b05, na.rm = TRUE),
            sur_refl_b06_mean = mean(sur_refl_b06, na.rm = TRUE),
            sur_refl_b07_mean = mean(sur_refl_b07, na.rm = TRUE),
            sur_refl_b01_median = median(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_median = median(sur_refl_b02, na.rm = TRUE),
            sur_refl_b03_median = median(sur_refl_b03, na.rm = TRUE),
            sur_refl_b04_median = median(sur_refl_b04, na.rm = TRUE),
            sur_refl_b05_median = median(sur_refl_b05, na.rm = TRUE),
            sur_refl_b06_median = median(sur_refl_b06, na.rm = TRUE),
            sur_refl_b07_median = median(sur_refl_b07, na.rm = TRUE),
            total_obs = n()
          ) %>% 
          ungroup()
      }
    } else if (resolution == 250) {
      if (summary_date == "daily") {
        
        indices_summarized <- reflectance_dataset %>% 
          group_by(date) %>% 
          summarise(
            ndvi_mean = mean(ndvi, na.rm = TRUE),
            nirv_mean = mean(nirv, na.rm = TRUE),
            kndvi_mean =  mean(kndvi, na.rm = TRUE),
            ndvi_median = median(ndvi, na.rm = TRUE),
            nirv_median = median(nirv, na.rm = TRUE),
            kndvi_median =  median(kndvi, na.rm = TRUE),
            sur_refl_b01_mean = mean(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_mean = mean(sur_refl_b02, na.rm = TRUE),
            sur_refl_b01_median = median(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_median = median(sur_refl_b02, na.rm = TRUE),
            total_obs = n()
          ) %>% 
          ungroup()
        
      } else if (summary_date == "weekly") {
        indices_summarized <- reflectance_dataset %>% 
          mutate(week = week(date),
                 year = year(date)) %>% 
          group_by(year, week) %>% 
          summarise(
            ndvi_mean = mean(ndvi, na.rm = TRUE),
            nirv_mean = mean(nirv, na.rm = TRUE),
            kndvi_mean =  mean(kndvi, na.rm = TRUE),
            ndvi_median = median(ndvi, na.rm = TRUE),
            nirv_median = median(nirv, na.rm = TRUE),
            kndvi_median =  median(kndvi, na.rm = TRUE),
            sur_refl_b01_mean = mean(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_mean = mean(sur_refl_b02, na.rm = TRUE),
            sur_refl_b01_median = median(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_median = median(sur_refl_b02, na.rm = TRUE),
            total_obs = n()
          ) %>% 
          ungroup()
      } else if (summary_date == "monthly") {
        indices_summarized <- reflectance_dataset %>% 
          mutate(date = zoo::as.yearmon(date)) %>% 
          group_by(date) %>% 
          summarise(
            ndvi_mean = mean(ndvi, na.rm = TRUE),
            nirv_mean = mean(nirv, na.rm = TRUE),
            kndvi_mean =  mean(kndvi, na.rm = TRUE),
            ndvi_median = median(ndvi, na.rm = TRUE),
            nirv_median = median(nirv, na.rm = TRUE),
            kndvi_median =  median(kndvi, na.rm = TRUE),
            sur_refl_b01_mean = mean(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_mean = mean(sur_refl_b02, na.rm = TRUE),
            sur_refl_b01_median = median(sur_refl_b01, na.rm = TRUE),
            sur_refl_b02_median = median(sur_refl_b02, na.rm = TRUE),
            total_obs = n()
          ) %>% 
          ungroup()
      }
    }
  } else if (sensor == "Sentinel") {
    if (summary_date == "daily") {
      
      indices_summarized <- reflectance_dataset %>% 
        group_by(date) %>% 
        summarise(
          ndvi_mean = mean(ndvi, na.rm = TRUE),
          nirv_mean = mean(nirv, na.rm = TRUE),
          evi_mean =  mean(evi, na.rm = TRUE),
          ndvi_median = median(ndvi, na.rm = TRUE),
          nirv_median = median(nirv, na.rm = TRUE),
          evi_median =  median(evi, na.rm = TRUE),
          total_obs = n()
        ) %>% 
        ungroup()
      
    } else if (summary_date == "weekly") {
      indices_summarized <- reflectance_dataset %>% 
        mutate(week = week(date),
               year = year(date)) %>% 
        group_by(year, week) %>% 
        summarise(
          ndvi_mean = mean(ndvi, na.rm = TRUE),
          nirv_mean = mean(nirv, na.rm = TRUE),
          evi_mean =  mean(evi, na.rm = TRUE),
          ndvi_median = median(ndvi, na.rm = TRUE),
          nirv_median = median(nirv, na.rm = TRUE),
          evi_median =  median(evi, na.rm = TRUE),
          total_obs = n()
        ) %>% 
        ungroup()
      
    } else if (summary_date == "monthly") {
      indices_summarized <- reflectance_dataset %>% 
        mutate(date = zoo::as.yearmon(date)) %>% 
        group_by(date) %>% 
        summarise(
          ndvi_mean = mean(ndvi, na.rm = TRUE),
          nirv_mean = mean(nirv, na.rm = TRUE),
          evi_mean =  mean(evi, na.rm = TRUE),
          ndvi_median = median(ndvi, na.rm = TRUE),
          nirv_median = median(nirv, na.rm = TRUE),
          evi_median =  median(evi, na.rm = TRUE),
          total_obs = n()
        ) %>% 
        ungroup()
    }
  }
}
