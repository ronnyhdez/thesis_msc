#' @import dplyr
NULL

#' @title Import oneflux data
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description Reads the complete oneflux datasets for Borden, Bartlett and
#' Michigan on a daily, weekly or monthly timeframe.
#' 
#' @details This is designed to parsed, clean variable names, write the NAs
#' for the dataframes that we need to use to plot the GPP trends or other
#' variables that comes directly from the complete oneflux datasets.
#' 
#' @param file Path to the file to be read.
#' @param timeframe Indicate if the file have daily, weekly or monthly values.
#'
#' @example 
#' \dontrun{
#' read_flux_file(file = file, timeframe = "monthly")
#'}
#'
read_flux_file <- function(file, timeframe) {
  
  if (timeframe == "daily") {
    
    one_flux_daily <- readr::read_csv(file) %>% 
      janitor::clean_names() %>% 
      # All NA values are written as -9999
      mutate_all(~na_if(., -9999)) %>% 
      mutate(date = lubridate::ymd(timestamp)) %>% 
      select(-timestamp) %>% 
      filter(lubridate::year(date) >= 2015)
    
  } else if (timeframe == "weekly") {
    
    one_flux_weekly <- readr::read_csv(file) %>% 
      janitor::clean_names() %>% 
      # All NA values are written as -9999
      mutate_all(~na_if(., -9999)) %>% 
      mutate(date_start = lubridate::ymd(timestamp_start),
             date_end = lubridate::ymd(timestamp_end)) %>% 
      select(-timestamp_start, -timestamp_end) %>%
      filter(lubridate::year(date_start) >= 2015)
    
  } else if (timeframe == "monthly") {
    one_flux_monthly <- readr::read_csv(file) %>% 
      janitor::clean_names() %>% 
      # All NA values are written as -9999
      mutate_all(~na_if(., -9999)) %>% 
      mutate(day = "01") %>% 
      tidyr::unite(date, c("timestamp", "day"), sep = "") %>% 
      mutate(date = lubridate::ymd(date)) %>% 
      mutate(date = zoo::as.yearmon(date)) %>% 
      filter(lubridate::year(date) >= 2015)
  }
  
}

