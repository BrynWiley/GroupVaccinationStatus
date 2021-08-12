# library(tidyverse)

#' Evaluate Dataset, used to clean a dataset, and return warnings/errors, along with if the
#' dataset should be used
#' @description A simple function to evaluate a specified data set, and return its cleaned version
#' @param pop_data the prospective population data entered by the user
#' @param vaccine_data the available vaccine data
#' @param default_option the default option for regions and age groups(ex: "All" or NA)
#' @param default_portion the default portion for the maximum portion vaccinated
#' @return A list with three components: 
#' message:A warning or error message as a string
#' good: A logical value indicating if the dataset should be updated with the cleaned value
#' sanitized_data: the sanitized dataset (if possible)
evaluate_dataset <- function(pop_data, vaccine_data, default_option, default_portion){
  results <- check_df(pop_data, vaccine_data, default_option, default_portion)
  if(length(results$missing_variables)>0){
    return(list(
      message=paste("Error: missing required variables: ",paste(results$missing_variables,collapse=","),
                    ". Refresh the app to restore defaults.",sep=" "),
      sanitized_data=NULL,
      good=FALSE
    ))
  }
  
  not_used_message<- NULL
  locations_message <- NULL
  regions_message <- NULL
  
  if(length(results$not_used)>0){
    not_used_message <- paste("These variables are not used and were removed:",paste(results$not_used,collapse=","))
  }
  if(length(results$bad_locations)>0){
    locations_message <- paste("These locations are not supported and were removed:",paste(results$bad_locations,collapse=","))
  }
  if(length(results$bad_regions)>0){
    regions_message <- paste("These regions are not supported and were removed:",paste(results$bad_regions,collapse=","))
  }
  
  message <- c(not_used_message,locations_message,regions_message)
  warning_message <- paste(message,collapse="<br/>")
  
  return(list(
    message=warning_message,
    sanitized_data=results$pop_data,
    good=TRUE
  ))
}

