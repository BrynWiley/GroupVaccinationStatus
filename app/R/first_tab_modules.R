library(shiny)


#'Function for making sure a file uploaded by a user has the required Location and `Number of People` variables. 
#'If it doesn't have these, adds the variables with NA values
#'@description A simple function used to check if the user uploaded dataframe has the minimum required variables, and adds them if missing
#'@param df The user uploaded data frame / tibble
#'@return The uploaded data frame/tibble, but with the Location and `Number of People` variables if missing
check_uploaded_df <- function(df){
  variables <- colnames(df)
  if(!"Location" %in% variables){
    df <- df %>%
      mutate(Location=NA)
  }
  if(!"Number of People" %in% variables){
    df <- df %>%
      mutate(`Number of People`=NA)
  }
  return(df)
}

