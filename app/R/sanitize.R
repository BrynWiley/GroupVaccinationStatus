#This file contains checks and functions for modifying 
#user input on population data so it aligns with existing data

library(tidyverse)
library(lubridate)
library(stringr)


#A simple function to allow equality comparisons with NA values
compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

#checks the df for the required columns. Returns a list
#with two components: a vector of required strings missing,
#and a vector of columns that will not be used
#' A function used to clean the user entered data about the group members so it aligns with the 
#' available data. Locations and Regions that don't exist are removed, and Age groups that don't
#' exist are spread evenly among those that do. If there are excess variables or variables are missing that
#' are required, return these
#' @description A function used to clean user entered data, and return potential problems
#' @param pop_data A data frame, with the user-entered data about the population, should have variables
#' Location, Region, Age, Number of People, and Maximum Portion Vaccinated
#' @param vaccine_data A data frame with the available vaccination rate data. Needs to have columns 
#' Date, Location, Region, Age, Portion_1, and Portion_2
#' @param default_option The value representing "all" (eg, all age groups)
#' @param default_portion The default upper limit on the portion of people vaccinated in the group
#' @return A list with the following components:
#' missing: A vector of missing variables
#' not_used: A vector of excess variables
#' pop_data: The cleaned population data
#' bad_locations: Locations that are not available
#' bad_regions: Regions that are not available
check_df <- function(pop_data,vaccine_data,default_option,default_portion){
  used <- c("Location","Region","Age","Number of People","Maximum Portion Vaccinated")
  required <- c("Location","Number of People")
  
  not_used_variables = setdiff(colnames(pop_data),used)
  missing_variables = setdiff(required,colnames(pop_data))
  missing_locations=character(0)
  missing_regions=character(0)
  if(length(missing_variables)==0){
    results <- sanitize_data(pop_data,vaccine_data,default_option,default_portion)
    pop_data <- results$sanitized_pop_data
    missing_locations=results$missing_locations
    missing_regions=results$missing_regions
  }
  
  to_return <- list(
    missing_variables = missing_variables,
    not_used = not_used_variables,
    pop_data = pop_data,
    bad_locations=missing_locations,
    bad_regions=missing_regions
  )
  
  return(to_return)
}


#' A utility function used to split the entered number of individuals in a age group (defined by a string)
#' into the available age groups (also defined by strings)
#' @description A utility function used to split an age group into the available age groups
#' @param number The number of individuals in the specified age group
#' @param group_string A string representing the age group of the individuals. 
#' @param available_groups A string vector representing the available groups
#' @return A data frame with two variables: 
#' group: an available age group
#' number: the adjusted number of individuals in the specified age group
adjust_age_group <- function(number,group_string,available_groups){
  group_string <- as.character(group_string)
  available_groups <- as.character(available_groups)
  #First we check if the desired group is NA and NA exists in available groups
  if(is.na(group_string) & any(is.na(available_groups))){
    return(tibble(
      number=number,
      group=NA
    ))
  }
  #These are "bad" conditions
  else if(is.na(group_string) | length(group_string)<1 | length(available_groups)<1){
    return(tibble(
      number=numeric(),
      group=character()
    ))
  }
  
  #At this point, group_string is not NA, and may contain an age group
  #Extract possible ages
  
  ages=str_extract_all(group_string,"[[:digit:]]+|\\+")[[1]]
  ages=str_replace(ages,"\\+","Inf")
  ages=as.numeric(ages)

  #No infinitely aged people
  if(all(ages==Inf) & length(ages)>1){
    return(tibble(
    number=numeric(),
    group=character()
  ))}
  #No possible ages can be extracted, or there is no possible match (all NA available groups)
  if(length(ages)<1 | all(is.na(available_groups))){
    return(tibble(
      number=number,
      group=NA
    ))
  }
  #boundaries for age group
  min_age=min(ages)
  max_age=max(ages)
  
  #boundaries for available age groups
  available_groups <- available_groups[!is.na(available_groups)]
  breaks_min=sapply(available_groups,function(X){
    nums=str_extract_all(X,"[[:digit:]]+|\\+")[[1]]
    nums=str_replace(nums,"\\+","Inf")
    if(length(nums)<1){
      return(NA)
    }else{
      return(min(as.numeric(nums)))
    }
  })
  
  breaks_max=sapply(available_groups,function(X){
    nums=str_extract_all(X,"[[:digit:]]+|\\+")[[1]]
    nums=str_replace(nums,"\\+","Inf")
    if(length(nums)<1){
      return(NA)
    }else{
      return(max(as.numeric(nums)))
    }
  })
  #Condition for not being able to extract any numbers is NA
  if(any(is.na(breaks_max))|any(is.na(breaks_min))){
    return(tibble(
      number=numeric(),
      group=character()
    ))
  }
  
  #Condition for inf maximum age
  if(max_age == Inf){
    to_return <- tibble(
      number=numeric(),
      group=character()
    )
    
    num_groups=sum(min_age <=breaks_max)
    indexes= which(min_age <=breaks_max)
    for(i in indexes){
      to_return <- add_row(
        to_return,
        number=number/num_groups,
        group=available_groups[i]
      )
    }
    return(to_return)
  }
  
  to_return <- tibble(
    number=numeric(),
    group=character()
  )
  
  for(i in 1:length(breaks_min)){
    #available group within a desired group
    if(min_age>=breaks_min[i] & max_age<=breaks_max[i]){
      
      return(tibble(
        group=available_groups[i],
        number=number)
      )
    } else
      
      #available inside a desired group
      if(min_age<=breaks_min[i] & max_age>=breaks_max[i]){
        to_return <- add_row(to_return,
                             group=available_groups[i],
                             number=number*(breaks_max[i]-breaks_min[i]+1)/(max_age-min_age+1))
      } else
        
        #cases for partial overlap
        if(min_age<=breaks_max[i] & max_age>breaks_max[i]){
          
          to_return <- add_row(to_return,
                               group=available_groups[i],
                               number=number*(breaks_max[i]-min_age+1)/(max_age-min_age+1))
          
        } else
          if(max_age>=breaks_min[i] & min_age<breaks_min[i]){
            
            to_return <- add_row(to_return,
                                 group=available_groups[i],
                                 number=number*(max_age-breaks_min[i]+1)/(max_age-min_age+1))
            
          }
  }
  return(to_return)
}


#Returns a list with two components:
#A data frame of entered entrant data sanitized to work properly with the rest of the program,
#and a vector of string names of locations that don't exist
#Locations that don't exist are removed,
#Regions that don't exist are assumed to be the entirety of the location (NA),
#and age groups are split among existing age groups, or converted
#This assumes that the vaccine_data has non-overlapping age groups that cover all possible age groups!
#Required variables are Location and Number of People

#pop_data: the user entered data
#vaccine_data: the available vaccination status data. Requires columns:
# Location, Region, Age
#default_option: the name for the default (example, all regions in a Location), a singleton value
#default_portion: the default maximum portion vaccinated
#' A utility function used to clean user entered data about the group of interest. Adds missing required variables,
#' removes unavailable location and regions, and adjusts age groups to available data.
#' @description A utility function used to clean user entered population data
#' @param pop_data A data frame, with the user-entered data about the population, should have variables
#' Location, Region, Age, Number of People, and Maximum Portion Vaccinated
#' @param vaccine_data A data frame with the available vaccination rate data. Needs to have columns 
#' Date, Location, Region, Age, Portion_1, and Portion_2
#' @param default_option The value representing "all" (eg, all age groups)
#' @param default_portion The default upper limit on the portion of people vaccinated in the group
#' @return A list with three components:
#' sanitized_pop_data: The cleaned user entered population data
#' missing_locations: The locations entered by the user that are not in the available dataset
#' missing_regions: The regions entered by the user that are not in the available dataset
sanitize_data <- function(pop_data, vaccine_data, default_option, default_portion){
  #Add unused variables
  if(!"Region" %in% colnames(pop_data)){
    pop_data <- mutate(pop_data,Region=NA)
  }
  if(!"Age" %in% colnames(pop_data)){
    pop_data <- mutate(pop_data,Age=NA)
  }
  if(!"Number of People" %in% colnames(pop_data)){
    pop_data <- mutate(pop_data,`Number of People`=NA)
  }
  if(!"Maximum Portion Vaccinated" %in% colnames(pop_data)){
    pop_data <- mutate(pop_data,`Maximum Portion Vaccinated`=NA)
  }
  
  #replace the default option with NA in Region and Age
  #This is required for sanitization
  pop_data$Region[pop_data$Region==default_option]<-NA
  pop_data$Age[pop_data$Age==default_option]<-NA
  
  #Make sure Number of People and Maximum Portion Vaccinated are numeric (if not, replace with 0 or the default value respectively)
  pop_data$`Number of People` <- as.numeric(pop_data$`Number of People`)
  pop_data$`Number of People`[is.na(pop_data$`Number of People`)]<-0
  
  pop_data$`Maximum Portion Vaccinated` <- as.numeric(pop_data$`Maximum Portion Vaccinated`)
  pop_data$`Maximum Portion Vaccinated`[is.na(pop_data$`Maximum Portion Vaccinated`)] <- default_portion
  
  #maximum portions that are above 1 are first attempted to convert to percentages, then converted to default portion if
  #still too big
  pop_data$`Maximum Portion Vaccinated`[pop_data$`Maximum Portion Vaccinated`>1.0] <- pop_data$`Maximum Portion Vaccinated`[pop_data$`Maximum Portion Vaccinated`>1.0]/100
  pop_data$`Maximum Portion Vaccinated`[pop_data$`Maximum Portion Vaccinated`>1.0] <- default_portion
  
  #For each valid location, check or clean up other variables
  sanitized_pop_data <- pop_data %>%
    filter(Location %in% vaccine_data$Location & !is.na(Location) & !is.na(`Number of People`) & !is.na(`Maximum Portion Vaccinated`)) %>%
    group_by(Location)%>%
    group_modify(function(data,key){
      empty_tibble <- tibble(
        Region=character(),
        Age=character(),
        `Number of People`=numeric(),
        `Maximum Portion Vaccinated`=numeric()
      )
      
      #No valid entries, return an empty tibble
      if(length(key$Location)<1 |nrow(data)<1){
        return(empty_tibble)
      }
      
      #regions that are invalid are converted to NA
      actual_regions <- unique(vaccine_data$Region[vaccine_data$Location==key$Location])
      data$Region[!data$Region %in% actual_regions]<- NA
      
      #age groups are recalculated. If an age group does not exist, reformatted to NA
      data <- data %>%
        group_by(Region) %>%
        group_modify(function(data_,key_){
          available_age_groups <- unique(vaccine_data$Age[vaccine_data$Location==key$Location & compareNA(vaccine_data$Region,key_$Region)])
          
          
          #Some age groups available
          to_return <- tibble(
            Age = character(),
            `Number of People`=numeric(),
            `Maximum Portion Vaccinated`=numeric()
          )
          for(i in 1:nrow(data_)){
            to_return <- rbind(to_return, adjust_age_group(data_$`Number of People`[i],data_$Age[i],available_age_groups)%>%
                                 rename(Age=group,`Number of People`=number)%>%
                                 mutate(`Maximum Portion Vaccinated`=data_$`Maximum Portion Vaccinated`[i]))
          }
          return(to_return)
        })
      return(data)
    })%>%
    mutate(Location=as.character(Location),Region=as.character(Region),
           `Number of People`=as.numeric(`Number of People`),Age=as.character(Age),`Maximum Portion Vaccinated`=as.numeric(`Maximum Portion Vaccinated`))%>%
    select(Location,Region,Age,`Number of People`,`Maximum Portion Vaccinated`) %>%
    group_by(Location,Region,Age)%>%
    summarise(`Number of People`=sum(`Number of People`),
              `Maximum Portion Vaccinated`=max(`Maximum Portion Vaccinated`))
  
  #Change NA values for Region and Age to the default option
  sanitized_pop_data$Region[is.na(sanitized_pop_data$Region)] <- default_option
  sanitized_pop_data$Age[is.na(sanitized_pop_data$Age)] <- default_option
  
  missing_locations <- setdiff(pop_data$Location,sanitized_pop_data$Location)
  missing_regions <- setdiff(pop_data$Region,sanitized_pop_data$Region)
  return(list(
    sanitized_pop_data=sanitized_pop_data,
    missing_locations=missing_locations[!is.na(missing_locations)],
    missing_regions=missing_regions[!is.na(missing_regions)]
  ))
}
