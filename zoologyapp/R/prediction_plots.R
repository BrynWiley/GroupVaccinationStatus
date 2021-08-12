#This file contains functions to produce the plot displaying the past and predicted
#portion of people with each vaccination status from each Location, Region, and Age group

# library(tidyverse)
# library(lubridate)


#'@description A simple function to combine the Location, Region, and Age arguments in a data frame (requires these are present)
#'into a Category variable
#'@param data the data frame/tibble to perform this operation on
#'@param default_option The observation value that indicates "all" (example, not specifying an age group)
#'@return The data parameter without the Location, Region, and Age arguments, but with a new Category variable that 
#'combines these into a string separated by commas
categorize <- function(data,default_option=NA){
  if(is.na(default_option)){
    return(
      data %>%
        rowwise()%>%
        mutate(Category=paste(c(Location,Region,Age)[!is.na(c(Location,Region,Age))],collapse=", "))%>%
        select(!Location)%>%
        select(!Age)%>%
        select(!Region)
    )
  } else {
    return(
    data %>%
      rowwise()%>%
      mutate(Category=paste(c(Location,Region,Age)[c(Location,Region,Age)!=default_option],collapse=", "))%>%
      select(!Location)%>%
      select(!Age)%>%
      select(!Region)
    )
  }
  
}


#'A function to return the plots for past and predicted vaccination proportions for at least one dose and both doses.
#'Also plots prediction intervals and indicates locations with uncertain data.
#'@description A function that generates plots for past and predicted vaccination proportions
#'@param prediction_data A data frame that contains data on the predicted vaccination rates for the desired locations, regions, and age groups.
#'Requires the variables Date, Portion_1, and Portion_2, and Category (represents Location, Region, and Age group).
#'@param vaccine_data A data frame that contains data on the past vaccination rates for the desired locations, regions, and age groups.
#'Requires the variables Date, Portion_1, and Portion_2, and Category (represents Location, Region, and Age group).
#'@param bad_locations A string vector of locations that have uncertain data
#'@param confidence_intervals A logical singleton indicating if prediction intervals should be plotted
#'@return A list with two values: 
#'Portion_1_plot: A ggplot object showing past and predicted partial vaccination status (at least 1 vaccine dose)
#'Portion_2_plot: A ggplot object showing past and predicted full vaccination status
prediction_plots <- function(prediction_data, vaccine_data, bad_locations, confidence_intervals=TRUE){
  vaccine_data_1_filtered <- vaccine_data %>%
    filter(!is.na(Portion_1)) %>%
    mutate(linetype=if_else(Category %in% bad_locations, "dashed","solid"))
  vaccine_data_2_filtered <- vaccine_data %>%
    filter(!is.na(Portion_2))%>%
    mutate(linetype=if_else(Category %in% bad_locations, "dashed","solid"))

  limited_1 <- vaccine_data_1_filtered %>%
    group_by(Category)%>%
    summarise(Date=Date, Portion_1=Portion_1,n=n())%>%
    ungroup()%>%
    filter(n<10)
  limited_2 <- vaccine_data_1_filtered %>%
    group_by(Category)%>%
    summarise(Date=Date, Portion_2=Portion_2,n=n())%>%
    ungroup()%>%
    filter(n<10)
  
  if(confidence_intervals){
    portion_1_graph <- prediction_data %>%
      ungroup()%>% 
      select(Date, Portion_1, Portion_1_lower, Portion_1_upper,Category)%>%
      ggplot(aes(x=Date,y=Portion_1,ymax=Portion_1_upper,ymin=Portion_1_lower,color=factor(Category),fill=factor(Category)))+
      geom_point()+geom_ribbon(alpha=0.25)+
      geom_line(data=vaccine_data_1_filtered,aes(x=Date,y=Portion_1,color=factor(Category),linetype=linetype),inherit.aes = FALSE,size=1)+
      geom_point(data=limited_1,aes(x=Date,y=Portion_1,color=factor(Category)),inherit.aes = FALSE,shape=4,size=3)+
      scale_linetype_identity()+
      scale_color_viridis_d()+scale_fill_viridis_d()+
      theme_bw()+theme(legend.title = element_blank())+
      xlab("Date")+ylab("Proportion with at least 1 dose")+
      ylim(0,1)
    
    portion_2_graph <- prediction_data %>%
      ungroup()%>% 
      select(Date, Portion_2, Portion_2_lower, Portion_2_upper,Category)%>%
      ggplot(aes(x=Date,y=Portion_2,ymax=Portion_2_upper,ymin=Portion_2_lower,color=factor(Category),fill=factor(Category)))+
      geom_point()+geom_ribbon(alpha=0.25)+
      geom_line(data=vaccine_data_2_filtered,aes(x=Date,y=Portion_2,color=factor(Category),linetype=linetype),inherit.aes = FALSE,size=1)+
      geom_point(data=limited_2,aes(x=Date,y=Portion_2,color=factor(Category)),inherit.aes = FALSE,shape=4,size=3)+
      scale_linetype_identity()+
      scale_color_viridis_d()+scale_fill_viridis_d()+
      theme_bw()+theme(legend.title = element_blank())+
      xlab("Date")+ylab("Proportion with two doses")+
      ylim(0,1)
    
    return(list(
      Portion_1_plot=portion_1_graph,
      Portion_2_plot=portion_2_graph
    ))
  } else {
    portion_1_graph <- prediction_data %>%
      ungroup()%>% 
      select(Date, Portion_1,Category)%>%
      ggplot(aes(x=Date,y=Portion_1,color=factor(Category)))+
      geom_point()+
      geom_line(data=vaccine_data_1_filtered,aes(x=Date,y=Portion_1,color=factor(Category),linetype=linetype),inherit.aes = FALSE,size=1)+
      geom_point(data=limited_1,aes(x=Date,y=Portion_1,color=factor(Category)),inherit.aes = FALSE,shape=4,size=3)+
      scale_linetype_identity()+
      scale_color_viridis_d()+scale_fill_viridis_d()+
      theme_bw()+theme(legend.title = element_blank())+
      xlab("Date")+ylab("Proportion with at least 1 dose")+
      ylim(0,1)
    
    portion_2_graph <-  prediction_data %>%
      ungroup()%>% 
      select(Date, Portion_2,Category)%>%
      ggplot(aes(x=Date,y=Portion_2,color=factor(Category)))+
      geom_point()+
      geom_line(data=vaccine_data_2_filtered,aes(x=Date,y=Portion_2,color=factor(Category),linetype=linetype),inherit.aes = FALSE,size=1)+
      geom_point(data=limited_2,aes(x=Date,y=Portion_2,color=factor(Category)),inherit.aes = FALSE,shape=4,size=3)+
      scale_linetype_identity()+
      scale_color_viridis_d()+scale_fill_viridis_d()+
      theme_bw()+theme(legend.title = element_blank())+
      xlab("Date")+ylab("Proportion with two doses")+
      ylim(0,1)
    
    return(list(
      Portion_1_plot=portion_1_graph,
      Portion_2_plot=portion_2_graph
    ))
  }
  
}
