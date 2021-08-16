cat("Release number 1.0\n")
# library(shiny)
# library(tidyverse)
# library(rhandsontable)
# library(lubridate)
library(tibble)
library(ggplot2)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(forcats)
#library(magrittr)
library(shiny)
library(lubridate)
library(rhandsontable)
library(investr)



results_data <- read_csv("results.csv")
total_data <- read_csv("totals.csv")
default_option <- NA
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

ui <- fluidPage(
  dataTableOutput("part1"),
  dataTableOutput("part2"),
  dataTableOutput("part3"),
  dataTableOutput("part4"),
  plotOutput("graph1"),
  plotOutput("graph2")
)



server <- function(input, output,session) {
  output$part1 <- renderDataTable({
    results_data %>%
      ungroup()%>%
      categorize(.,default_option)
  })
  output$part2 <- renderDataTable({
    results_data %>%
      ungroup()%>%
      categorize(.,default_option)%>%
      # tidyr::pivot_longer(
      #   cols=c(Proportion_fully,Proportion_partially,Proportion_unvacc)
      # )%>% 
      gather(
        "name",
        "value",
        Number_fully,Number_partially,Number_unvacc
      )
  })
  output$part3 <- renderDataTable({
    results_data %>%
      ungroup()%>%
      categorize(.,default_option)%>%
      # tidyr::pivot_longer(
      #   cols=c(Proportion_fully,Proportion_partially,Proportion_unvacc)
      # )%>% 
      gather(
        "name",
        "value",
        Number_fully,Number_partially,Number_unvacc
      )%>%
      inner_join(tibble(
        name=c("Number_fully","Number_partially","Number_unvacc"),
        group=c("Fully Vaccinated","Partially Vaccinated","Unvaccinated")
      ))
  })
  output$part4 <- renderDataTable({
    results_data %>%
      ungroup()%>%
      categorize(.,default_option)%>%
      # tidyr::pivot_longer(
      #   cols=c(Proportion_fully,Proportion_partially,Proportion_unvacc)
      # )%>% 
      gather(
        "name",
        "value",
        Number_fully,Number_partially,Number_unvacc
      )%>%
      inner_join(tibble(
        name=c("Number_fully","Number_partially","Number_unvacc"),
        group=c("Fully Vaccinated","Partially Vaccinated","Unvaccinated")
      ))%>%
      mutate(group=factor(group,levels=c("Unvaccinated","Partially Vaccinated","Fully Vaccinated")))
  })
  output$graph1 <- renderPlot({
    results_data %>%
      ungroup()%>%
      categorize(.,default_option)%>%
      # tidyr::pivot_longer(
      #   cols=c(Proportion_fully,Proportion_partially,Proportion_unvacc)
      # )%>% 
      gather(
        "name",
        "value",
        Number_fully,Number_partially,Number_unvacc
      )%>%
      inner_join(tibble(
        name=c("Number_fully","Number_partially","Number_unvacc"),
        group=c("Fully Vaccinated","Partially Vaccinated","Unvaccinated")
      ))%>%
      mutate(group=factor(group,levels=c("Unvaccinated","Partially Vaccinated","Fully Vaccinated")))%>%
      ggplot(aes(x=group,y=value,fill=factor(Category)))+geom_bar(position="stack",stat="identity")+
      xlab("")+ylab("Number of People")
  })
  output$graph2 <- renderPlot({
    ymax <- max(total_data$`Number Upper Bound`*1.1)
    results_data %>%
      ungroup()%>%
      categorize(.,default_option)%>%
      # tidyr::pivot_longer(
      #   cols=c(Proportion_fully,Proportion_partially,Proportion_unvacc)
      # )%>% 
      gather(
        "name",
        "value",
        Number_fully,Number_partially,Number_unvacc
      )%>%
      inner_join(tibble(
        name=c("Number_fully","Number_partially","Number_unvacc"),
        group=c("Fully Vaccinated","Partially Vaccinated","Unvaccinated")
      ))%>%
      mutate(group=factor(group,levels=c("Unvaccinated","Partially Vaccinated","Fully Vaccinated")))%>%
      ggplot(aes(x=group,y=value,fill=factor(Category)))+geom_bar(position="stack",stat="identity")+
      xlab("")+ylab("Number of People")+ theme_bw()+theme(legend.title=element_blank())+
      scale_fill_viridis_d()+coord_cartesian(expand=FALSE)+ 
      geom_errorbar(data=total_data,aes(x=factor(Status),ymin=`Number Lower Bound`,ymax=`Number Upper Bound`),
                    inherit.aes=FALSE,size=1.2)+ylim(0,ymax)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
