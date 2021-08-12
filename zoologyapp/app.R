
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
library(magrittr)
library(shiny)
library(lubridate)
library(rhandsontable)
library(investr)

#default option
default_option <- NA
default_portion <- 1.0

#setwd("./data")
temp <- tempfile()
githubURL <- ("https://raw.githubusercontent.com/BrynWiley/GroupVaccinationStatus/main/vaccine_data/vaccine_data.rds")
#download.file(githubURL,"vaccine_data.rds", method="curl")
#vaccine_data <- read_rds("vaccine_data.rds")
download.file(githubURL,temp,method="curl")
vaccine_data <- read_rds(temp)
unlink(temp)
githubURL <- ("https://raw.githubusercontent.com/BrynWiley/GroupVaccinationStatus/main/vaccine_data/bad_locations.rds")
#download.file(githubURL,"bad_locations.rds", method="curl")
#bad_locations <-read_rds("bad_locations.rds")
download.file(githubURL,temp,method="curl")
bad_locations <- read_rds(temp)
unlink(temp)
#setwd("..")



#vaccine data to be used throughout app, uncommend if default_option is NOT NA
# vaccine_data <- vaccine_data%>%
#   rowwise()%>%
#   mutate(Region=if_else(is.na(Region),default_option,Region))%>%
#   mutate(Age=if_else(is.na(Age),"All",Age))

setwd("./R")
sapply(list.files("."),function(X){source(X)})
setwd("..")


#starting values for various reactive values
starting_date <- today()+days(7)
starting_method <- "linear"
starting_pop_data <- read_csv("./data/pop_data.csv",
                              col_types = cols(Location=col_character(),
                                               Region=col_character(),
                                               Age=col_character(),
                                               `Number of People`=col_number(),
                                               `Maximum Portion Vaccinated`=col_number()))
starting_selected_categorized_vaccine_data <- vaccine_data %>%
  inner_join(starting_pop_data)%>%
  categorize(.,default_option)%>%  
  select(!`Number of People`)%>%
  select(!`Maximum Portion Vaccinated`)
starting_predictions <- get_predictions(starting_pop_data,starting_date,vaccine_data,starting_method)
starting_predictions_categorized <- starting_predictions %>% categorize(.,default_option)
starting_results <- get_results(starting_pop_data,starting_predictions)
starting_total <- get_total_results(starting_results,starting_pop_data)



#starting initial values for 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage("", id="mainpage",
             #Home Tab##########################################################################################
             tabPanel(icon("home"),value="home",
                      fluidRow(
                        titlePanel("COVID-19 Group Vaccine Status Estimation"),
                        column(width=8,
                               h3("Estimate the vaccination status of a group of people at a future date"),
                               p("This app estimates the numbers of fully vaccinated, partially vaccinated, and unvaccinated people in a group at a future date. 
                                 These people can be from different locations and from different age groups subject to data availability."),
                               div(
                                 p(tags$strong("This app consists of the following steps:")),
                                 p(tags$ul(
                                   tags$li("Step 1: Specify group members"),
                                   tags$li("Step 2: Specify estimation date and other settings"),
                                   tags$li("Step 3: Review the estimated vaccination rates per location and age group"),
                                   tags$li("Step 4: View the estimated group vaccination status")
                                 ))
                               ),
                               actionButton("start","Start",
                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:5px; font-size:150%")
                        ),
                        column(width=4,
                               tags$img(src="virus_image.png",width="300px"))
                      ),
                      hr(),
                      p("Created by Bryn Wiley. Thank you to Dr. Sarah Otto, Dr. Daniel J. McDonald, and the BC COVID-19 Modelling Group for their substantial input and suggestions.")
             ),
             #First Tab##########################################################################################
             tabPanel("Who's in the group?",value="first",
                      #New main body of First Page
                      h2("Step 1: Add Group Members"),
                      sidebarLayout(position="right",
                                    sidebarPanel = sidebarPanel(
                                      h5(actionButton("save","Apply Changes", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")," to save group changes and check for data ability"),
                                      htmlOutput("messages"),
                                      hr(),
                                      h5(actionButton("tosecond","Next Step", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")," to move to the next step")
                                    ),
                                    mainPanel = mainPanel(
                                      div(
                                        p(tags$strong("To add or remove group members"),"either"),
                                        p(tags$ul(
                                          tags$li("Click or right click on the table to adjust the defaults,"),
                                          tags$li("Use the dropdown menus below, or"),
                                          tags$li("Upload a csv file containing group membership information")
                                        ))
                                      ),
                                      hr(),
                                      div(
                                        p(tags$strong("Location: "),"A location for individuals to originate from (eg: United States)"),
                                        p(tags$strong("Region: "),"A region of a selected location. Currently only supports US states or Canadian provinces"),
                                        p(tags$strong("Age: "),"An age group for individuals from the selected location and region. Not available for all locations or regions."),
                                        p(tags$strong("Number of People: "),"The number of people from the selected location, region, and age group."),
                                        p(tags$strong("Maximum Portion Vaccinated: "),"The maximum allowable portion of people vaccinated. 
                                   If you have an upper limit on what you expect the maximum portion of people to be you can set this to a value between 0 and 1.")
                                      ),
                                      rHandsontableOutput("data_table"),
                                      hr(),
                                      fluidRow(
                                        column(
                                          3,
                                          selectInput(
                                            "location_input",
                                            "Location",
                                            choices = unique(vaccine_data$Location)
                                          ),
                                          numericInput(
                                            "people_input",
                                            "Number of People",
                                            value=0,
                                            min=0
                                          )
                                        ),
                                        column(
                                          3,
                                          selectInput(
                                            "region_input",
                                            "Region",
                                            choices=c(" ")
                                          ),
                                          numericInput(
                                            "portion_input",
                                            "Max Portion Vaccinated",
                                            value=1.0,
                                            min=0,
                                            max=1
                                          )
                                        ),
                                        column(
                                          3,
                                          selectInput(
                                            "age_input",
                                            "Age",
                                            choices=c(" ")
                                          )
                                        ),
                                        column(
                                          3,
                                          actionButton(
                                            "add_row",
                                            "Add to Dataset"
                                          ),
                                          hr(),
                                          actionButton(
                                            "remove_row",
                                            "Remove Last Row"
                                          )
                                        )
                                      ),
                                      hr(),
                                      fileInput(
                                        "filename",
                                        "Entrant File (csv file)",
                                        accept=".csv"
                                      ),
                                      p("To ensure required group data is present, use this ",downloadLink("template_download",
                                                                                                           "template")," for file upload.")
                                    )
                      )
             ),
             #Second Tab##########################################################################################
             tabPanel("Date and Estimation Settings",value="second",
                      h2("Step 2: Date and Estimation Settings"),
                      sidebarLayout(position="right",
                                    mainPanel =  mainPanel(
                                      dateInput(
                                        "date_input",
                                        "Estimation Date",
                                        value=today()+weeks(1)
                                      ),
                                      p("This is the date on which vaccination status estimation will be made. It is not recommended to specify this more than 3 weeks in the future."),
                                      hr(),
                                      selectInput(
                                        "method_input",
                                        "Prediction Method",
                                        c("Linear Regression on the past 3 weeks of available data"="linear",
                                          "Logistic Regression on the past 6 weeks of available data"="logistic")
                                      ),
                                      p("This specifies how the app will estimate future vaccination rates. There are two options:"),
                                      p(tags$ul(
                                        tags$li("Linear regression assumes vaccination coverage will continue increasing at its present rate in the future. This is best used for short term estimation."),
                                        tags$li("Logistic regression assumes vaccination coverage accelerates early then decelerates and plateaus later. This produces a more pessimistic prediction better for longer term estimation.")
                                      ))
                                    ),
                                    sidebarPanel = sidebarPanel(
                                      h5(actionButton("backtofirst","Previous Step", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")," to move back to group members"),
                                      hr(),
                                      h5(actionButton("tothird","Next Step", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")," to move to the next step")
                                    )
                      )
                      
             ),
             
             #Third Tab##########################################################################################
             tabPanel("Estimated Vaccination Rates",value="third",
                      h2("Step 3: Review Estimated Vaccination Rates "),
                      p("Review the past and future estimated vaccination rates per location, region, and age group"),
                      hr(),
                      sidebarLayout(position="right",
                                    mainPanel = mainPanel(
                                      h3("Portion with at least one dose, by category"),
                                      plotOutput("first_plot"),
                                      hr(),
                                      h3("Portion fully vaccinated, by category"),
                                      plotOutput("second_plot")
                                    ),
                                    sidebarPanel = sidebarPanel(
                                      
                                      h5(actionButton("backtosecond","Previous Step", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")," to move back to date and estimation settings"),
                                      hr(),
                                      h5(actionButton("tofourth","Next Step", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")," to move to the next step"),
                                      hr(),
                                      p(tags$strong("Dots "), "are predictions up to the specified prediction date. "), 
                                      p(tags$strong("Dashed lines"), "are locations with uncertain data, 
                                      and they include approximations or estimations for past vaccination rates. This app likely overestimates
                                        the proportion with at least one dose and underestimates the proportion fully vaccinated for these locations (see Data Sources and Explanations for more information)."),
                                      p("Locations and age groups with too few past observations cannot generate reasonable future estimations. 
                                      For these, we assume the vaccination rate will be constant in the future."),
                                      hr(),
                                      checkboxInput("plot_uncertainty","Plot uncertainty in vaccine rate prediction (modified 95% prediction intervals, see Data Sources and Explanations)",FALSE),
                                      checkboxGroupInput(
                                        "to_graph",
                                        "Graphed Categories",
                                        NULL
                                      )
                                      
                                    )
                      )
             ),
             
             
             #Fourth Tab##########################################################################################
             tabPanel("Estimated Group Vaccine Coverage",value="fourth",
                      h2("Step 4: Review Estimated Group Vaccination Coverage"),
                      p("Review the estimated numbers or percentages of people unvaccinated, partially vaccinated, or fully vaccinated in the group"),
                      hr(),
                      sidebarLayout(position="right",
                                    mainPanel = mainPanel(
                                      htmlOutput("bad_location_warning"),
                                      h3("Estimated Group Vaccination  Coverage"),
                                      plotOutput("bar_plot"),
                                      hr(),
                                      h3("Estimated Vaccination Status Totals and Confidence Interval Bounds (rounded)"),
                                      dataTableOutput("totals_table"),
                                      downloadButton("download_totals", "Download totals as a csv file"),
                                      hr(),
                                      h3("Estimated Vaccination Status Totals by Category (rounded)"),
                                      dataTableOutput("results_table"),
                                      hr(),
                                      downloadButton("download_results", "Download totals by category as a csv file")
                                    ),
                                    sidebarPanel = sidebarPanel(
                                      checkboxInput("percent","Scale results as percent of total group size"),
                                      hr(),
                                      p("Error bars on graphs represent the uncertainty in prediction for total numbers of each level of vaccination. They are 
                                      estimated 95% confidence intervals, see Data Sources and Explanations for more information."),
                                      hr(),
                                      h5(actionButton("backtothird","Previous Step", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")," to move back to vaccination rate visualisation")
                                    )
                      )
                      
             ),
             tabPanel("Data Sources and Explanations",
                      p(tags$strong("Disclamer: "),"Estimations of future vaccination rates are made based on available data, which can be infrequent or incomplete (see below). 
                        These predictions are made by extrapolating from recent trends, and changes in vaccination policy, vaccination availability, and anything that makes the individuals gathering 
                        less representative of the places they come from would cause errors in predictions. Projecting vaccinations into the future is overall highly uncertain, and is subject 
                        to increasing error the further the event is into the future."),
                      hr(),
                      p(tags$strong("Vaccination data "),
                        "is taken from Our World in Data, the US CDC, and the Public Health Agency of Canada. 
                        There are some countries who will primarily report total doses given instead of first or second doses. If a country has not reported a quantity for first or second doses within
                        the past month we assume that the proportion fully vaccinated remains constant from the last date it was reported and all new reported doses are first doses. This is conservative with regards 
                        to fully vaccinated individuals but optimistic to the number partially vaccinated. Currently, the countries to which this applies are: ", sub("(.*),", "\\1 and",paste(bad_locations, collapse = ", "))),
                      p(tags$strong("Prediction using linear regression"), " uses available data from the past 3 weeks to estimate a linear model with the ", code("lm")," function in R. 
                        If there are two or less observations in this period, regression is deemed not viable and so the vaccination rate is held constant."),
                      p(tags$strong("Prediction using logistic regression"), " uses available data from the past 6 weeks to estimate a logistic (s-shaped) curve using the ", code("nls"), " function in R.
                         More recent observations are weighted stronger than observations further in the past. Starting values are generated using the ", code("SSlogis"), " function, but if the starting estimate for the asymptote is greater than the maximum allowed value or less than the current vaccination rate,
                        it is adjusted to the maximum allowed value or the current value respectively. In the event that the ", code("nls"), " function fails to fit a logistic function, a linear regression is fit instead. 
                        Again, if there are two or less observations in this period, regression is deemed not viable and so the vaccination rate is held constant"),
                      p(tags$strong("95% prediction intervals for estimated vaccination rates")," are generated from the ", code("predict"), " function for linear regression, or from the ",code("predFit"), " function from the ", code("investr"), " package in R for logistic regression.
                        If the upper bound of the prediction interval is larger than 1 or the maximum specified amount, it is reduced to this maximum amount. For the proportion fully vaccinated 
                        this upper amount is either the maximum specified amount or the maximum predicted proportion with at least one vaccination. Similarly, if the lower bound of the prediction interval is smaller than the 
                        most recent observation it is set to be equal to the most recent observation, as vaccination rates should not decrease."),
                      p(tags$strong("Confidence intervals for total group vaccination percentages "),"are estimated by taking the maximum and minimum predicted totals for each vaccination status,
                        as generated by the vaccination rate prediction intervals from each category, and then adding a safe binomial confidence interval of 1/sqrt(N), where N is the total group size."),
                      hr(),
                      p(tags$strong("Data sources:")),
                      p("Centers for Disease Control and Prevention. (2021). COVID-19 Vaccinations in the United States,Jurisdiction [Data set]. Retrieved from https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc"),
                      p("Mathieu, E., Ritchie, H., Ortiz-Ospina, E. et al. A global database of COVID-19 vaccinations. Nat Hum Behav (2021). https://doi.org/10.1038/s41562-021-01122-8"),
                      p("Public Health Agency of Canada. Canadian COVID-19 vaccination coverage report. Ottawa: Public Health Agency of Canada. https://health-infobase.canada.ca/covid-19/vaccination-coverage/"),
                      hr(),
                      p(tags$strong("All relevant code "), "is hosted at ",tags$a(href="https://github.com/BrynWiley/GroupVaccinationStatus", "https://github.com/BrynWiley/GroupVaccinationStatus")),
                      p(tags$strong("Questions or comments? "),"Email Bryn Wiley at",tags$a(href="mailto:wiley@zoology.ubc.ca", "wiley@zoology.ubc.ca"))
             )
             
  ) 
)



server <- function(input, output,session) {
  #Reactive values
  vals <- reactiveValues(
    date=starting_date,
    method=starting_method,
    pop_data = starting_pop_data,
    selected_categorized_vaccine_data = starting_selected_categorized_vaccine_data,
    predictions = starting_predictions,
    predictions_categorized = starting_predictions_categorized,
    results = starting_results,
    total = starting_total
  )
  #Home Page
  observeEvent(input$start,{
    updateTabsetPanel(session,"mainpage",selected="first") 
  }
  )
  #First Tab##########################################################################################
  output$data_table <- renderRHandsontable({
    rhandsontable(starting_pop_data)
  })
  
  
  #Dropdown Updating
  observe({
    Location <- input$location_input
    Region <- input$region_input
    if(Region==" " | Region == ""){
      Region=default_option
    }
    Regions <- unique(vaccine_data$Region[vaccine_data$Location==Location])
    
    
    if(Region %in% Regions){
      Ages <- unique(vaccine_data$Age[vaccine_data$Location==Location & vaccine_data$Region==Region])
      if(compareNA(Region,default_option)){
        Ages <- unique(vaccine_data$Age[vaccine_data$Location==Location & is.na(vaccine_data$Region)])
      }
      Ages[is.na(Ages)] <- " "
      updateSelectInput(session=session,inputId = "age_input",
                        choices= Ages)
      if(is.na(Region)){Region=""}
      Regions[is.na(Regions)] <- " "
      updateSelectInput(session=session, inputId="region_input",
                        choices=Regions,
                        selected = Region)
    } else {
      Regions[is.na(Regions)] <- " "
      Ages <- unique(vaccine_data$Age[vaccine_data$Location==Location & is.na(vaccine_data$Region)])
      updateSelectInput(session=session,inputId = "age_input",
                        choices= Ages)
      updateSelectInput(session=session,inputId = "region_input",
                        choices = Regions)
    }
  })
  
  #Adding a row to the data table
  observeEvent(input$add_row,{
    Location <- input$location_input
    Region <- input$region_input
    Age <- input$age_input
    if(Location=="" | Location == " "){
      Location = NA
    }
    if(Region=="" | Region == " "){
      Region = NA
    }
    if(Age=="" | Age == " "){
      Age = NA
    }
    `Number of People` <- as.numeric(input$people_input)
    `Maximum Portion Vaccinated` <- as.numeric(input$portion_input)
    row <- c(Location,Region,Age,`Number of People`,`Maximum Portion Vaccinated`)
    row[row==" "]<-default_option
    old_data <- hot_to_r(input$data_table)
    output$data_table <- renderRHandsontable({
      rhandsontable(old_data %>%
                      add_row(
                        Location=row[1],
                        Region=row[2],
                        Age=row[3],
                        `Number of People`=as.numeric(row[4]),
                        `Maximum Portion Vaccinated` = as.numeric(row[5])
                      ))
    })
    
  })
  
  #Removing a row from the data table
  observeEvent(input$remove_row,{
    old_data <- hot_to_r(input$data_table)
    if(nrow(old_data)>0){
      old_data <- old_data[-nrow(old_data),]
    }
    output$data_table <- renderRHandsontable({
      rhandsontable(old_data)
    })
  })
  
  #Saving data to vals from rhandson table and printing warning/error messages
  observeEvent(input$save,{
    pop_data <- hot_to_r(input$data_table)
    
    sanitized <- evaluate_dataset(pop_data,vaccine_data,default_option,default_portion)
    output$messages <- renderUI({
      HTML(sanitized$message)
    })
    if(sanitized$good){
      pop_data = sanitized$sanitized_data
      output$data_table <- renderRHandsontable(
        rhandsontable(pop_data)
      )
      predictions <- get_predictions(pop_data,input$date_input,vaccine_data,input$method_input)
      results <- get_results(pop_data,predictions)
      
      
      vals$pop_data = pop_data
      vals$selected_categorized_vaccine_data = vaccine_data %>%
        inner_join(pop_data)%>%
        categorize(.,default_option)%>%  
        select(!`Number of People`)%>%
        select(!`Maximum Portion Vaccinated`)
      vals$predictions= predictions
      vals$predictions_categorized=predictions %>% categorize(.,default_option)
      vals$results = results
      vals$total  = get_total_results(results,pop_data)
    }
  })
  
  
  #Get a file from the user, add missing columns, and add to display
  observe({
    file <- input$filename
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    pop_data <- read_csv(file$datapath,
                         col_types = cols(Location=col_character(),
                                          Region=col_character(),
                                          Age=col_character(),
                                          `Number of People`=col_number(),
                                          `Maximum Portion Vaccinated`=col_number()))
    output$data_table <- renderRHandsontable(
      rhandsontable(pop_data)
    )
  })
  
  #Provide a template to the user for file upload
  output$template_download <- downloadHandler(
    filename=function(){"template.csv"},
    content=function(file){
      template <- read_csv("./data/pop_data.csv")
      write_csv(template,file)
    }
  )
  
  #Move to second tab
  observeEvent(input$tosecond,{
    updateTabsetPanel(session,"mainpage",selected="second") 
  }
  )
  #Second Tab##########################################################################################
  observe({
    if(input$date_input > today()+weeks(3)){
      showNotification("Dates more than 3 weeks in the future are not recommended",type="warning")
    }
    vals$date<-input$date_input
    vals$method<-input$method_input
    
    predictions <- get_predictions(vals$pop_data,input$date_input,vaccine_data,input$method_input)
    results <- get_results(vals$pop_data,predictions)
    
    
    vals$predictions= predictions
    vals$predictions_categorized=predictions %>% categorize(.,default_option)
    vals$results = results
    vals$total  = get_total_results(results,vals$pop_data)
  })
  observeEvent(input$tothird,{
    updateTabsetPanel(session,"mainpage",selected="third") 
  }
  )
  observeEvent(input$backtofirst,{
    updateTabsetPanel(session,"mainpage",selected="first") 
  }
  )
  #Third Tab##########################################################################################
  #Update checked categories input
  observe({
    categories <- unique(vals$predictions_categorized$Category)
    updateCheckboxGroupInput(session=session,inputId = "to_graph",
                             choices = categories,
                             selected = categories)
  })
  
  #Plot graphs for expected vaccination rates by category
  observe({
    categories <- input$to_graph
    uncertainty <- input$plot_uncertainty
    
    predictions_category <- vals$predictions_categorized %>%
      filter(Category %in% categories)
    vaccine_data_selected <- vals$selected_categorized_vaccine_data%>%
      filter(Category %in% categories)
    
    plots <- prediction_plots(predictions_category,vaccine_data_selected,bad_locations,uncertainty)
    output$first_plot <- renderPlot({plots[[1]]})
    output$second_plot <- renderPlot({plots[[2]]})
  })
  observeEvent(input$tofourth,{
    updateTabsetPanel(session,"mainpage",selected="fourth") 
  }
  )
  observeEvent(input$backtosecond,{
    updateTabsetPanel(session,"mainpage",selected="second") 
  }
  )
  
  #Fourth Tab##########################################################################################
  #Output warning about bad locations
  output$bad_location_warning <- renderUI({
    chosen_bad <- intersect(vals$pop_data$Location,bad_locations)
    message <- ""
    if(length(chosen_bad)==1){
      locations_string <- sub("(.*),", "\\1 and",paste(chosen_bad, collapse = ", "))
      first_part <- tags$strong("Note:")
      second_part <- " These estimates include data from "
      third_part <- " which does not have recent full or partial vaccination status data. This means 
      these predictions likely overstate the number partially vaccinated, and understate the number fully vaccinated 
      (see Data Sources and Explanations)."
      message <- paste0(first_part, second_part, locations_string,third_part)
    } else if(length(chosen_bad > 1)){
      locations_string <- sub("(.*),", "\\1 and",paste(chosen_bad, collapse = ", "))
      first_part <- tags$strong("Note:")
      second_part <- " These estimates include data from "
      third_part <- " which do not have recent full or partial vaccination status data. This means 
      these predictions likely overestimate the number partially vaccinated, and underestimate the number fully vaccinated 
      (see Data Sources and Explanations)."
      message <- paste0(first_part, second_part, locations_string,third_part)
    }
    HTML(message)
    
  })
  
  #Output bar chart with prediction intervals
  output$bar_plot <- renderPlot(
    result_bar_plot(vals$results, vals$total,default_option,TRUE,input$percent)
  )
  #Output total totals data table
  output$totals_table <- renderDataTable({
    total <- vals$total
    if(input$percent){
      total <- total %>%
        mutate(Percent = round(Proportion*100),
               `Percent Upper Bound` = round(`Proportion Upper Bound`*100),
               `Percent Lower Bound` = round(`Proportion Lower Bound`*100))%>%
        select(Status, Percent, `Percent Upper Bound`, `Percent Lower Bound`)
      
    } else {
      total <- total %>%
        mutate(Persons = round(Number),
               `Persons Upper Bound`=round(`Number Upper Bound`),
               `Persons Lower Bound`=round(`Number Lower Bound`))%>%
        select(Status, Persons, `Persons Upper Bound`,`Persons Lower Bound`)
    }
    total
  })
  
  #Output results by "category" in a data table
  output$results_table <- renderDataTable({
    results <- vals$results
    if(input$percent){
      results <- results %>%
        mutate(`Percent Unvaccinated`=round(Proportion_unvacc*100),
               `Percent Partially Vaccinated`=round(Proportion_partially*100),
               `Percent Fully Vaccinated`=round(Proportion_fully*100))%>%
        select(Location, Region, Age, starts_with("Percent"))
    } else {
      results <- results %>%
        mutate(`Persons Unvaccinated`=round(Number_unvacc),
               `Persons Partially Vaccinated`=round(Number_partially),
               `Persons Fully Vaccinated`=round(Number_fully))%>%
        select(Location, Region, Age, starts_with("Persons"))
    }
    results
  })
  #Download totals
  output$download_totals <- downloadHandler(
    filename=function(){"totals.csv"},
    content=function(file){
      total <- vals$total
      if(input$percent){
        total <- total %>%
          mutate(Percent = Proportion*100,
                 `Percent Upper Bound` = `Proportion Upper Bound`*100,
                 `Percent Lower Bound` = `Proportion Lower Bound`*100)%>%
          select(Status, Percent, `Percent Upper Bound`, `Percent Lower Bound`)
        
      } else {
        total <- total %>%
          rename(Persons = Number,
                 `Persons Upper Bound`=`Number Upper Bound`,
                 `Persons Lower Bound`=`Number Lower Bound`)%>%
          select(Status, Persons, `Persons Upper Bound`,`Persons Lower Bound`)
      }
      write_csv(total,file)
    }
  )
  #Download results
  output$download_results <- downloadHandler(
    filename=function(){"results.csv"},
    content=function(file){
      results <- vals$results
      if(input$percent){
        results <- results %>%
          mutate(`Percent Unvaccinated`=Proportion_unvacc*100,
                 `Percent Partially Vaccinated`=Proportion_partially*100,
                 `Percent Fully Vaccinated`=Proportion_fully*100)%>%
          select(Location, Region, Age, starts_with("Percent"))
      } else {
        results <- results %>%
          mutate(`Persons Unvaccinated`=Number_unvacc,
                 `Persons Partially Vaccinated`=Number_partially,
                 `Persons Fully Vaccinated`=Number_fully)%>%
          select(Location, Region, Age, starts_with("Persons"))
      }
      write_csv(results,file)
    }
  )
  
  observeEvent(input$backtothird,{
    updateTabsetPanel(session,"mainpage",selected="third") 
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
