library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(lubridate)
library(RSocrata)
library(cansim)
#This script defines functions to get available vaccine data from a variety of sources,
#format appropriately, and fill in missing values

#Returns a DF with following columns:
#Date, Location, Region, Age, Portion_1, Portion_2
#from all available sources
get_vaccine_data <- function(){
  bad_locations <- NULL
  #Get world data without age groups or sub regions
  owid_vaccine_data <-  read_csv("https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/vaccinations.csv")%>%
    rowwise()%>%
    mutate(people_fully_vaccinated_per_hundred = if_else(!is.na(total_vaccinations_per_hundred)&!is.na(people_vaccinated_per_hundred)
                                                         &is.na(people_fully_vaccinated_per_hundred&location != "China"),
                                                         total_vaccinations_per_hundred-people_vaccinated_per_hundred,people_fully_vaccinated_per_hundred))%>%
    mutate(people_vaccinated_per_hundred = if_else(!is.na(total_vaccinations_per_hundred)&!is.na(people_fully_vaccinated_per_hundred)
                                                   &is.na(people_vaccinated_per_hundred&location != "China"),
                                                   total_vaccinations_per_hundred-people_fully_vaccinated_per_hundred,people_vaccinated_per_hundred))%>%
    filter(!location %in% c("High income","Low income","Lower middle income","Upper middle income"))%>%
    rename(Location=location,Date=date)%>%
    group_by(Location)%>%
    group_modify(function(data,key){
      past_month <- data %>%
        filter(Date >= today()-days(30), !is.na(people_vaccinated_per_hundred))

      #Not enough recent measurements of vaccinations per person
      if(nrow(past_month)<1){
        data <- data %>%mutate(bad=TRUE)
        #when was the last 
        start_index <- max(which(!is.na(data$people_fully_vaccinated_per_hundred)))
        start_date <- data$Date[start_index]
        if(!is.na(start_date)){
          max_fully <- data$people_fully_vaccinated_per_hundred[start_index]
          data <- data %>%
            rowwise()%>%
            mutate(people_vaccinated_per_hundred = if_else(
              Date>start_date & is.na(people_vaccinated_per_hundred),
              total_vaccinations_per_hundred-max_fully,people_vaccinated_per_hundred))%>%
            mutate(people_fully_vaccinated_per_hundred=if_else(
              Date>start_date,max_fully,people_fully_vaccinated_per_hundred))
        }
        else {
          max_fully <- 0
          data <- data %>%
            rowwise()%>%
            mutate(people_vaccinated_per_hundred = if_else(
              is.na(people_vaccinated_per_hundred),
              total_vaccinations_per_hundred-max_fully,people_vaccinated_per_hundred))%>%
            mutate(people_fully_vaccinated_per_hundred=if_else(
              is.na(people_fully_vaccinated_per_hundred),max_fully,people_fully_vaccinated_per_hundred))
        } 
      }else {
        data <- data %>% mutate(bad=FALSE)
      }
      return(data)
    })
  bad_locations <- unique(owid_vaccine_data$Location[owid_vaccine_data$bad])
  owid_vaccine_data <- owid_vaccine_data %>%
    mutate(Portion_1 = people_vaccinated_per_hundred/100, Portion_2=people_fully_vaccinated_per_hundred/100, Portion_3=total_boosters_per_hundred/100,
           Region=NA,Age=NA)%>%
    select(Date,Location,Region,Age,Portion_1,Portion_2,Portion_3)%>%
    mutate(Portion_1=if_else(Portion_1>1.0,1.0,Portion_1)) %>%
    mutate(Date=as_date(Date)) 
  # %>%
  #   filter(!Location %in% c("United States","Canada"))
  
  #owid data with age groups, fill in missing lower age groups
  owid_age_data <- read_csv("https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/vaccinations-by-age-group.csv")%>%
    rename(Location=location,Date=date,Age=age_group)%>%
    mutate(Region=NA,
           Portion_1=people_vaccinated_per_hundred/100,
           Portion_2=people_fully_vaccinated_per_hundred/100,
           Portion_3=people_with_booster_per_hundred/100)%>%
    select(Date,Location,Region,Age,Portion_1,Portion_2,Portion_3) %>%
    group_by(Location,Region,Date)%>%
    group_modify(function(data,key){
      present_ages <- data$Age
      min_age <- min(as.numeric(unlist(str_extract_all(present_ages,"[[:digit:]]+"))))
      if(min_age>0){
        return(data%>%
                 add_row(
                   Portion_1=0.0,
                   Portion_2=0.0,
                   Portion_3=0.0,
                   Age=paste(0,min_age-1,sep="-")
                 ))
      }else{
        return(data)
      }
    }) %>%
    mutate(Date=as_date(Date))
  
  #US age data by region
  state_names <- read_tsv("states.tsv")
  US_data <- read.socrata(
    "https://data.cdc.gov/resource/unsk-b7fc.json",
    app_token = "0P7CcleIVNFBIHw2FiQgfasDF"
  )%>%
    group_by(date,location)%>%
    group_modify(function(data,key){
      pop_65=as.numeric(data$administered_65plus)/(as.numeric(data$admin_per_100k_65plus)/1e+05)
      
      pop_18=as.numeric(data$administered_18plus)/(as.numeric(data$admin_per_100k_18plus)/1e+05)-pop_65
      
      pop_12=as.numeric(data$administered_12plus)/(as.numeric(data$admin_per_100k_12plus)/1e+05)-pop_65-pop_18
      
      pop_5=as.numeric(data$administered_5plus)/(as.numeric(data$admin_per_100k_5plus)/1e+05)-pop_65-pop_18-pop_12
      
      Portion_1=as.numeric(data$administered_dose1_pop_pct)/100
      Portion_1_65=as.numeric(data$administered_dose1_recip_5)/pop_65
      Portion_1_18=(as.numeric(data$administered_dose1_recip_3)-as.numeric(data$administered_dose1_recip_5))/pop_18
      Portion_1_12=(as.numeric(data$administered_dose1_recip_1)-as.numeric(data$administered_dose1_recip_3))/pop_12
      Portion_1_5=(as.numeric(data$administered_dose1_recip_5plus)-as.numeric(data$administered_dose1_recip_1))/pop_5
      
      Portion_2=as.numeric(data$series_complete_pop_pct)/100
      Portion_2_65=as.numeric(data$series_complete_65plus)/pop_65
      Portion_2_18=(as.numeric(data$series_complete_18plus)-as.numeric(data$series_complete_65plus))/pop_18
      Portion_2_12=(as.numeric(data$series_complete_12plus)-as.numeric(data$series_complete_18plus))/pop_12
      Portion_2_5=(as.numeric(data$series_complete_5plus)-as.numeric(data$series_complete_12plus))/pop_5
      
      Portion_3=as.numeric(data$additional_doses_vax_pct)/100 * Portion_2
      Portion_3_65=as.numeric(data$additional_doses_65plus)/pop_65
      Portion_3_18=(as.numeric(data$additional_doses_18plus)-as.numeric(data$additional_doses_65plus))/pop_18
      
      Age=c("0-4", "5-11","12-17","18-64","65+",NA)
      Portion_1=c(0.0,Portion_1_5,Portion_1_12,Portion_1_18,Portion_1_65,Portion_1)
      Portion_1[is.nan(Portion_1)] <- 0.0
      Portion_2=c(0.0,Portion_2_5,Portion_2_12,Portion_2_18,Portion_2_65,Portion_2)
      Portion_2[is.nan(Portion_2)] <- 0.0
      Portion_3=c(0.0,0.0,0.0,Portion_3_18,Portion_3_65,Portion_3)
      Portion_3[is.nan(Portion_3)] <- 0.0
      return(tibble(
        Age=Age,
        Portion_1=Portion_1,
        Portion_2=Portion_2,
        Portion_3=Portion_3
      ))
    })%>%
    inner_join(state_names)%>%
    rename(Region=LongName,Date=date)%>%
    mutate(Location="United States")%>%
    #filter(!(Region=="United States" & is.na(Age)))%>%
    mutate(Region=if_else(Region=="United States",as.character(NA),Region))%>%
    ungroup(location)%>%
    select(Date,Location,Region,Age,Portion_1,Portion_2,Portion_3) %>%
    mutate(Date=as_date(Date)) %>%
    filter(!(is.na(Region)&is.na(Age)))
  
  
  canada_data <- get_canada_pop_data()
  write_rds(bad_locations,"bad_locations.rds")
  return(rbind(
    owid_vaccine_data,owid_age_data,US_data,canada_data
  )%>%
    mutate(Date=as_date(Date)))
}

get_canada_pop_data <- function(){
  #Canada data. Yuck.
  canada_vaccine_age <- read_csv("https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex.csv",col_types = cols(.default="c"),na = c("", "NA","na")) %>%
    mutate_at(vars(matches("num")),as.numeric) %>%
    mutate(age=if_else(age=="05-11","5-11",age))%>%
    mutate(age=if_else(age=="80+","70+",age))%>%
    mutate(age=if_else(age=="70-79","70+",age))%>%
    group_by(prename,age,week_end)%>%
    summarise(Num1=sum(numtotal_atleast1dose),
              Num2=sum(numtotal_fully),
              Num3=sum(numtotal_additional))%>%
    ungroup()%>%
    mutate(Age=age,
           Location="Canada",
           Date=as.Date(week_end))
  canada_vaccine_age$prename[canada_vaccine_age$prename=="Canada"]<-NA
  canada_vaccine_age <-canada_vaccine_age%>%
    rename(Region=prename)%>%
    select(Location,Region,Age,Date,Num1,Num2,Num3)
  
  canada_vaccine_age$Age[canada_vaccine_age$Age=="All ages"] <- NA
  
  #Age groups we'll use
  age1 <- c("0 to 4 years")
  age1.1 <- c("5 to 9 years","10 years","11 years")
  age1.2 <- c("12 years","13 years","14 years","15 years","16 years","17 years")
  age2 <- c("18 years", "19 years", "20 to 24 years" , "25 to 29 years",
            "30 to 34 years" , "35 to 39 years", "40 to 44 years" , "45 to 49 years")
  age2.1 <- c("18 years", "19 years", "20 to 24 years" , "25 to 29 years")
  age2.2 <- c("30 to 34 years" , "35 to 39 years")
  age2.3 <- c("40 to 44 years" , "45 to 49 years")
  age3 <- c("50 to 54 years" , "55 to 59 years")
  age4 <- c("60 to 64 years" , "65 to 69 years")
  age5 <- c("70 to 74 years")
  age6 <- c("75 to 79 years")
  age7 <- c("80 to 84 years","85 to 89 years","90 years and over")
  
  canada_pop_data <- get_cansim("17-10-0005") %>%
    normalize_cansim_values() %>%
    filter(Date==max(Date),Sex=="Both sexes") %>%
    mutate(age=case_when(`Age group` %in% age1 ~ "0-4",
                         `Age group` %in% age1.1 ~ "5-11",
                         `Age group` %in% age1.2 ~ "12-17",
                         `Age group` %in% age2.1 ~ "18-29",
                         `Age group` %in% age2.2 ~ "30-39",
                         `Age group` %in% age2.3 ~ "40-49",
                         `Age group` %in% age3 ~ "50-59",
                         `Age group` %in% age4 ~ "60-69",
                         #`Age group` %in% c(age5,age6) ~ "70-79",
                         #`Age group` %in% age6 ~ "75-79",
                         `Age group` %in% c(age5,age6,age7) ~ "70+",
                         `Age group` == "All ages" ~ "Total",
                         TRUE ~ NA_character_))%>%
    filter(!is.na(age))%>%
    rename(Age=age)%>%
    mutate(Location="Canada")%>%
    rename(Population=VALUE)%>%
    group_by(Location,Age,GEO)%>%
    summarise(Population=sum(Population))
  canada_pop_data$GEO[canada_pop_data$GEO=="Canada"] <- NA
  canada_pop_data$Age[canada_pop_data$Age=="Total"] <- NA
  canada_pop_data <- canada_pop_data%>%
    rename(Region=GEO)%>%
    select(Location,Region,Age,Population)
  
  #TEMPORARY! Use archived and manually updated BC CDC released excel files for BC Portion_3 data
  bc_data <- read_csv("bc_data.csv")
  
  canada_pop_data %>%
    inner_join(canada_vaccine_age)%>%
    mutate(Portion_1=Num1/Population,
           Portion_2=Num2/Population,
           Portion_3=Num3/Population)%>%
    select(Date,Location,Region,Age,Portion_1,Portion_2,Portion_3)%>%
    full_join(.,bc_data,by=c("Date", "Location","Region","Age")) %>%
    mutate(Portion_3=if_else(is.na(Portion_3.x),Portion_3.y,Portion_3.x))%>%
    select(!c("Portion_3.y","Portion_3.x")) %>%
    distinct()%>%
    filter(!(is.na(Age)&is.na(Region))) %>%
    mutate(Date=as_date(Date))
  
}
setwd("./vaccine_data")
data<- get_vaccine_data()
write_rds(data,"vaccine_data.rds")