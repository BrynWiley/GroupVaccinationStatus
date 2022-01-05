library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
vaccine_data <- read_rds("vaccine_data.rds")

bc_data <- read_excel("(Master) COVID_LHA_CHSA_Data.xlsx",sheet="HA") %>%
  rowwise()%>%
  filter(HA_Name=="BC")%>%
  filter(Age_group != "12+ Years")%>%
  mutate(Date=as_date(Date))%>%
  mutate(Date=Date+days(1))%>%
  mutate(Age=if_else(Age_group=="All Ages",NA_character_,Age_group))%>%
  mutate(Age=str_remove(Age," Years"))%>%
  mutate(Portion_3=D3_coverage/100)%>%
  mutate(Location="Canada",Region="British Columbia")%>%
  select(Location,Region,Date,Age,Portion_3)%>%
  filter(!is.na(Portion_3),Age %in% vaccine_data$Age[vaccine_data$Region=="British Columbia"]) 

write_csv(bc_data,"bc_data.csv")
