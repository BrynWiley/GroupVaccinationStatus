# Vaccination raw data processing
This folder contains the processed data files used by the shiny app containing global vaccination information, along with the script to create them. </br>
**get_vaccine_data.R** is the R script that gathers vaccine information into a form useable by the app, and writes it into **vaccine_data.rds**. This script is run three times a week using a  github action. **states.tsv** contains state abbreviations used to decipher US state-based data, and **bad_locations.rds** contains a list of locations for which defficient or problematic vaccination data exists.
