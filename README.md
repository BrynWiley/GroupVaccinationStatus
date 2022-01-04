# Group Vaccination Status
This repository contains code for the group vaccination status R Shiny app located [here](https://shiney.zoology.ubc.ca/wiley/GroupVaccinationStatus/) <br>
/app contains all the necessary R files and supplementary data for the app to function, and is what is uploaded to the Shiny server. <br>
/vaccine_data contains the data and the R script for pulling current vaccination data from various sources for the Shiny app, and processesing them into a useable form. This data is refreshed every Monday, Wednesday, Friday, and Saturday at 12:00 UTC, and is downloaded from this location each time the app is run. <br>
