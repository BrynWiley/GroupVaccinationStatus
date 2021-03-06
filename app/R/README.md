# Essential App Functionality
This folder contains R files containing all the functions used by the app
* **evaluate_dataset.R** contains functions for final cleaning of the data used by the app, as supplied by both the user and the data processing steps in **/vaccine_data**.
* **first_tab_modules.R** contains a function for cleaning up a custom user-defined group information file.
* **forecast.R** constains all the functions required to fit both linear and logistic regressions to vaccination data, and then provide predictions and prediction intervals from those regressions.
* **prediction_plots.R** contains the functions for creating plots of past vaccination proportions and future predictions, as generated by the functions of **forecast.R**.
* **results.R** contains functions for properly formating the final group vaccination composition results from the predictions from **forecast.R** functions.
* **results_plots.R** contains functions for plotting the predicted group vaccination composition at the specified prediction date.
* **sanitize.R** contains functions for changing user inputed group composition data so it is appropriate for use by the app, and attempts to account for all possible user error.
