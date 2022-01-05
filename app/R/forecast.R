#This file contains function s related to predictions
#for the shiny app (namely logistic and linear models)

# library(tidyverse)
# library(lubridate)
#library(tseries)
#library(imputeTS)
# library(investr)


#' A function to return a logistic predition model. Attempts to fit a logistic model with
#' nls with a starting value for the asymptote of the maximum allowable level for the response value. Also
#' uses weighted least squares, with more recent observations getting a higher weight.
#' If this model cannot be fit, returns FALSE
#' @description A function to attempt to fit a weighted logistic model
#' @param x The independent variable for regression, must be a numeric vector
#' @param y The dependent variable for regression, must be a numeric vector
#' @param max_level the maximum allowed value for the asymptote for a logistic model
#' @return A regression model from either the nls logistic model fit, or FALSE t if this
#' isn't possible
predict_logistic <- function(x,y,max_level){
  df <- data.frame(
    x=x,y=y
  )
  #We don't allow asymptotes below the current value
  min_level <- max(y)
  model <- tryCatch({
    SS <- getInitial(y~SSlogis(x,Asym,xmid,scal),data=df)
    #Make sure the asymptote (SS[1] is between min_level and max_level)
    if(SS[1]>max_level & max_level > max(y)){
      SS[1]<-max_level
    } else if (SS[1]>1){
      SS[1]<-1
    } else if (SS[1]<min_level){
      SS[1]<-min_level
      
    }
    
    if(max_level < max(y)){
      max_level <- 1
    }
    #Weighting for logistic regression (more recent dates are more important)
    x_corrected = (max(x)-x)
    weights = 0.8^(x_corrected)
    
    #Attempt to fit the model
    nls(y~SSlogis(x,Asym,xmid,scal),data=df,
        weights=weights,
        nls.control(warnOnly=TRUE,maxiter=500,minFactor=2e-16),
        upper=c(max_level,Inf,Inf),lower=c(min_level,-Inf,-Inf),
        start=list(
          Asym=SS[1],
          xmid=SS[2],
          scal=SS[3]
        ),
        algorithm="port")},
    #If the model fit doesn't fit, use a linear regression instead
    error=function(c){
      FALSE
    })
  
  return(model)
}

#' A function that gets the predictions for vaccination rates on specificed dates using a given
#' set of observations, along with a specified maximum possible vaccination level and a desired
#' estimation method
#' @description A function to get vaccination rate predictions
#' @param observations A data frame of past vaccination rates, must include variables level (the vaccination rate) and date (the date of the past vaccination rate observation)
#' @param forecast_method A string for the desired forecast method. Must be one of "logistic" or "linear" for a logistic or linear model fit respectively.
#' @param max_level The maximum possible vaccination rate. All predictions will be less than or equal to this rate.
#' @param prediction_dates The dates for prediction. Must be a vector of Date objects.
vaccine_forecast <- function(observations,forecast_method,max_level,prediction_dates){
  types <- c("holt-winters","logistic","linear")
  if(!forecast_method %in% types){
    return(-1)
  }

  date <- prediction_dates
  #Predicted vaccination level
  fit <- NULL
  #Upper bound of prediction interval
  upper <- NULL
  #Lower bound of prediction interval
  lower <- NULL
  
  max_observed <- max(observations$level)

  num_dates <- length(prediction_dates)
  
  reg = TRUE
  #logistic regression
  if(forecast_method==types[2]){
    #Filter for past 42 days of available observations
    observations <- observations %>%
      filter(date >= max(observations$date)-days(42), level>0,!is.na(level))
    #If not enough observations, hold vaccination rate constant (we cannot perform a reasonable regression)
    if(nrow(observations)<=2 & nrow(observations)>0){
      fit=rep(max_observed,num_dates)
      upper=rep(max_observed,num_dates)
      lower=rep(max_observed,num_dates)
    }else if(nrow(observations)<=0){
      fit=rep(0,num_dates)
      upper=rep(0,num_dates)
      lower=rep(0,num_dates)
    } else {
      reg <- predict_logistic(as.numeric(observations$date),observations$level,max_level)
      if(!is.logical(reg)){
        predictions <- tryCatch({
          predFit(reg,newdata=data.frame(x=as.numeric(prediction_dates)),
                  interval="prediction",level=0.95)
        },
        error=function(c){
          forecast_method=types[3]
          reg <- lm(level~date,data=observations)
          predFit(reg,newdata=data.frame(x=as.numeric(prediction_dates)),
                  interval="prediction",level=0.95)
        })
        if(nrow(predictions)==1){
          if(predictions[1]>max_level){
            predictions <- rep(max_level,3)
          }
          fit=predictions[1]
          upper=predictions[3]
          lower=predictions[2]
        } else {
          fit=predictions[,1]
          upper=predictions[,3]
          lower=predictions[,2]
        }
      } else {
        forecast_method=types[3]
      }
    }
  }
  
  #linear regression
  if(forecast_method == types[3] | is.logical(reg)){
    #Filter for past 3 weeks of available non-zero observations
    observations <- observations %>%
      filter(date >= max(observations$date)-days(21), level>0, !is.na(level))
    #Not enough observations, hold rate constant (we can't perform a reasonable regression)
    if(nrow(observations)<=2 & nrow(observations)>0){
      fit=rep(max_observed,num_dates)
      upper=rep(max_observed,num_dates)
      lower=rep(max_observed,num_dates)
    }else if(nrow(observations)<=0){
      fit=rep(0,length(prediction_dates))
      upper=rep(0,length(prediction_dates))
      lower=rep(0,length(prediction_dates))
      #Otherwise, perform regression, and include prediction intervals
    } else {
      reg <- lm(level~date,data=observations)
      predictions <- predict(reg, newdata=data.frame(date=prediction_dates),
                             interval="prediction")
      if(nrow(predictions)==1){
        if(predictions[1]>max_level){
          predictions <- rep(max_level,3)
        }
        fit=predictions[1]
        upper=predictions[3]
        lower=predictions[2]
      } else {
        fit=predictions[,1]
        upper=predictions[,3]
        lower=predictions[,2]
      }
    }
  }
  
  
  
  #Holt-Winters with DAILY interpolation
  #Not good, turned off
  if(forecast_method == types[1]){
    return(-1)
  }
  #Make sure predictions and their intervals are greater than the greatest observation (vaccination rates can't go down),
  #and less than the maximum allowable value
  min_level <- max(observations$level)
  print(fit)
  print(prediction_dates)

  if(min_level>max_level){
    min_level <- max_level
  }
  
  fit[fit>max_level]<-max_level
  fit[fit<min_level]<-min_level
  upper[upper>max_level]<-max_level
  upper[upper<min_level]<-min_level
  lower[lower>max_level]<-max_level
  lower[lower<min_level]<-min_level
  return(tibble(
    date=prediction_dates,
    fit=fit,
    upper=upper,
    lower=lower
  ))
}


#gets predictions for all days until prediction date
#returns the date, the predicted portion 1 and portion 2,
#and the upper/lower bound for these predictions
#Pop data: the population information data (includes Location, Region, Age, Number of People, and Maximum Portion Vaccinated)
get_predictions <- function(pop_data, prediction_date,vaccine_data,forecast_method="linear"){
  vaccine_data %>%
    inner_join(pop_data)%>%
    group_by(Location,Region,Age)%>%
    group_modify(function(data,key){
      max_rate = unique(data$`Maximum Portion Vaccinated`)
      if(prediction_date<=today()){
        max_date <- max(data$Date[data$Date <= prediction_date & !is.na(data$Portion_1) & !is.na(data$Portion_2)])
        if(is.infinite(max_date)){
          max_date <- min(data$Date[!is.na(data$Portion_1) & !is.na(data$Portion_2)])
        }
        index = (data$Date == max_date)
        return(tibble(
          Date=prediction_date,
          Portion_1=data$Portion_1[index],
          Portion_1_lower=data$Portion_1[index],
          Portion_1_upper=data$Portion_1[index],
          Portion_2=data$Portion_2[index],
          Portion_2_lower=data$Portion_2[index],
          Portion_2_upper=data$Portion_2[index],
          Portion_3=data$Portion_3[index],
          Portion_3_lower=data$Portion_3[index],
          Portion_3_upper=data$Portion_3[index],
        ))
      }
      
      to_return <- tibble(
        Date=as_date(integer()),
        Portion_1=numeric(),
        Portion_1_lower=numeric(),
        Portion_1_upper=numeric(),
        Portion_2=numeric(),
        Portion_2_lower=numeric(),
        Portion_2_upper=numeric(),
        Portion_3=numeric(),
        Portion_3_lower=numeric(),
        Portion_3_upper=numeric(),
      )
      
      prediction_dates <- today()
      if(prediction_date >= today()){
        prediction_dates <- seq(today()+days(1),prediction_date,by="days")
      }
      
      to_pass_first = data %>%
        filter(!is.na(Portion_1))%>%
        select(Date,Portion_1)%>%
        rename(date=Date,level=Portion_1)
      
      to_pass_second = data %>%
        filter(!is.na(Portion_2))%>%
        select(Date,Portion_2)%>%
        rename(date=Date,level=Portion_2)
      
      to_pass_third = data %>%
        filter(!is.na(Portion_3))%>%
        select(Date,Portion_3)%>%
        rename(date=Date,level=Portion_3)
      
      
      predictions_first <- vaccine_forecast(to_pass_first,forecast_method,max_rate,prediction_dates)
      
      
      predictions_second <- vaccine_forecast(to_pass_second,forecast_method,max(predictions_first$fit),prediction_dates)
      
      predictions_third <- vaccine_forecast(to_pass_third,forecast_method,max(predictions_second$fit),prediction_dates)
      
      to_return <- add_row(to_return,
                           Date=prediction_dates,
                           Portion_1=predictions_first$fit,
                           Portion_1_lower=predictions_first$lower,
                           Portion_1_upper=predictions_first$upper,
                           Portion_2=predictions_second$fit,
                           Portion_2_lower=predictions_second$lower,
                           Portion_2_upper=predictions_second$upper,
                           Portion_3=predictions_third$fit,
                           Portion_3_lower=predictions_third$lower,
                           Portion_3_upper=predictions_third$upper
      )
      
      return(to_return)
    })
}
