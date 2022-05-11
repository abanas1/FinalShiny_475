## FINAL SHINY WORK

library(shiny)
library(shinyjs)
library(quantmod)
library(plotly)
library(fpp3)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(ggeasy)
library(tidyquant)
library(ggthemes)
library(shinythemes)
library(datasets)
library(tidyr)
library(tidyverse)
library(reshape2)
library(zoo)
library(tsibble)
library(gridExtra)
library(shinyWidgets)
library(shinycssloaders)
library(rsconnect)
# "C:\\Users\\abiba\\OneDrive\\Documents\\Spring 2022\\BAS 475 - Brian\\Final_App"

# make a github repo for everything
# deploy on shinyapps.io

# instruction page for all users
# date ranges for user selection
# interactive ggplotlys
# tabs for each type of graph requirement 
# tab for full time series(1), midterm graphs(3), simple models(4), exponential smoothing(2), ARIMA(2)




# library shiny css loaders
# withspinner(color) piped through



## making the data a tsibble ----

lung_data <- data.frame(LungDeaths = fdeaths, Date = seq(as.Date(as.yearmon("1974-01")), by = "month", length.out = 72))

lung_data <- as_tsibble(lung_data, index = Date)

lung_data %>% mutate(Date = yearmonth(Date)) -> lung_data


# graphing full time series -----
lung_data %>% autoplot() %>% ggplotly()

autoplot(lung_data) +
  labs(title = "Plot of Lung Death Trends:", y = "Number of Deaths", x = "Date") + 
  easy_center_title() + theme_igray()


# graphing simple models -------------

lung_data %>% model(NAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot()
lung_data %>% model(NAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot(lung_data)

lung_data %>% model(SNAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot()
lung_data %>% model(SNAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot(lung_data)

lung_data %>% model(MEAN(LungDeaths)) %>% forecast(h = 12) %>% autoplot()
lung_data %>% model(MEAN(LungDeaths)) %>% forecast(h = 12) %>% autoplot(lung_data)

lung_data %>% model(RW(LungDeaths ~ drift())) %>% forecast(h = 12) %>% autoplot()
lung_data %>% model(RW(LungDeaths ~ drift())) %>% forecast(h = 12) %>% autoplot(lung_data)

# graphing exponential smoothing ----------

lung_data %>% model(ETS(LungDeaths ~ error("A") + trend("A") + season("N"))) %>% forecast(h = 12) %>% autoplot()
lung_data %>% model(ETS(LungDeaths ~ error("A") + trend("A") + season("N"))) %>% forecast(h = 12) %>% autoplot(lung_data)

lung_data %>% model(additive = ETS(LungDeaths ~ error("A") + trend("A") + season("A"))) %>% forecast(h = 12) %>% autoplot()
lung_data %>% model(additive = ETS(LungDeaths ~ error("A") + trend("A") + season("A"))) %>% forecast(h = 12) %>% autoplot(lung_data)

# graphing ARIMA ------------------------

#AUTO ARIMA
lung_data %>% model(ARIMA(LungDeaths, stepwise = TRUE, approx = FALSE)) %>% forecast(h = 12) %>% autoplot()
lung_data %>% model(ARIMA(LungDeaths, stepwise = TRUE, approx = FALSE)) %>% forecast(h = 12) %>% autoplot(lung_data)


#ARIMA
lung_data %>%   model(
  arima210011 = ARIMA(LungDeaths ~ pdq(2,0,0) + PDQ(0,0,1))
  ) %>% forecast(h = 12) %>% autoplot(lung_data)


lung_data %>%   model(ARIMA(LungDeaths ~ pdq(0,0,2) + PDQ(0,0,1))) %>% forecast(h = 12) %>% autoplot()







