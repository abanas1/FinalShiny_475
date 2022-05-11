# code by Abigail Banas

## Libraries to load: -----
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
# "C:\\Users\\abiba\\OneDrive\\Documents\\Spring 2022\\BAS 475 - Brian\\Final_App"
## ----

## Loading in Data ---
lung_data <- data.frame(LungDeaths = fdeaths, Date = seq(as.Date(as.yearmon("1974-01")), by = "month", length.out = 72))

lung_data <- as_tsibble(lung_data, index = Date)

lung_data %>% mutate(Date = yearmonth(Date)) -> lung_data
## ------





ui <- fluidPage(theme = shinytheme("cosmo"),
                
                useShinyjs(),
                
                h2(titlePanel("Analysis of Monthly Lung Disease Deaths in the UK")),
                
                navbarPage("",
                           
                           tabPanel(h4("App Instructions!"), fluid = TRUE,
                                    
                                    column(6,
                                           h3(p("Welcome to the Lung Disease Deaths Analysis App!")),
                                           br(),
                                           h4(p("This app is a collection of different graphs, descriptions, and features with data on monthly lung disease deaths 
                                           in the United Kingdom from January 1974 to December 1979. This app is equipped with a total of six tabs, 
                                           as you can see at the top of this page. The first tab, the one you are currently on, contains instructions 
                                                and helpful tips for navagating this app. ")),
                                           
                                           h4(p("The second tab, titled \"Full Time Series!\", contains an interactive plot of the entire time series for this 
                                                data. The tab also contains a date range option for looking at specific time periods in the series. The third tab, 
                                                titled \"Breakdowns!\" contains a drop down box with the option of four different ways to look at the series. Those 
                                                variations include the Seasonality, Autocorrelation, Difference, and the Decomposition 
                                                of the series. Descriptions of each can be found under this tab as well.")),
                                           
                                           h4(p("The fourth tab contains four simple models for forecasting the data twelve months into the future, which in this case 
                                                is the 1980's. Those simple models include the Naive Model, the Seasonal Naive Model, Mean Model, and the Drift 
                                                Model. Descriptions for those models can be found in the \"Simple Models!\" tab.")),
                                           
                                           h4(p("Tab number five is our Exponential Smoothing tab, where you will find two exponential smoothing models, along with 
                                                a description of each and a graph displaying the forecast of the data.")),
                                           
                                           h4(p("Lastly, tab number six, the \"ARIMA!\" tab gives you a description of the ARIMA model, and the option to 
                                                manually pick your parameters for the model. There is also an automatic option in that tab."))
                                    ),
                                    
                                    column(4,
                                           br(),
                                           br(),
                                           img(src = "https://i.pinimg.com/originals/d5/45/ca/d545ca87ab62504517ff68b30999e08e.gif"))
                           ), # app instructions END
                           
                           
                           tabPanel(h4("Full Time Series!"), fluid = TRUE,
                                    setSliderColor(c("#FF8200", "#FF8200", "#FF8200"), c(1,2,3)),
                                    sidebarLayout(
                                        sidebarPanel(
                                            
                                            sliderInput("rangeDate", label = h3("Select a Date Range"), min = as.Date(yearmonth("1974-01")), 
                                                        max = as.Date(yearmonth("1979-12")), 
                                                        value = c(as.Date(yearmonth("1974-01")), as.Date(yearmonth("1979-12"))), 
                                                        timeFormat = "19%y-%m",
                                                        ),
                                            hr(),
                                            h4(p("Above is a slider bar with the date ranges of January 1974 to December 1979. You are able to 
                                                 move the slider bar as you wish to focus in on any specific date ranges in the series.")),
                                            h4(p("The graph is also interactive, so feel free to click and drag and zoom for specific data points."))
                                            
                                        ),
                                        
                                        mainPanel(
                                            plotlyOutput("plot1")%>% withSpinner(color = "#FF8200"),
                                            
                                            hr(),
                                            h4(p("The time series plot above is the series of Lung Disease Deaths in the UK between the years 1974 and 1979. 
                                                 We are able to see a very strong seasonal trend that follows a pattern of decreasing into the middle of the year,
                                                 and increasing in the latter part of the year. We are able to note that this series is a stationary series, as it 
                                                 follows a non-linear trend and the plot remains homoskedastic, other than the strong obvious spike in early 1976."))
                                         )
                                    )
                           ), # full time series END
                           
                           tabPanel(h4("Breakdowns!"), fluid = TRUE,
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            
                                            selectInput("selectPlot2", label = h3("Select A Plot Type:"), 
                                                        choices = list("Seasonality" = "Seasonality", 
                                                                       "Autocorrelation" = "Autocorrelation",
                                                                       "Difference" = "Difference",
                                                                       "Decomposition" = "Decomposition"), 
                                                        selected = "Seasonality"),
                                            hr(),
                                            h4(p("Please use the drop down box to select a plot type to view. Below are descriptions of each option.")),
                                            br(),
                                            h4(p("Seasonality - Displays each season of data independently to help in identifying seasons when the 
                                                 pattern changes")),
                                            h4(p("Autocorrelation - Displays the linear relationship between lagged values in the time series")),
                                            h4(p("Difference - Displays a new time series consisting of the differences between consecutive observations")),
                                            h4(p("Decomposition - Displays the time series in individual components: a trend-cycle component, a seasonal 
                                                 component, and a remainder component "))
                                        ),
                                        
                                        mainPanel(
                                            plotOutput("yoPlots2")%>% withSpinner(color = "#FF8200"),
                                            
                                            hr(),
                                            h4(textOutput("yoTextTab2"))
                                            
                                        )
                                    )
                           ), # breakdowns END
                           
                           tabPanel(h4("Simple Models!"), fluid = TRUE,
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                        
                                            selectInput("selectPlot3", label = h3("Select A Model Type:"), 
                                                        choices = list("Naive" = "Naive", 
                                                                       "Seasonal Naive" = "Seasonal Naive",
                                                                       "Mean" = "Mean",
                                                                       "Drift" = "Drift"), 
                                                        selected = "Naive"),
                                            
                                            hr(),
                                            h4(p("Please use the drop down box to select a model type to view. Below are descriptions of each option.")),
                                            br(),
                                            h4(p("Naive - Sets all forecasts to be the value of the last observation")),
                                            h4(p("Seasonal Naive - Sets each forecast to be equal to the last observed value from the same season")),
                                            h4(p("Mean - Sets the forecasts of all future values are equal to the average of the historical data")),
                                            h4(p("Drift - The amount of change over time is set to be the average change seen in the historical data; 
                                                 the equivalent to drawing a line between the first and last observations"))
                                           
                                        ),
                                        
                                        mainPanel(
                                            fluidRow(
                                                splitLayout(cellWidths = c("60%", "40%"), 
                                                            plotOutput("yoPlots3")%>% withSpinner(color = "#FF8200"), 
                                                            plotOutput("yoPlots3v2")%>% withSpinner(color = "#FF8200"))
                                            ),
                                           
                                            hr()
                                        )
                                    )
                           ), # simple models END
                           
                           tabPanel(h4("Exponential Smoothing!"), fluid = TRUE,
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            
                                            selectInput("selectPlot4", label = h3("Select A Model Type"),
                                                         choices = list("Holt's" = "Holt's", "Holt's / Winter's" = "Holt's / Winter's"), 
                                                         selected = "Holt's"),
                                            
                                            hr(),
                                            h4(p("Please use the drop down box to select a model type to view. Below are descriptions of each option.")),
                                            br(),
                                            h4(p("Holt's - Displays a constant linear trend into the future")),
                                            h4(p("Holt's / Winter's - Expands on Holt's methods, but takes seasonality into consideration "))
                                            
                                        ),
                                        
                                        mainPanel(
                                            fluidRow(
                                                splitLayout(cellWidths = c("60%", "40%"),
                                                            plotOutput("yoPlots4")%>% withSpinner(color = "#FF8200"), 
                                                            plotOutput("yoPlots4v2")%>% withSpinner(color = "#FF8200"))
                                            ),
                                            
                                            hr()
                                        )
                                    )
                           ), # exponential smoothing END
                           
                           tabPanel(h4("ARIMA!"), fluid = TRUE,
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            
                                            selectInput("selectPlot5", label = h3("Select A Model Creation Option:"), 
                                                        choices = list("Manually Select Parameters" = "Manually Select Parameters", 
                                                                       "Auto Select Parameters" = "Auto Select Parameters"), 
                                                        selected = "Manually Select Parameters"),
                                            
                                            h4(p("Please use the drop down box to select an input option for the ARIMA model. 
                                                 Below is a description of the manual parameters.")),
                                            hr(),
                                            
                                            sliderInput("sliderARIMA1", label = h3("Slider Input for ARIMA Model (P)"), min = 0, 
                                                        max = 2, value = 0),
                                            sliderInput("sliderARIMA2", label = h3("Slider Input for ARIMA Model (Q)"), min = 0, 
                                                        max = 2, value = 1),
                                            
                                            hr(),
                                            h4(p("ARIMA models aim to describe the autocorrelations within the data by accounting for several components of 
                                                 the series. ARIMA stands for \"autoregressive integrated moving average\".")),
                                            h4(p("This app uses a seasonal model for graphing the series, so the model contains ARIMA(p,d,q)(P,D,Q).
                                            The uppercase portion of that formula is what you get to choose to input, hence the P and Q slider bars. The P 
                                                 influences the number of lag observations used, and the Q is the way we are using the moving average of the 
                                                 series.")),
                                            h4(p("Feel free to play with the slider bars and watch how different ARIMA models forecast the next twelve months 
                                                 of this series."))
                                           
                                        ),
                                        
                                        mainPanel(
                                            fluidRow(
                                                splitLayout(cellWidths = c("60%", "40%"), 
                                                            plotOutput("yoPlots5")%>% withSpinner(color = "#FF8200"), 
                                                            plotOutput("yoPlots5v2")%>% withSpinner(color = "#FF8200")) 
                                            ),
                                            
                                            hr()
                                        )
                                    )
                           ), # arima END
                           
                ) # navbar page END
) # fluid page END


server <- function(input, output) {

    output$value <- renderPrint({ input$rangeDate })
    
    output$dropDown2 <- renderPrint({ input$selectPlot2 })
    
    output$dropDown3 <- renderPrint({ input$selectPlot3 })
    
    output$dropDown4 <- renderPrint({ input$selectPlot4 })
    
    output$slider1 <- renderPrint({ input$sliderARIMA1 })
    
    output$slider2 <- renderPrint({ input$sliderARIMA2 })
    
    output$dropDown5 <- renderPrint({ input$selectPlot5 })
    
    
    # Full Time Series ---
    output$plot1 <- renderPlotly({
        
        date_start <- input$rangeDate[1]
        date_end <- input$rangeDate[2]
        
        lung_data %>% filter(Date >= date_start & Date <= date_end) %>% autoplot(LungDeaths)  + 
            labs(title = "Interactive Plot of Lung Death Trends in the UK:", y = "Number of Deaths", x = "Date") + 
            easy_center_title() + theme_igray() 
    })
    
    
    # Breakdowns ---
    plotsTab2 <- reactive({
        
        date_start <- input$rangeDate[1]
        date_end <- input$rangeDate[2]
        
        if(input$selectPlot2 == "Seasonality") {
            x <- lung_data %>% filter(Date >= date_start & Date <= date_end) %>% gg_season(LungDeaths) + 
                labs(title = paste("Seasonality Plot of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        if(input$selectPlot2 == "Autocorrelation") {
            x <- lung_data %>% filter(Date >= date_start & Date <= date_end) %>% ACF(LungDeaths) %>% autoplot() +
                labs(title = paste("Autocorrelation Plot of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        if(input$selectPlot2 == "Difference") {
            x <- lung_data %>% mutate(diff = difference(LungDeaths)) %>% filter(Date >= date_start & Date <= date_end) %>% autoplot(diff) +
                labs(title = paste("Difference Plot of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        if(input$selectPlot2 == "Decomposition") {
            x <- lung_data %>% filter(Date >= date_start & Date <= date_end) %>% 
                model(classical_decomposition(LungDeaths, type = "additive")) %>% 
                components()  %>% autoplot() +
                labs(title = paste("Decomposition Plot of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        
        return(x)

    })
    
    output$yoPlots2 <- renderPlot({
        plotsTab2()
    })
    
    funTextTab2 <- reactive({
        if(input$selectPlot2 == "Seasonality" ) { text <- " This seasonality plot of the series shows a downward trend in all seasons through the months 
        of March to September. There is a constant pattern seen in the months of June to August in all years other than the year 1976. In the year 1976 
        we also see a sharp spike in the month of February, which does not follow the seasonal trend of the other years."}
        
        if(input$selectPlot2 == "Autocorrelation" ) { text <- "The prominent scallop shape of this ACF plot indicates there is strong correlation in the 
        seasonality of the series. The positive mark seen at lag 1 confirms this series is stationary."}
        
        if(input$selectPlot2 == "Difference" ) { text <- "This plot of the differenced values shows a seasonal pattern in the last two seasons of the series. 
        We can see the same irregular spike in 1976, as that is the largest difference on the plot."}
        
        if(input$selectPlot2 == "Decomposition" ) { text <- "Additive decomposition was done on this series due to its stationary characteristics. From that 
        decomposition, we are able to see that the seasonal component of the series carries the strongest weight. The overall trend itself has little influence 
        on the series, as it has the largest box on the decomposition plot. The remainder uncharacteristically carries a bit of influence on the series, as we 
        can note that the scale box is close in size to the seasonal box."}
        
        return(text)
    })
    
    output$yoTextTab2 <- renderText({
        funTextTab2()
    })
    
    
    # Simple Models ---
    plotsTab3 <- reactive({

        if(input$selectPlot3 == "Naive") {
            p <- lung_data %>% model(NAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot(lung_data) + 
                labs(title = paste("Naive Forecast Model of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        if(input$selectPlot3 == "Seasonal Naive") {
            p <- lung_data %>% model(SNAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot(lung_data) +
                labs(title = paste("Seasonal Naive Forecast Model of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        if(input$selectPlot3 == "Mean") {
            p <- lung_data %>% model(MEAN(LungDeaths)) %>% forecast(h = 12) %>% autoplot(lung_data) +
                labs(title = paste("Mean Forecast Model of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        if(input$selectPlot3 == "Drift") {
            p <- lung_data %>% model(RW(LungDeaths ~ drift())) %>% forecast(h = 12) %>% autoplot(lung_data) +
                labs(title = paste("Drift Forecast Model of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        return(p)
        
    })
    
    plotsTab3v2 <- reactive({

    if(input$selectPlot3 == "Naive") {
        p2 <- lung_data %>% model(NAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot() +
            labs(y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
    }
    if(input$selectPlot3 == "Seasonal Naive") {
        p2 <- lung_data %>% model(SNAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot() +
            labs(y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
    }
    if(input$selectPlot3 == "Mean") {
        p2 <- lung_data %>% model(MEAN(LungDeaths)) %>% forecast(h = 12) %>% autoplot() +
            labs(y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
    }
    if(input$selectPlot3 == "Drift") {
        p2 <- lung_data %>% model(RW(LungDeaths ~ drift())) %>% forecast(h = 12) %>% autoplot() +
            labs(y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
    }

    return(p2)

    })
    
    output$yoPlots3 <- renderPlot({
            plotsTab3()
    })
    
    output$yoPlots3v2 <- renderPlot({
        plotsTab3v2()
    })
    
    
    # Exponential Smoothing ---
    plotsTab4 <- reactive({
        
        if(input$selectPlot4 == "Holt's") {
            m <- lung_data %>% model(NAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot(lung_data) + 
                labs(title = paste("Holt's Forecast Model of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        if(input$selectPlot4 == "Holt's / Winter's") {
            m <- lung_data %>% model(SNAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot(lung_data) +
                labs(title = paste("Holt's / Winter's Forecast Model of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }

        return(m)
        
    })
    
    plotsTab4v2 <- reactive({
        
        if(input$selectPlot4 == "Holt's") {
            m2 <- lung_data %>% model(NAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot() + 
                labs(y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        if(input$selectPlot4 == "Holt's / Winter's") {
            m2 <- lung_data %>% model(SNAIVE(LungDeaths)) %>% forecast(h = 12) %>% autoplot() +
                labs(y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        
        return(m2)
        
    })
    
    output$yoPlots4 <- renderPlot({
        plotsTab4()
    })
    
    output$yoPlots4v2 <- renderPlot({
        plotsTab4v2()
    })

    
    # ARIMA ---
    plotsTab5 <- reactive({
        
        if(input$selectPlot5 == "Manually Select Parameters") {
            f <- lung_data %>%   model(
                arima210011 = ARIMA(LungDeaths ~ pdq(2,0,0) + PDQ( input$sliderARIMA1,0,input$sliderARIMA2))) %>% forecast(h = 12) %>% 
                autoplot(lung_data) + 
                labs(title = paste("ARIMA Forecast Model of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        if(input$selectPlot5 == "Auto Select Parameters") {
            f <- lung_data %>% model(ARIMA(LungDeaths, stepwise = TRUE, approx = FALSE)) %>% forecast(h = 12) %>% autoplot(lung_data) +
                labs(title = paste("Auto Selected ARIMA Model of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
            # for some reason has a difficult time loading in the shiny app, but will run properly outside of the app.
        }
        
        return(f)
        
    })
    
    plotsTab5v2 <- reactive({
        
        if(input$selectPlot5 == "Manually Select Parameters") {
            f2 <- lung_data %>%   model(
                arima210011 = ARIMA(LungDeaths ~ pdq(2,0,0) + PDQ( input$sliderARIMA1,0,input$sliderARIMA2))) %>% forecast(h = 12) %>% 
                autoplot() + 
                labs(title = paste("ARIMA Forecast Model of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
        }
        if(input$selectPlot5 == "Auto Select Parameters") {
            f2 <- lung_data %>% model(ARIMA(LungDeaths, stepwise = TRUE, approx = FALSE)) %>% forecast(h = 12) %>% autoplot() +
                labs(title = paste("Auto Selected ARIMA Model of Lung Death Data"), y = "Deaths", x = "Month") + easy_center_title() + theme_igray()
            # for some reason has a difficult time loading in the shiny app, but will run properly outside of the app.
            
        }
        
        return(f2)
        
    })
    
    output$yoPlots5 <- renderPlot({
        plotsTab5()
    })
    
    output$yoPlots5v2 <- renderPlot({
        plotsTab5v2()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)








