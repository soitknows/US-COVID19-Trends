library(shiny)
library(plotly)


case_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
csv <- read.csv(case_url)
beg <- as.Date(colnames(csv)[13], format = "X%m.%d.%y")
end <- as.Date(colnames(csv)[length(colnames(csv))], format = "X%m.%d.%y")

pop <- read.csv("co-est2019-alldata.csv")
pop <- data.frame(pop$STNAME, pop$CTYNAME, pop$POPESTIMATE2019)
pop$pop.CTYNAME <-  as.character(lapply(pop$pop.CTYNAME, function(i) { sub(" County", "", i)}))
colnames(pop) <- c("ST", "CTY", "POP")

rollingAvg <- function(data){
  ra <- c()
  l <- length(data)
  for(i in c(1:l)){
    if(i > 3 && i < l - 3){
      ra[i] <- mean(c(data[i-3], data[i-2], data[i-1], data[i], data[i+1], data[i+2], data[i+3]))
    } else {
      ra[i] <- 0
    }
  }
  ra
}

newCases <- function(data){
  na <- c()
  l <- length(data)
  for(i in c(1:l)){
    if(i > 1){
      na[i] <- data[i] - data[i-1]
    } else {
      na[i] <- data[i]
    }
  }
  na
}

getData <- function (state1, county1, state2, county2, mode) {
  date <- seq(beg, end, by="days")
  cases1 <- as.numeric(unname(csv[( csv$Admin2 ==  county1 & csv$Province_State == state1), ][,13:ncol(csv)]))
  cases2 <- as.numeric(unname(csv[( csv$Admin2 ==  county2 & csv$Province_State == state2), ][,13:ncol(csv)]))
  population1 <- ifelse(is.na(cases1[1]),cases1,pop[pop$ST == state1 & pop$CTY == county1,]$POP)
  population2 <- ifelse(is.na(cases2[1]),cases2,pop[pop$ST == state2 & pop$CTY == county2,]$POP)
  avg1 <- cases1 / (population1 / 100000)
  avg2 <- cases2 / (population2 / 100000)
  rac1 <- rollingAvg(cases1)
  rac2 <- rollingAvg(cases2)
  raa1 <- rac1 / (population1 / 100000)
  raa2 <- rac2 / (population2 / 100000)
  nac1 <- newCases(cases1)
  nac2 <- newCases(cases2)
  naa1 <- nac1 / (population1 / 100000)
  naa2 <- nac2 / (population2 / 100000)
  df <- data.frame(date
                   , county1, population1, cases1, avg1, rac1, raa1, nac1, naa1
                   , county2, population2, cases2, avg2, rac2, raa2, nac2, naa2)
  df <- df[df$date >= '2020-03-01',]
  df$date <- as.character((df$date))
  colnames(df) <- c("Date"
                    , "County1", "Population1", "Cases1", "Per100K1", "RAC1", "RAA1", "NAC1", "NAA1"
                    , "County2", "Population2", "Cases2", "Per100K2", "RAC2", "RAA2", "NAC2", "NAA2")
  df
}


function (input, output, session) {

  compare <- reactive({
    data <- getData(input$state1Compare, input$county1Compare, input$state2Compare, input$county2Compare, mode = "compare")
  })

  
  output$plotCompare <- renderPlotly({
    if (input$outputCompare == "Cases") {
      plot_ly(compare(), x= ~as.Date(Date), y = ~Cases1, type='scatter', mode = 'lines', name = input$county1Compare, line = list(color = '#3db1ff', width = 4)) %>%
        add_trace(y = ~Cases2, name = input$county2Compare, line = list(color = '#ffb649', width = 4)) %>% 
        layout(title = "", xaxis = list(title="DATE"), yaxis = list(title="TOTAL CASES"))
    } else {
      plot_ly(compare(), x= ~as.Date(Date), y = ~Per100K1, type='scatter', mode = 'lines', name = input$county1Compare, line = list(color = '#3db1ff', width = 4)) %>%
        add_trace(y = ~Per100K2, name = input$county2Compare, line = list(color = '#ffb649', width = 4)) %>% 
        layout(title = "", xaxis = list(title="DATE"), yaxis = list(title="CASES / POPULATION"))
    }
  })
  
  output$plotNewCases <- renderPlotly({
    if (input$outputCompare == "Cases") {
      plot_ly(compare(), x= ~as.Date(Date), y = ~NAC1, type='scatter', mode = 'lines', name = input$county1Compare, line = list(color = '#3db1ff', width = 4)) %>%
        add_trace(y = ~NAC2, name = input$county2Compare, line = list(color = '#ffb649', width = 4)) %>%
        layout(title = "", xaxis = list(title="DATE"), yaxis = list(title="NEW CASES"))
    } else {
      plot_ly(compare(), x= ~as.Date(Date), y = ~NAA1, type='scatter', mode = 'lines', name = input$county1Compare, line = list(color = '#3db1ff', width = 4)) %>%
        add_trace(y = ~NAA2, name = input$county2Compare, line = list(color = '#ffb649', width = 4)) %>%
        layout(title = "", xaxis = list(title="DATE"), yaxis = list(title="NEW CASES /POPULATION"))
    }
  })
  
  fluidRow(
    column(4,output$dataCompare1 <- renderTable(tail(compare(), n = 7)))
  )
  
  
  observe({
    counties2 <- csv[(csv$Country_Region == "US" & csv$Province_State == input$state1Compare),]$Admin2
    updateSelectInput(session, "county1Compare", choices = counties2)
  })
  
  observe({
    counties3 <- csv[(csv$Country_Region == "US" & csv$Province_State == input$state2Compare),]$Admin2
    updateSelectInput(session, "county2Compare", choices = counties3)
  })
}