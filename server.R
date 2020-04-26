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


getData <- function (state1, county1, state2, county2, mode) {
  date <- seq(beg, end, by="days")
  cases1 <- as.numeric(unname(csv[( csv$Admin2 ==  county1 & csv$Province_State == state1), ][,13:ncol(csv)]))
  cases2 <- as.numeric(unname(csv[( csv$Admin2 ==  county2 & csv$Province_State == state2), ][,13:ncol(csv)]))
  population1 <- ifelse(is.na(cases1[1]),cases1,pop[pop$ST == state1 & pop$CTY == county1,]$POP)
  population2 <- ifelse(is.na(cases2[1]),cases2,pop[pop$ST == state2 & pop$CTY == county2,]$POP)
  avg1 <- cases1 / (population1 / 100000)
  avg2 <- cases2 / (population2 / 100000)
  df <- data.frame(date, cases1, population1, avg1, county1, cases2, population2, avg2, county2)
  df <- df[df$date >= '2020-03-01',]
  colnames(df) <- c("Date", "Cases1", "Population1", "Per100K1", "County1", "Cases2", "Population2", "Per100K2", "County2")
  df
}


function (input, output, session) {

  compare <- reactive({
    data <- getData(input$state1Compare, input$county1Compare, input$state2Compare, input$county2Compare, mode = "compare")
  })

  
  output$plotCompare<- renderPlotly({
    if (input$outputCompare == "Cases") {
      plot_ly(compare(), x= ~as.Date(Date), y = ~Cases1, type='scatter', mode = 'lines', name = input$county1Compare, line = list(color = '#3db1ff', width = 4)) %>%
        add_trace(y = ~Cases2, name = input$county2Compare, line = list(color = '#ffb649', width = 4)) %>% 
        layout(title = "", xaxis = list(title="DATE"), yaxis = list(title="CASES"))
    } else {
      plot_ly(compare(), x= ~as.Date(Date), y = ~Per100K1, type='scatter', mode = 'lines', name = input$county1Compare, line = list(color = '#3db1ff', width = 4)) %>%
        add_trace(y = ~Per100K2, name = input$county2Compare, line = list(color = '#ffb649', width = 4)) %>% 
        layout(title = "", xaxis = list(title="DATE"), yaxis = list(title="CASES / POPULATION"))
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