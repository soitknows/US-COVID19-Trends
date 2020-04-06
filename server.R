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
  if (mode == "single"){
    x <- seq(beg, end, by="days")
    y1 <- as.numeric(unname(csv[( csv$Admin2 ==  county1 & csv$Province_State == state1), ][,13:ncol(csv)]))
    p1 <- ifelse(is.na(y1[1]),y1,pop[pop$ST == state1 & pop$CTY == county1,]$POP)
    a1 <- y1 / p1 * 100
    df <- data.frame(x,y1,p1,a1)
    df
  } else {
    x <- seq(beg, end, by="days")
    y1 <- as.numeric(unname(csv[( csv$Admin2 ==  county1 & csv$Province_State == state1), ][,13:ncol(csv)]))
    y2 <- as.numeric(unname(csv[( csv$Admin2 ==  county2 & csv$Province_State == state2), ][,13:ncol(csv)]))
    p1 <- ifelse(is.na(y1[1]),y1,pop[pop$ST == state1 & pop$CTY == county1,]$POP)
    p2 <- ifelse(is.na(y2[1]),y2,pop[pop$ST == state2 & pop$CTY == county2,]$POP)
    a1 <- y1 / p1 * 100
    a2 <- y2 / p2 * 100
    df <- data.frame(x,y1,p1,a1,y2,p2,a2)
    df
  }
}


function (input, output, session) {

  single <- reactive({
    data <- getData(input$state1, input$county1, mode = "single")
  })
  
  compare <- reactive({
    data <- getData(input$state2, input$county2, input$state3, input$county3, mode = "compare")
  })

  output$single_plot<- renderPlotly({
      plot_ly(single(), x= ~x, y = ~y1, type='scatter', mode = 'lines', name = input$county1) %>%
        layout(title = "Single County Cases", xaxis = list(title="DATE"), yaxis = list(title="CASES"))
  })
  
  output$compare_plot<- renderPlotly({
    if (input$output == "Cases") {
      plot_ly(compare(), x= ~x, y = ~y1, type='scatter', mode = 'lines', name = input$county2) %>%
        add_trace(y = ~y2, name = input$county3) %>% 
        layout(title = "Case Comparison", xaxis = list(title="DATE"), yaxis = list(title="CASES"))
    } else {
      plot_ly(compare(), x= ~x, y = ~a1, type='scatter', mode = 'lines', name = input$county2) %>%
        add_trace(y = ~a2, name = input$county3) %>% 
        layout(title = "Case / Population Comparison", xaxis = list(title="DATE"), yaxis = list(title="CASES / POPULATION"))
    }
  })
  
  observe({
    counties1 <- csv[(csv$Country_Region == "US" & csv$Province_State == input$state1),]$Admin2
    counties2 <- csv[(csv$Country_Region == "US" & csv$Province_State == input$state2),]$Admin2
    counties3 <- csv[(csv$Country_Region == "US" & csv$Province_State == input$state3),]$Admin2
    updateSelectInput(session, "county1", choices = counties1)
    updateSelectInput(session, "county2", choices = counties2)
    updateSelectInput(session, "county3", choices = counties3)
  })
}