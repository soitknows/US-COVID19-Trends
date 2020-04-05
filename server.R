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


getData <- function (state1, county1, state2, county2) {
  x <- seq(beg, end, by="days")
  y1 <- as.numeric(unname(csv[( csv$Admin2 ==  county1 & csv$Province_State == state1), ][,13:ncol(csv)]))
  y2 <- as.numeric(unname(csv[( csv$Admin2 ==  county2 & csv$Province_State == state2), ][,13:ncol(csv)]))
  p1 <- ifelse(is.na(y1[1]),y1,pop[pop$ST == state1 & pop$CTY == county1,]$POP)
  p2 <- ifelse(is.na(y2[1]),y2,pop[pop$ST == state2 & pop$CTY == county2,]$POP)
  a1 <- y1 / p1 * 100
  a2 <- y2 / p2 * 100
  df <- data.frame(x,y1,p1,a1,y2,p2,a2)
  print(df)
  df
}


function (input, output, session) {

  df <- reactive({
    data <- getData(input$state1, input$county1, input$state2, input$county2)
  })

  output$stats<- renderPlotly({
    if (input$output == "Cases"){
      plot_ly(df(), x= ~x, y = ~y1, type='scatter', mode = 'lines', name = input$county1) %>%
        add_trace(y = ~y2, name = input$county2) %>% 
        layout(xaxis = list(title="DATE"), yaxis = list(title="CASES"))
    } else {
      plot_ly(df(), x= ~x, y = ~a1, type='scatter', mode = 'lines', name = input$county1) %>%
        add_trace(y = ~a2, name = input$county2) %>% 
        layout(xaxis = list(title="DATE"), yaxis = list(title="CASES / POPULATION"))
    }
    
  })
  
  observe({
    counties1 <- csv[(csv$Country_Region == "US" & csv$Province_State == input$state1),]$Admin2
    counties2 <- csv[(csv$Country_Region == "US" & csv$Province_State == input$state2),]$Admin2
    updateSelectInput(session, "county1", choices = counties1)
    updateSelectInput(session, "county2", choices = counties2)
  })
}