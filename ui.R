library(shiny)
library(plotly)

case_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
states <- read.csv(case_url)
source <- a("COVID-19 Data Repository by Johns Hopkins CSSE", href="https://github.com/CSSEGISandData/COVID-19")
tags <- tagList("DATA SOURCE:  ", source)



ui <- fluidPage(
  
    titlePanel("U.S. COVID-19 Trends"),
    sidebarLayout(
      sidebarPanel(width = 3,
        radioButtons(inputId = "mode",
                    label = "Mode",
                    choices = c("Single County", "County Comparison")),          
        radioButtons(inputId = "output",
                    label = "Output",
                    choices = c("Cases", "Cases / Population")),
        selectInput(inputId = "state1",
                    label = "State",
                    choices = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State,
                    selected = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State[1]),
        selectInput(inputId = "county1",
                    label = "County",
                    choices = NULL),
        
        conditionalPanel(condition = "input.mode == 'County Comparison'",
                         selectInput(inputId = "state2",
                                     label = "State",
                                     choices = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State,
                                     selected = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State[1]),
                         selectInput(inputId = "county2",
                                     label = "County",
                                     choices = NULL))
        
      ),
      
      
    
      
      mainPanel(width = 9,
        plotlyOutput(outputId = "stats")
      )
    ),
    helpText(tags)
)