library(shiny)
library(plotly)

case_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
states <- read.csv(case_url)
source <- a("COVID-19 Data Repository by Johns Hopkins CSSE", href="https://github.com/CSSEGISandData/COVID-19")
tags <- tagList("DATA SOURCE:  ", source)


ui <- fluidPage(
  
    theme= "bootstrap.css",
  
    titlePanel("U.S. COVID-19 Trends"),
    
    
    sidebarLayout(
      sidebarPanel(width = 4,
                   
        radioButtons(inputId = "outputCompare",
                     label = "Output",
                     choices = c("Cases", "Cases per 100k people")),
        
        hr(),
        
        selectInput(inputId = "state1Compare",
                    label = "State 1",
                    choices = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State,
                    selected = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State[1]),
        selectInput(inputId = "county1Compare",
                    label = "County 1",
                    choices = c()),
        
        hr(),
        
        selectInput(inputId = "state2Compare",
                    label = "State 2",
                    choices = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State,
                    selected = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State[1]),
        selectInput(inputId = "county2Compare",
                    label = "County 2",
                    choices = c())
        
      ),
      
      
      mainPanel(width = 8,
                plotlyOutput(outputId = "plotCompare"),
                hr(),
                tableOutput("dataCompare1")
      )
    ),
    
    
    hr(),
    helpText(tags)
    
)