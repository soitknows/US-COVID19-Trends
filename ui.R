library(shiny)
library(plotly)

case_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
states <- read.csv(case_url)
source <- a("COVID-19 Data Repository by Johns Hopkins CSSE", href="https://github.com/CSSEGISandData/COVID-19")
tags <- tagList("DATA SOURCE:  ", source)


ui <- fluidPage(
  
    titlePanel("U.S. COVID-19 Trends"),
    helpText("Single County"),
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput(inputId = "state1",
                    label = "State",
                    choices = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State,
                    selected = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State[1]),
        selectInput(inputId = "county1",
                    label = "County",
                    choices = NULL)
      ),
      mainPanel(width = 9,
        plotlyOutput(outputId = "single_plot")
      )
    ),
    
    helpText("County Comparison"),
    sidebarLayout(
      sidebarPanel(width = 3,
        radioButtons(inputId = "output",
                     label = "Output",
                     choices = c("Cases", "Cases / Population")),
        selectInput(inputId = "state2",
                    label = "State",
                    choices = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State,
                    selected = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State[1]),
        selectInput(inputId = "county2",
                    label = "County",
                    choices = NULL),
        selectInput(inputId = "state3",
                    label = "State",
                    choices = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State,
                    selected = states[states$Country_Region == "US" & states$Admin2 != "",]$Province_State[1]),
        selectInput(inputId = "county3",
                    label = "County",
                    choices = NULL)
      ),
      mainPanel(width = 9,
                plotlyOutput(outputId = "compare_plot")
      )
    ),
    
    helpText(tags)
)