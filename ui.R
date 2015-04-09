library(rjson)
library(shiny)
library(shinythemes)
json_data <- fromJSON(file="places.json")
places = as.data.frame(json_data)
shinyUI(
  fluidPage(theme = shinytheme("flatly"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "sh-default.css")
      ),
    h1("Just Dial Extraction"),
    sidebarLayout(
      sidebarPanel(
        selectInput("city",selected = "coimbatore",
                    "Please select the city",
                    choices =levels(places$lazyLoadCities) ),
        textInput("search",
                  label = "What do you want to see?",
                  value = "Colleges"),
        numericInput("numpages",
                     "Pages",
                     2),
        actionButton("goButton", "Go!"),
        downloadLink('downloadData', 'Download')
      ),
      mainPanel(
        dataTableOutput("jdtable")
      )
    )
  ))
