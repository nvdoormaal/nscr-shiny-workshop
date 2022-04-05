#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Load packages
## install.packages("gsheet")
library(shiny)
library(tidyverse)
library(gsheet)
library(lubridate)

# Create species list to be used later
SpeciesList <- c("Elephant", "Buffalo", "Leopard", "Lion", "Rhino")

# Define UI for our application
ui <- fluidPage(

    # Application title
    titlePanel("Camera trap data exploration"),

    # Sidebar with a slider input for selecting time interval (in secs) 
    sidebarLayout(
        sidebarPanel(
           sliderInput(
             inputId = "tdiff", label = "Time difference:", ticks = TRUE,
             min = 0, max = 120, step = 5, value = 60, post = "min"
           ),
           selectInput(
              inputId = "species", label = "Species", choices = SpeciesList,
              selected = "Elephant"
           ),
           actionButton(inputId = "show", label = "Show me!")
        ),

        ## Define where we want to put our outputs
        mainPanel(
          textOutput("speciesSum"),
          hr(),
          plotOutput("activityPlot")
        )
    )
)

# Define server logic required to create text output and plots
server <- function(input, output) {

## Load the data from Sheets  
  AllData <- gsheet::gsheet2tbl(url = "https://docs.google.com/spreadsheets/d/1Fm1Y_ooTrSdGTk23JEEO0JQw7sv2GkhrvXvaa9fy-kU/edit?usp=sharing") %>%
    mutate(
      Species = case_when(
        str_detect(string = Species, pattern = "Rhino") ~ "Rhino",
        TRUE ~ Species
      )
      )

## Create subset based on input variables
  selected_data <- eventReactive(input$show, { 
    AllData %>%
      filter(Species == input$species) %>%
      arrange(FirstPhoto) %>%
      mutate(
        tdiff = FirstPhoto - lag(LastPhoto)
      ) %>%
      filter(tdiff >= input$tdiff)
  })

## Extract the hours to be used later in the activity plot
  hour_data <- eventReactive(input$show, {
    selected_data() %>%
      mutate(
        Hour = hour(FirstPhoto)
    )
  })

  ## Text output
  output$speciesSum <- renderText({
    paste("The number of independent", input$species, "events is",
          nrow(selected_data()))
  })

  ## Activity plot output
    output$activityPlot <- renderPlot({
      hour_data() %>%
        ggplot() +
        geom_density(aes(x = Hour, fill = Species),
                     alpha = 0.7, show.legend = FALSE, size = 1.1) +
        labs(title = paste0("Daily activity pattern of ", input$species)) +
        theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
