# GOV 50 Final Project Shiny App

# Load libraries

library(shiny)
library(shinythemes)
library(tidyverse)

# Load data

wb_wellness_data <- readRDS("data/wb_wellness_test.rds")

# Define UI for application that draws a histogram
# Define UI
ui <- navbarPage(
    "Title",
    
    # Creating Model tab which will allow user to select a year from 2006-2017 
    # as an input and will display a scatter plot based on selection.
    
    tabPanel("Model",
             fluidPage(theme = shinytheme("cerulean"),
                       titlePanel("Model"),
                       sidebarLayout(
                           sidebarPanel(
                               helpText("Choose an indicator."),
                               selectInput(
                                   "indicator",
                                   label = "indicator",
                                   choices = c("interfere_home", "long_hours"),
                                   selected = "interfere_home"
                               )),
                           mainPanel(plotOutput("scatterPlot")))
             )),
    
    # Creating a "Discussion" tab (where I will discuss modeling choices later).
    
    tabPanel("Discussion",
             titlePanel("Discussion"),
             p("This is where I will give a tour of the modeling choices I made 
             and an explanation of why I made them.")),
    
    # Creating an "About" tab (for project background and personal info)
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("For my project, I am collaborating with the Harvard 
             School of Public Health's SHINE Initiative (Sustainability and 
               Health Initiative for Net Positive Enterprise). I am planning the
               SHINE team with analyzing a new dataset on World Bank employees globally
               and different measures of mental well-being."),
             h3("Github Repo"),
             p("My Github repository can be found here: 
               https://github.com/ciaraduggan/wb-wellness"),
             h3("About Me"),
             p("My name is Ciara Duggan. I am a senior at Harvard College 
             concentrating in Social Studies with a secondary field in Global 
             Health and Health Policy. 
             You can reach me at ciaraduggan@college.harvard.edu.")))

# Define server logic 

server <- function(input, output) {
    output$scatterPlot <- renderPlot({ 
        
        # This code tells Shiny to generate a scatter plot (using the opioid 
        # prescription values and drug-related death values for each county
        # for given year) based on the input year which the user selects.
        
        wb_wellness_data %>%
            ggplot(aes_string(x = input$indicator, y = "satisfaction")) +
            geom_point(alpha = 0.1) +
            geom_smooth(method = lm) +
            labs(title = "title", 
                 x = "x label",
                 y = "y label")
    })
}

# Run the application 

shinyApp(ui = ui, server = server)
