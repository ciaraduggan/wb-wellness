
# World Bank Group Wellness Analysis Shiny App (Author: Ciara Duggan)

# Load libraries

library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(gt)

# Loading data

# wb_wellness_data <- readRDS("./data/wb_wellness.rds")
simple_satisfaction_model <- readRDS("./data/simple_satisfaction_model.rds")
simple_stressed_model_model <- readRDS("./data/simple_stressed_model.rds")

# Define UI
ui <- navbarPage(
    theme = shinytheme("cosmo"),
    "World Bank Group Wellness Data Analysis",
    
    ############################
    
    ############## FIRST PAGE ##############
    
    
    # Creating an "About" tab (for project background and personal info)
    
    tabPanel("About", 
             titlePanel("About"),
             
             mainPanel(
                h3("Project Background and Motivations"),
                 p(
                "For my project, I am collaborating with the Harvard 
                School of Public Health's SHINE Initiative (Sustainability and 
                Health Initiative for Net Positive Enterprise). I am planning the
                SHINE team with analyzing a new dataset on World Bank employees globally
                and different measures of mental well-being."
                ),
             
                h3("Github Repo"),
                p(
                 "This project's GitHub repository lives",
                 a("here", href = "https://github.com/ciaraduggan/wb-wellness"),
                 "."
                ),
             
                 h3("About Me"),
                 p(
                "My name is Ciara Duggan. I am a senior at Harvard College 
                concentrating in Social Studies with a secondary field in Global 
                Health and Health Policy. 
                You can reach me at",
                a("ciaraduggan@college.harvard.edu", href = "mailto:ciaraduggan@college.harvard.edu"),
                  "."
                )
            )),
    
    ############################
    
    
    ############## SECOND PAGE ##############
    
    # Creating a tab with exploratory analysis.
    
    tabPanel("Exploratory Analysis",
             titlePanel("Exploratory Analysis"),
             p("This is where I will include some plots/exploratory analysis"
               )
             ),
    
    ############################
    
    
    ############## THIRD PAGE ##############
    
    # Creating a "Model" tab (where I will present my models and discuss 
    # modeling choices).
    
    tabPanel("Model",
             titlePanel("Model"),
             p("This is where I will give a tour of the modeling choices I made 
             and an explanation of why I made them."
               )
             ),
                
    ############################
    
    
    ############## FOURTH PAGE ##############

      tabPanel("Predictive Modeling Tool",
             fluidPage(
                       titlePanel("Predictive Modeling Tool"),
                       sidebarLayout(
                           sidebarPanel(
                               p("Predict an employee's overall job satisfaction 
                                 by completing the fields below."
                                 ),
                               helpText("Complete the fields below."),
                               
                               numericInput("fair_treatment",
                                           label = "fair_treatment",
                                           value = 1,
                                           min = 1,
                                           max = 4),
                               numericInput("trust_management",
                                           label = "trust_management",
                                           value = 1,
                                           min = 1,
                                           max = 4),
                               numericInput("respect",
                                           label = "respect",
                                           value = 1,
                                           min = 1,
                                           max = 4),
                               numericInput("caring_management",
                                           label = "caring_management",
                                           value = 1,
                                           min = 1,
                                           max = 4)
                               ),
                           
                           mainPanel(
                                tabsetPanel(type = "tabs",
                                    tabPanel("Job Satisfaction",
                                    h2("Employee's predicted overall job satisfaction"),
                                    plotOutput("satisfaction_plot")),
                                tabPanel("Stressed at Work",
                                    h2("Employee's predicted frequency of stress at work"),
                                    plotOutput("stress_plot")))
                                )
                           )
                       )
                       
             )
)
    

 ##############################

# Define server logic 

server <- function(input, output) {
    
    
    predictor_sat <- reactive({
        predict(simple_satisfaction_model,
                tibble(fair_treatment = input$fair_treatment,
                       trust_management = input$trust_management,
                       respect = input$respect,
                       caring_management = input$caring_management),
                interval = "confidence")
        
    })
    
    predictor_stressed <- reactive({
      predict(simple_stressed_model,
              tibble(fair_treatment = input$fair_treatment,
                     trust_management = input$trust_management,
                     respect = input$respect,
                     caring_management = input$caring_management),
              interval = "confidence")
      
    })
                
    output$satisfaction_plot <- renderPlot({
        prediction <- predictor_sat()
        
        ggplot() +
            geom_rect(aes(xmin = prediction[2],
                          xmax = prediction[3], 
                          ymin = 0, 
                          ymax = 0.5, fill = "blue"),
                      fill = "#8cc8db", alpha = 0.5) +
            geom_segment(aes(x = prediction[1], y = 0, xend = prediction[1], yend = 0.5), 
                         color = "#8cc8db", size = 1) +
            geom_rect(aes(xmin = 1,
                          xmax = 10,
                          ymin = 0,
                          ymax = 0.5),
                      fill = "grey", alpha = 0.2) +
            geom_segment(aes(x = 1,
                             y = 0.5,
                             xend = 1,
                             yend = -0.2),
                         linetype = 2) +
            annotate("text", x = 1, y = -0.25,
                     label = "1",
                     size = 5) +
          geom_segment(aes(x = 2,
                           y = 0.5,
                           xend = 2,
                           yend = -0.2),
                       linetype = 2) +
          annotate("text", x = 2, y = -0.25,
                   label = "2",
                   size = 5) +
          geom_segment(aes(x = 3,
                           y = 0.5,
                           xend = 3,
                           yend = -0.2),
                       linetype = 2) +
          annotate("text", x = 3, y = -0.25,
                   label = "3",
                   size = 5) +
          geom_segment(aes(x = 4,
                           y = 0.5,
                           xend = 4,
                           yend = -0.2),
                       linetype = 2) +
          annotate("text", x = 4, y = -0.25,
                   label = "4",
                   size = 5) +
          geom_segment(aes(x = 5,
                           y = 0.5,
                           xend = 5,
                           yend = -0.2),
                       linetype = 2) +
          annotate("text", x = 5, y = -0.25,
                   label = "5",
                   size = 5) +
          geom_segment(aes(x = 6,
                           y = 0.5,
                           xend = 6,
                           yend = -0.2),
                       linetype = 2) +
          annotate("text", x = 6, y = -0.25,
                   label = "6",
                   size = 5) +
          geom_segment(aes(x = 7,
                           y = 0.5,
                           xend = 7,
                           yend = -0.2),
                       linetype = 2) +
          annotate("text", x = 7, y = -0.25,
                   label = "7",
                   size = 5) +
          geom_segment(aes(x = 8,
                           y = 0.5,
                           xend = 8,
                           yend = -0.2),
                       linetype = 2) +
          annotate("text", x = 8, y = -0.25,
                   label = "8",
                   size = 5) +
          geom_segment(aes(x = 9,
                           y = 0.5,
                           xend = 9,
                           yend = -0.2),
                       linetype = 2) +
          annotate("text", x = 9, y = -0.25,
                   label = "9",
                   size = 5) +
          geom_segment(aes(x = 10,
                           y = 0.5,
                           xend = 10,
                           yend = -0.2),
                       linetype = 2) +
          annotate("text", x = 10, y = -0.25,
                   label = "10",
                   size = 5) +
          annotate("text", x = prediction[1], y = 0.6,
                     label = "Employee's estimated job satisfaction level", 
                     color = "#37758a",
                     size = 5) +
            ylim(-0.48, 0.65) +
            xlim(0, 10) +
            theme_void()
    })
    
    output$stress_plot <- renderPlot({
        prediction <- predictor_stressed()
        
        ggplot() +
            geom_rect(aes(xmin = prediction[2],
                          xmax = prediction[3], 
                          ymin = 0, 
                          ymax = 0.5, fill = "blue"),
                      fill = "#8cc8db", alpha = 0.5) +
            geom_segment(aes(x = prediction[1], y = 0, xend = prediction[1], yend = 0.5), 
                         color = "#8cc8db", size = 1) +
            geom_rect(aes(xmin = 1,
                          xmax = 4,
                          ymin = 0,
                          ymax = 0.5),
                      fill = "grey", alpha = 0.2) +
            geom_segment(aes(x = 1,
                             y = 0.5,
                             xend = 1,
                             yend = -0.2),
                         linetype = 2) +
            annotate("text", x = 1, y = -0.25,
                     label = "All the time",
                     size = 5) +
            geom_segment(aes(x = 2,
                             y = 0.5,
                             xend = 2,
                             yend = -0.2),
                         linetype = 2) +
            annotate("text", x = 2, y = -0.25,
                     label = "Frequently",
                     size = 5) +
            geom_segment(aes(x = 3,
                             y = 0.5,
                             xend = 3,
                             yend = -0.2),
                         linetype = 2) +
            annotate("text", x = 3, y = -0.25,
                     label = "Occasionally",
                     size = 5) +
            geom_segment(aes(x = 4,
                             y = 0.5,
                             xend = 4,
                             yend = -0.2),
                         linetype = 2) +
            annotate("text", x = 4, y = -0.25,
                     label = "Never",
                     size = 5) +
            annotate("text", x = prediction[1], y = 0.6,
                     label = "Employee's estimated frequency of stress at work", 
                     color = "#37758a",
                     size = 5) +
            ylim(-0.48, 0.65) +
            xlim(0, 5) +
            theme_void()
    })
}

# Run the application 

shinyApp(ui = ui, server = server)
