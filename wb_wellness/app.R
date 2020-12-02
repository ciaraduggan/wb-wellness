
# World Bank Group Employee Job Satisfaction Analysis Shiny App 
# Author: Ciara Duggan

# Load libraries

library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(tidymodels)

# Loading data

satisfaction_vi <- readRDS("./sat_vi.rds")
simple_satisfaction_model <- readRDS("./simple_satisfaction_model.rds")


# Define UI
ui <- navbarPage(
    theme = shinytheme("cosmo"),
    "World Bank Group Employee Job Satisfaction",
    
    ############################
    
    ############## FIRST PAGE ##############
    
    
    # Creating an "About" tab (for project background and personal info)
    
    tabPanel("About", 
           
             mainPanel(
                h3("Project Background and Motivations"),
                 p(
                "This project investigates the relationship between the overall
                job satisfaction of World Bank group employees and a range of
                variables related to their work and their workplace environment. 
                The primary goal of this project was to understand which 
                work-related factors are most predictive of overall job 
                satisfaction. This analysis accounted for a variety of factors 
                related to workplace culture, worklife integration, 
                supervisor and coworker support, workplace design, job security,
                employee autonomy, workload, recognition, pay, personal growth,
                and a sense of meaningful work."),
                
                h3("About the Data"),
                p("The data used in this analysis comes from a study conducted
                  by the Harvard T. H. Chan School of Public Health's SHINE
                  (Sustainability and Health Initiative for Net Positive 
                  Enterprise) team. Scientists at SHINE \" conduct researh with
                  the aim of shining a light on the dynamic connections that 
                  exist between individuals, organizations, workplaces and 
                  communities and their impact on our well-being.\" You can
                  learn more about SHINE and its research",
                  a("here", href = "https://shine.sph.harvard.edu/"),
                  "."),
                p("This survey data was collected in 2020 as part of the World
                Bank Group's Staff Wellness Project. The",
                a("World Bank Group", href = 
                    "https://www.worldbank.org/en/who-we-are"),
                "(WBG) is a global partnership between 189 member countries and
                five institutions working towards the shared goals of 
                towards the shared goal of reducing poverty and promoting
                shared prosperity in developing countries. The WBG has 
                offices in over 130 locations and staff from more than 170
                countries."
                ),
                p("The original de-identified dataset contained 7390 
                responses and over 250 variables. 
                Survey responses were collected from WBG employees based in 
                locations around the globe. After incomplete responses 
                were removed, the data contained a total of 5940 observations. 
                This analysis considers the relative importance of 33 variables
                related to work and the workplace in predicting overall 
                job satisfaction of WBG employees."
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
                a("ciaraduggan@college.harvard.edu", 
                  href = "mailto:ciaraduggan@college.harvard.edu"),
                  "."
                )
            )),
    
    ############################
    
    
    ############## SECOND PAGE ##############
    
    # Creating a "Model" tab (where I will present my models and discuss 
    # modeling choices).
    
    tabPanel(
      "Model",
      h1("Modeling WBG Employee Job Satisfaction"),
      h2("Determining Variable Importance through LASSO Regression"),
      p("The primary modeling goal of this project was to determine which 
      variables related to work and the workplace environment are most 
      predictive of World Bank Group employees' overall reported job 
      satisfaction. In order to determine which variables were most important 
      for predicting job satisfaction, I performed LASSO (Least Absolute 
      Shrinkage and Selection Operator) regression analysis. LASSO is a 
      variation of linear regression which is useful when one is handling
      data with a large number of predictor variables. This method can reduce
      the problem of overfitting by applying a penalty parameter and 
      reducing the variable coefficients which are responsible for
      large variance. I optimized this model by determining the optimal 
      penalty/regularization parameter and selecting the model 
      with the lowest Root Mean Square Error."
        ),
      p("Each variables measures the extent to which an employee agrees 
        with a given statement. Higher scores represent stronger agreement; 
        lower scores represent stronger disagreement. The statements associated 
        with each variable can be found at the bottom of this page."
        ),
      
        plotOutput("importance_plot"),
      
      h2("Model Interpretation"),
      p(
        "The Variable Importance Plot above visualizes the relative importance
        of each variable in predicting WBG employees' overall reported job 
        satisfaction (based on the results of the LASSO regression analysis).
        As this plot shows, the most important predictor of WBG employees'
        overall job satisfaction, by far, is whether or not they find their work 
        meaningful. The second most important predictor of job satisfaction
        is whether employees feel recognized for their work. Some other top 
        predictors of job satisfaction include (a) whether employees
        feel that their employers pay them fairly for their work,
        (b) whether employees feel that they are part of a team at work,
        and (c) whether employees worry about losing their job."
      ),
      p("The plot indicates that certain variables are (perhaps surprisingly)
        less important when it comes to predicting job satisfaction. 
        For example, whether an employee feels that they work \"too many 
        long hours or too much overtime\" does not appear to be an important 
        predictor of overall job satisfaction. Nor does whether an employee 
        feels that they have sufficient rest/break time at work seem to be an 
        important predictor of job satisfaction."
        ),
      
      h2("Variable Codes"),
      
      tags$ul(
        
        tags$li(
          tags$b("meaningful_work:"), "\"I find my work meaningful.\""
        ),
        
        tags$li(
          tags$b("recognition:"), "\"I feel recognized for my work.\""
        ),
        
        tags$li(
          tags$b("fair_pay:"), "\"My employer pays me fairly for my work.\""
        ),
        
        tags$li(
          tags$b("team:"), "\"I feel part of a team at work.\""
        ),
        
        tags$li(
          tags$b("job_insecurity:"), "\"I worry about losing my job.\""
        ),
        
        tags$li(
          tags$b("helpful_management:"), "\"Management helps me deal with challenges at work.\""
        ),
        
        tags$li(
          tags$b("work_autonomy:"), "\"Staff feel respected at work.\""
        ),
        
        tags$li(
          tags$b("respect:"), "\"I have a lot of freedom to decide how to do my work.\""
        ),
        
        tags$li(
          tags$b("helpful_supervisor:"), "\"My supervisor is helpful.\""
        ),
        
        tags$li(
          tags$b("energizing_culture:"), "\"There is a vibrant atmosphere which is energizing.\""
        ),
        
        tags$li(
          tags$b("stressful:"), "\"My job is stressful.\""
        ),
        
        tags$li(
          tags$b("workplace_design:"), "\"The physical design of my workplace helps me to be productive (consider before COVID pandemic).\""
        ),
        
        tags$li(
          tags$b("workfriends:"), "\"Some of my coworkers are my personal friends.\""
        ),
        
        tags$li(
          tags$b("mental_exhaustion:"), "\"My job is mentally exhausting.\""
        ),
        
        tags$li(
          tags$b("clear_expectations:"), "\"I know what is expected of me.\""
        ),
        
        tags$li(
          tags$b("caring_supervisor:"), "\"My supervisor truly cares about me.\""
        ),
        
        tags$li(
          tags$b("community_culture:"), "\"People feel a sense of loyalty, commitment and community within my workplace.\""
        ),
        
        tags$li(
          tags$b("advancement_opportunities:"), "\"There are opportunities for advancement or a higher position.\""
        ),
        
        tags$li(
          tags$b("caring_management:"), "\"Management truly cares about the health and well-being of staff.\""
        ),
        
        tags$li(
          tags$b("poor_worklife_integration:"), "\"Demands of my job interfere with my home life.\""
        ),
        
        tags$li(
          tags$b("fair_treatment:"), "\"Staff feel they are treated fairly.\""
        ),
        
        tags$li(
          tags$b("too_much_work:"), "\"I have too much to do at work to do a good job.\""
        ),
        
        tags$li(
          tags$b("authentic_culture:"), "\"The culture is authentic and honest.\""
        ),
        
        tags$li(
          tags$b("trust_management:"), "\"Staff trust management.\""
        ),
        
        
        tags$li(
          tags$b("predictable_schedule:"), "\"My schedule is predictable.\""
        ),
        
        tags$li(
          tags$b("flexible_schedule:"), "\"I can decide when to do my work (My schedule is flexible on a daily basis and I set my own schedule).\""
        ),
        
        tags$li(
          tags$b("long_hours:"), "\"I work too many long hours or too much overtime.\""
        ),
        
        tags$li(
          tags$b("reliable_coworkers:"), "\"I can rely on my coworkers for help.\""
        ),
        
        tags$li(
          tags$b("employee_input:"), "\"I have a lot to say about what happens in the workplace.\""
        ),
        
        tags$li(
          tags$b("physical_exhaustion:"), "\"My job is physically exhausting.\""
        ),
        
        tags$li(
          tags$b("fair_supervisor:"), "\"My supervisor treats me fairly.\""
        ),
        
        tags$li(
          tags$b("schedule_overflows:"), "\"My work schedule overflows into my home/family/leisure time to accommodate international time-zones.\""
        ),
        
        tags$li(
          tags$b("rest_time:"), "\"I have sufficient rest/break time in my job.\""
        )
      )
    ),
                
    ############################
    
    
    ############## FOURTH PAGE ##############

      tabPanel("Predictive Modeling Tool",
             fluidPage(
                       titlePanel("Predictive Modeling Tool"),
                       
                       h3("Predict an employee's overall job satisfaction 
                                 by completing the fields below."),
                       
                       p("LASSO 
                                 regression analysis indicated that the 
                                 variables below are the twelve workplace 
                                 environment factors which are most predictive 
                                 of overall job satisfaction (based on the data 
                                 collected by SHINE).
                                 This predictive tool uses a simple linear 
                                 regression model to predict a WBG employee's 
                                 overall job satisfaction based on the values 
                                 the user provides. For more information on how 
                                 this tool works, check out the source code
                                 in this project's Github repo."
                       ),
                       
                       fluidRow(
                         column(9, plotOutput("satisfaction_plot"))
                       ),
                       
                       p("For the variables below, a score of 1 indicates that the employee \"Strongly Disagrees\" with the given statement,
                         a score of 2 indicates that they \"Disagree\",
                         a score of 3 indicates that they \"Agree\", and
                         a score of 4 indicates that they \"Strongly Agree\".
                         The statements which correspond with each variable 
                         are shown below."
                         ),
                       
                       fluidRow(
                         column(2, 
                                numericInput("meaningful_work",
                                             label = "meaningful_work",
                                             value = 1,
                                             min = 1,
                                             max = 4),
                                numericInput("work_autonomy",
                                             label = "work_autonomy",
                                             value = 1,
                                             min = 1,
                                             max = 4),
                         ),
                         
                         column(2, 
                                numericInput("recognition",
                                             label = "recognition",
                                             value = 1,
                                             min = 1,
                                             max = 4),
                                numericInput("respect",
                                             label = "respect",
                                             value = 1,
                                             min = 1,
                                             max = 4),
                                ),
                                
                                column(2, 
                                       numericInput("fair_pay",
                                                    label = "fair_pay",
                                                    value = 1,
                                                    min = 1,
                                                    max = 4),
                                       numericInput("helpful_supervisor",
                                                    label = "helpful_supervisor",
                                                    value = 1,
                                                    min = 1,
                                                    max = 4),
                                ),
                         
                         column(2, 
                                numericInput("team",
                                             label = "team",
                                             value = 1,
                                             min = 1,
                                             max = 4),
                                numericInput("energizing_culture",
                                             label = "energizing_culture",
                                             value = 1,
                                             min = 1,
                                             max = 4),
                         ),
                        
                         column(2, 
                                numericInput("job_insecurity",
                                             label = "job_insecurity",
                                             value = 1,
                                             min = 1,
                                             max = 4),
                                numericInput("stressful",
                                             label = "stressful",
                                             value = 1,
                                             min = 1,
                                             max = 4),
                         ),

                                column(2, 
                                       numericInput("helpful_management",
                                                    label = "helpful_management",
                                                    value = 1,
                                                    min = 1,
                                                    max = 4),
                                       numericInput("workplace_design",
                                                    label = "workplace_design",
                                                    value = 1,
                                                    min = 1,
                                                    max = 4)
                                )
                       ),
                       
                       h3("What do these variables mean?"),
                       
                       tags$ul(
                         
                         tags$li(
                           tags$b("meaningful_work:"), "\"I find my work meaningful.\""
                         ),
                         
                         tags$li(
                           tags$b("recognition:"), "\"I feel recognized for my work.\""
                         ),
                         
                         tags$li(
                           tags$b("fair_pay:"), "\"My employer pays me fairly for my work.\""
                         ),
                         
                         tags$li(
                           tags$b("team:"), "\"I feel part of a team at work.\""
                         ),
                         
                         tags$li(
                           tags$b("job_insecurity:"), "\"I worry about losing my job.\""
                         ),
                         
                         tags$li(
                           tags$b("helpful_management:"), "\"Management helps me deal with challenges at work.\""
                         ),
                         
                         tags$li(
                           tags$b("work_autonomy:"), "\"Staff feel respected at work.\""
                         ),
                         
                         tags$li(
                           tags$b("respect:"), "\"I have a lot of freedom to decide how to do my work.\""
                         ),
                         
                         tags$li(
                           tags$b("helpful_supervisor:"), "\"My supervisor is helpful.\""
                         ),
                         
                         tags$li(
                           tags$b("energizing_culture:"), "\"There is a vibrant atmosphere which is energizing.\""
                         ),
                         
                         tags$li(
                           tags$b("stressful:"), "\"My job is stressful.\""
                         ),
                         
                         tags$li(
                           tags$b("workplace_design:"), "\"The physical design of my workplace helps me to be productive (consider before COVID pandemic).\""
                         )
                       )
                       

                       )
                       
             )
)
    

 ##############################

# Define server logic 

server <- function(input, output) {
  
  output$importance_plot <- renderPlot({
    
    satisfaction_vi %>%
      mutate(Importance = abs(Importance),
             Variable = fct_reorder(Variable, Importance)) %>%
      ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
      geom_col() +
      scale_x_continuous(expand = c(0, 0.01)) +
      labs(y = NULL,
           title = "Variable Importance Scores", 
           subtitle = "Sense of meaning in work and feeling recognized are the most important predictors of overall job satisfaction") +
      theme_light()
  })
  
    
    
    predictor_sat <- reactive({
 
      predict(simple_satisfaction_model, 
              tibble(
                meaningful_work =
                  input$meaningful_work, 
                recognition = 
                  input$recognition,
                fair_pay = 
                  input$fair_pay,
                team = 
                  input$team,
                job_insecurity = 
                  input$job_insecurity,
                helpful_management = 
                  input$helpful_management,
                work_autonomy = 
                  input$work_autonomy,
                respect = 
                  input$respect,
                helpful_supervisor = 
                  input$helpful_supervisor,
                energizing_culture = 
                  input$energizing_culture,
                stressful = 
                  input$stressful,
                workplace_design = 
                 input$workplace_design),
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
}

# Run the application 

shinyApp(ui = ui, server = server)
