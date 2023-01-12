#************************************* Shiny App Code *************************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

### SETUP ###

# source model code
here::i_am("3_App/app.R")
#source("2_years_saved.R") not accessing this yet- when I do I get an error

# libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)
library(RColorBrewer)
library(tidyverse)
library(htmltools)


# Define UI:
ui <- fluidPage(
  
  # Application title
  titlePanel("Model-based analysis of controlled human infection studies for a hepatitis C vaccine"),
  
  # SIDEBAR
  sidebarLayout(
    sidebarPanel(
      #tabsetPanel(
        
        # MAIN PARAMETERS
      #  tabPanel("Main", fluid=TRUE,
                 
                 h4("Vaccine information"),
                 h5("These sliders describe describe the efficacy and uptake of a vaccine, where 1 is 100%."),
                 sliderInput("d", "Vaccine uptake fraction", min=.1, max=9, value=.23, step = 0.05),                               
                 sliderInput("e", "Vccine efficacy fraction", min=.5, max=.9, value=.7, step = 0.05),  
                 
                 h4("Trial information"),
                 h5("These sliders describe challenge trial inputs."),
                 sliderInput("t", "Number of trials", min=1, max=5, value=3, step = 1), 
                 sliderInput("p", "Per candidate probability of success", min=.05, max=.4, value=.11, step = 0.05),
                # sliderInput("??", "Sample size of trial", min=0, max=.95, value=.75, step = 0.05), 
                 
                 h4("Disease information"),
                # sliderInput("?", "Incidence at rollout year"), min=1.5, max=3.5, value=2.5, step = 0.1),  
                # sliderInput("?", "% of trial participants who become infected after exposure), min=.4, max=1.5, value=0.9, step = 0.1),    
                 
               #  radioButtons("xaxis", "X-axis variable:", choiceNames= choiceNames, 
                #              choiceValues = choiceValues,
                 #             selected = NULL,
                  #            inline = FALSE, width = NULL),
                 
                 # save and download
               #  actionButton("restore_all", "Restore original inputs"),
              #   downloadButton(outputId = "download_Inputs", 
               #                 label = 'Download inputs',
                #                class= "mybutton"),
                # downloadButton(outputId = "download_Data", 
                 #               label = 'Download estimates',
                  #              class= "mybutton")
                 
        ),
        
    
    # Show the output
    mainPanel(
      textOutput("trial_infections"),
      textOutput("infections_averted_undiscounted"),
      textOutput("infections_averted_discounted"),
      textOutput("br_ratio")
  #  )
 # )
)))

# Define server logic required to output values
server <- function(input, output) {
  
   output$trial_infections<-renderText({
     paste("The expected number of trial infections is", input$t) #incorrect
    })
  
    output$infections_averted_undiscounted<-renderText({
      paste("The expected infections averted (undiscounted) is", input$t) #incorrect
    })
    
    output$infections_averted_discounted<-renderText({
      paste("The expected infections averted (discounted) is", input$e) #incorrect
    })
    
    output$br_ratio <- renderText({
      paste("The benefit-risk ratio is",input$t) #this is not correct right now- need to connect to ratio in 2_years_saved
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
