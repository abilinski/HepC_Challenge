#************************************* Shiny App Code *************************************#
# Note: this won't work right now                                                          #
# Built it out to connect to a csv file that is df in 2_years_saved.R                      #
#                                                                                          #
#******************************************************************************************#

### SETUP ###

# source model code
here::i_am("3_App/app.R")
source(here("3_App", "2_years_saved.R"))

#instead of sourcing this code, could I just export the df to a csv? Called app_data
#app_data<- read.csv("file name here", stringsAsFactors=FALSE)

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
  #Theme
  theme=shinytheme("simplex"),
  
  # Application title
  titlePanel("Model-based analysis of controlled human infection studies for a hepatitis C vaccine"),
  
  # SIDEBAR
  sidebarLayout(
    sidebarPanel(
      #tabsetPanel(
        
        # MAIN PARAMETERS
        tabPanel("Main", fluid=TRUE,
                 
                 h4("Vaccine information"),
                 h5("These sliders describe describe the efficacy and uptake of a vaccine, where 1 is 100%."),
                 sliderInput("d", "Vaccine uptake fraction", min=.1, max=9, value=.23, step = 0.05),                               
                 sliderInput("e", "Vaccine efficacy fraction", min=.5, max=.9, value=.7, step = 0.05),  
                 
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
                 
        )),
        
    
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
server <- function(input, output, session) {
  filtered<- reactive({
    app_data %>%
      filter(p == input$p,
             t == input$t,
             e == input$y,
             d == input$d
             #i == input$i don't have this assigned
             )
  })
  
  #OUTPUT: Take that filtered row from dataset
  output$trial_infections<-renderText({
    paste("The expected number of trial infections is", input$t) #incorrect
  })
  
  output$infections_averted_undiscounted<-renderText({
    paste("The expected infections averted (undiscounted) is", input$t) #incorrect
  })
  
  output$infections_averted_discounted<-renderText({
    paste("The expected infections averted (discounted) is", filtered$infs) #is this how I write it?
  })
  
  output$br_ratio <- renderText({
    paste("The benefit-risk ratio is",filtered$br_ratio) #is this how I write it?
  })


  
  #IF I FOLLOWED THE CONTACT TRACING APP CODE:
  #INPUTS: other idea
  #observeEvent(input$file, { #confused what file should be referring to. No slider called file
    #if file isn't empty:
   # if(!is.null(inFile)){
      #load inputs
    #  uploaded_inputs<-read.csv(inFile$datapath)
      #update each input
      
   # }
  #})
#}
  #MAKE INPUT TABLE
  #MODEL
  #WRITE TEXT
#}

# Run the application 
shinyApp(ui = ui, server = server)
