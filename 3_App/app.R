#************************************* Shiny App Code *************************************#
# Updated w/correct values                                                                 #
# missing undiscounted rate,

#CURRENT ISSUE: trying to have a drop down where users can select vaccine uptake: like HPV, like rotavirus, etc.,
#and then other means that a slider will appear for user to select another value.

#But running into errors here- not sure conditional can override. 
#******************************************************************************************#

### SETUP ###

# source model code
here::i_am("3_App/app.R")
app_data<- read.csv(here("3_App","data.csv"),stringsAsFactors=FALSE)


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
                 
                 #for vaccine uptake: what if I had a radio button with a conditional panel?
                 #if other is selected, then slider will appear
                 #0.14 for HPV, 0.23 for rotavirus, 0.9 for HBV
                 
                 #     c("HPV weighted uptake" = 0.14,
                 #"Rotavirus weighted uptake"=0.23,
                 #"HPV weighted uptake"=0.9,
                 #"Other value"="a"))
                 
                 #Note: will need to do d<-as.integer(input$d) in the server
                 radioButtons("d", 
                              label= "Vaccine Uptake fraction",
                              choiceValues=list(0.14, 0.23, 0.9), #is error b/c there are only 3 here?
                              choiceNames=list("HPV","Rotavirus", "HPV", "Other"),
                              selected = character(0),
                              
                        
                 conditionalPanel(
                   condition= "input.d=='Other' ",
                   sliderInput("d",
                               label = "Vaccine Uptake fraction",
                               min=.1, max=0.9, value=0.1, step=0.1)
                 ),
                 
                 
                 #sliderInput("d", "Vaccine uptake fraction", min=.1, max=.9, value=.1, step = 0.1),                               
                 sliderInput("e", "Vaccine efficacy fraction", min=.5, max=.9, value=.7, step = 0.2),  
                 
                 h4("Trial information"),
                 h5("These sliders describe challenge trial inputs."),
                 sliderInput("t", "Number of trials", min=1, max=5, value=3, step = 1), 
                 sliderInput("p", "Per candidate probability of success", min=.05, max=.4, value=.05, step = 0.05),
                 sliderInput("y", "Years saved if successful", min=2.5, max=10, value=2.5, step = 2.5), #STUCK b/c no 7.5 in data
                 
                 h4("Disease information"),
                 sliderInput("i", "Incidence of annual infections", min=500000, max=1000000, value=500000, step=500000),  
                # sliderInput("?", "% of trial participants who become infected after exposure), min=.4, max=1.5, value=0.9, step = 0.1),    
                 
        )),
        
    
    # Show the output
    mainPanel(
      textOutput("trial_infections"),
    #  textOutput("infections_averted_undiscounted"),
      textOutput("infections_averted_discounted"),
      textOutput("years_saved"),
      textOutput("br_ratio")
  #  )
 # )
))))

# Define server logic required to output values
server <- function(input, output, session) {
  filtered<- reactive({
    app_data %>%
      filter(p == input$p,
             t == input$t,
             e == input$e,
             d == input$d,
             i == input$i,
             y == input$y
             ) 
  })
  
  d<-as.integer(input$d) #added b/c of conditional

   
#OUTPUT: Take that filtered row from dataset, print output:
  
  #this is infs
  output$trial_infections<-renderText({
    paste("The expected number of trial infections is", round(filtered()$infs, digits=2), ".")
  })
  
  #infections averted undiscounted isn't in the dataframe- commented this out for now
  #output$infections_averted_undiscounted<-renderText({
   # paste("The expected infections averted (undiscounted) is", input$t) 
  #})
  
  output$infections_averted_discounted<-renderText({
    paste("The expected infections averted (discounted) is", round(filtered()$benefit, digits=2), ".") #is this how I write it?
  })
  
  output$years_saved<-renderText({
    paste("The expected number of years saved by a challenge trial is", round(filtered()$expected_years_saved, digits=2), ".")
  })
  
  output$br_ratio <- renderText({
    paste("The benefit-risk ratio is",round(filtered()$ratio, digits=2), ".") 
  })
  
}

  

# Run the application 
shinyApp(ui = ui, server = server)
