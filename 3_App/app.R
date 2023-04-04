#************************************* Shiny App Code *************************************#
# Updated w/correct values for vaccine uptake (d)                                                                
#updated: data2 has vaccine uptake by 0.01


#******************************************************************************************#

### SETUP ###

# source model code
here::i_am("3_App/app.R")
app_data<- read.csv(here("3_App","data_final.csv"),stringsAsFactors=FALSE)


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
  
  #Header
  tags$head(tags$style("#header { text-align: center; }")), # center the header text
  h2(id = "header", "Expected outcomes given inputs:"),
  
  # SIDEBAR
  sidebarLayout(
    sidebarPanel(
      #tabsetPanel(
        
        # MAIN PARAMETERS
        tabPanel("Main", fluid=TRUE,
                 
                 h4("Vaccine information"),
                 h5("These sliders describe describe the efficacy and uptake of a vaccine, where 1 is 100%."),
                 sliderInput("d", "Vaccine uptake fraction", min=.1, max=.9, value=.1, step = 0.1),         
                 
                 #radio button:
                 radioButtons("historic", "Historic Vaccine Uptake", c("Rotavirus", "HPV", "HBV")), #used unweighted
                 sliderInput("e", "Vaccine efficacy fraction", min=.5, max=.9, value=.7, step = 0.2),  
                 
                 h4("Trial information"),
                 h5("These sliders describe challenge trial inputs."),
                 sliderInput("t", "Number of trials", min=1, max=5, value=3, step = 2), 
                 #Should I edit this b/c figure 1 only takes in 0.11 and 0.06?
                 #sliderInput("p", "Per candidate probability of success", min=.05, max=.4, value=.05, step = 0.05),
                 sliderInput("p", "Per candidate probability of success", min=.06, max=.11, value=.06, step = 0.05),
                 sliderInput("y", "Years saved if successful", min=2.5, max=10, value=2.5, step = 2.5), 
                 
                 h4("Disease information"),
                 sliderInput("i", "Incidence of annual infections", min=500000, max=1000000, value=500000, step=500000),  
                # sliderInput("?", "% of trial participants who become infected after exposure), min=.4, max=1.5, value=0.9, step = 0.1),    
                 
        )),
        
    
    # Show the output
    mainPanel(
      textOutput("trial_infections"),
      br(),
      
      textOutput("infections_averted_discounted"),
      br(),
    
      textOutput("years_saved"),
      br(),
    
      textOutput("br_ratio"),
      br(),
      
      #plot output
      plotOutput("plot")
  #  )
 # )
)))

# Define server logic required to output values
server <- function(input, output, session) {
  
  #connect radio button to slider w/historic vaccine uptake 
  observeEvent(input$historic, {
    if (input$historic=="Rotavirus") {
      updateSliderInput(session, "d", value=0.23)
    }
    else if (input$historic=="HPV") {
      updateSliderInput(session,"d", value=0.14)
    }
    else {
      updateSliderInput(session,"d",value=0.9)
    }
  })
  
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
    paste("The expected infections averted (discounted) is", round(filtered()$benefit, digits=2), ".") 
  })
  
  output$years_saved<-renderText({
    paste("The expected number of years saved by a challenge trial is", round(filtered()$expected_years_saved, digits=2), ".")
  })
  
  output$br_ratio <- renderText({
    paste("The benefit-risk ratio is",round(filtered()$ratio, digits=2), ".") 
  })
  
  #For Figure 1: store benefit value
  #given p, t, e, d, i, y
  fig1_filtered<-reactive({
    app_data %>%
      filter(p == input$p,
             t == input$t,
             e == 0.7,
             d == input$d,
             i == 1350000,
             y == input$y
      )
  })
  
  #benefit value:
  fig1_benefit<- reactive({
      fig1_filtered()$benefit
  })
  
  #save benefit value
  observe({
    benefit<-fig1_benefit()
  })
  
  #Create Figure 1
  output$plot<- renderPlot({
    ggplot(app_data %>% filter(e==.7 & i == 1350000 & t %in% c(1,3,5) & p %in% c(.06, .11)) %>%
             mutate(t_fac = paste("Number of candidates:", t),
                    p_fac = paste("Per-candidate success probability:", p),
                    p_fac = fct_rev(p_fac)),
           aes(x = d, y = benefit/1e6, group = paste(y, p), col = factor(y))) +
      facet_grid(p_fac~t_fac) + 
      geom_line() +
      scale_color_manual(name = "Difference in\ntrial length (y)", values = pal) + 
      theme(panel.grid.minor = element_blank(),
            panel.background = element_blank()) + 
      labs(x = "Vaccine uptake", y = "Future infections averted (m, discounted)") +
      
      #add point to show user where their inputs are
      geom_point(data=data.frame(x=input$d, 
                                 y=fig1_benefit()/1e6, 
                                 p=input$p, t=input$t,
                                 ylab = paste("Difference in trial length: ", input$y, "y", sep = "")),
                 aes(x=x,y=y,group=paste(ylab,p), col=factor(ylab))) 
    
      #make sure the point only plots on the correct facet:
    #  filter(t_fac == paste("Number of candidates:", input$t) &
     #       p_fac == paste("Per-candidate success probability:", input$p))

  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
