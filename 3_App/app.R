#************************************* Shiny App Code *************************************#


#******************************************************************************************#

### SETUP ###

# source model code
here::i_am("3_App/app.R")

#Data: for figure 1
app_data<- read.csv(here::here("3_App","data_final.csv"),stringsAsFactors=FALSE)

#Data: for figure 2
fig2_data<- read.csv(here::here("3_App","data_fig2.csv"),stringsAsFactors=FALSE)


# libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)
library(RColorBrewer)
library(tidyverse)
library(htmltools)
pal = c("#fbe392", "#fab24d", "#ec8400", "#d25700", "#b02912", "#311432")
pal2 = c("#fbe392", "#fab24d", "#ec8400", "#d25700", "#b02912", "#311432", "#311572")

# Define UI:
ui <- fluidPage(
  #Theme
  theme=shinytheme("simplex"),
  
  # Application title
  titlePanel("Model-based analysis of controlled human infection studies for a hepatitis C vaccine"),
  
  # SIDEBAR
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        
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
                 sliderInput("t", "Number of trials", min=1, max=10, value=3, step = 1), 
                 sliderInput("p", "Per candidate probability of success", min=.06, max=.4, value=.06, step = 0.02),
                 sliderInput("y", "Years saved if successful", min=2.5, max=10, value=2.5, step = 2.5),
                 
                 #should this go under challenge trial input?
                 sliderInput("r", "Discount rate", min=.000000000001, max=0.03,value=0.03, step=0.02999999999),

                 h4("Disease information"),
                 sliderInput("i", "Incidence of annual infections", min=250000, max=2000000, value=500000, step=250000),  
                 # sliderInput("?", "% of trial participants who become infected after exposure), min=.4, max=1.5, value=0.9, step = 0.1),    
                 
        ))),
    
    
    #Results
    #edit text here: have "results summary" and then round values to whole # (except 1 decimal for yrs saved)
    mainPanel(tabsetPanel(type="tabs",
                          
                          #Text output
                          tabPanel("Model Results",
                                   htmlOutput("overall_info"),
                                   br(),
                                   
                                   textOutput("trial_infections"),
                                   
                                   textOutput("infections_averted_discounted"),
                                   
                                   textOutput("years_saved"),
                                   
                                   textOutput("br_ratio"),
                                   br(),
                                  
                                    #plot output: figure 1
                                   plotOutput("plot"),
                                   br(),
                                   
                                   #plot output: figure 2
                                   plotOutput("plot2"),
                          ),
                          
                          #Documentation
                          tabPanel("Documentation",
                                   includeMarkdown("documentation.md"))
    )
    )),
  
  hr(),
  includeMarkdown("footer.md")
)

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
      filter(p == input$p, #prob success
             t == input$t, #num trials
             e == input$e, #efficacy
             d == input$d, #vaccine uptake
             i == input$i, #incidence
             y == input$y #years saved if successful
      ) 
  })
  
  df2_filtered <- reactive({
    fig2_data %>%
      filter(e==input$e, #efficacy
             v == input$d, #vaccine uptake
             t == input$t, #num trials
             p == input$p, #prob success
             i == input$i, #incidence
             d == input$r #discount rate
      ) 
  })
  

  #OUTPUT: Take that filtered row from dataset, print output:
  
  #overall info:
  output$overall_info <- renderUI({
    HTML("<div style='font-size: 14px;'>
         <strong>What are the risks and benefits of challenge trials for hepatitis C vaccine development?</strong>
         Altering inputs shows that the benefits of a challenge trial increase with more vaccine candidates, 
         faster trials, and greater uptake.  
         <em>For more details, see the documentation tab.</em>")
  })
  
  #this is infs
  output$trial_infections<-renderText({
    paste("The expected number of trial infections is", round(filtered()$infs, digits=2), ".")
  })
  
  output$infections_averted_discounted<-renderText({
    paste("The expected infections averted (discounted) is", round(filtered()$benefit, digits=2), ".") 
  })
  
  output$years_saved<-renderText({
    paste("The expected number of years saved by a challenge trial is", round(filtered()$expected_years_saved, digits=2), ".")
  })
  
  output$br_ratio <- renderText({
    paste("The benefit-risk ratio is",round(filtered()$ratio, digits=2), ".") 
  })
  
  #output for figure 1:
  #benefit value:  calculates benefit value from filtered data
  fig1_benefit<- reactive({
    filtered()$benefit
  })
  
  #save benefit value in variable called "benefit"
  observe({
    benefit<-fig1_benefit()
  })
  
  #output for figure 2:
  #calculate difference in trial length from filtereed data
  fig2_y<- reactive({
    df2_filtered()$y
  })
  
  #save y in variable called "y"
  observe({
    y<-fig2_y()
  })
  
  
  #Create Figure 1
  #HELP: How do I add a slider to multiply the outputs by 500, and then the plot will show estimated monetary cost instead?
  
  output$plot<- renderPlot({
    
    #create a dataframe for the specified output:
    filtered2<- app_data %>% 
      filter(e==input$e & i == input$i & t %in% c(1,input$t,5) & p %in% c(.06, input$p)) %>%
      mutate(t_fac = paste("Number of candidates:", t),
             p_fac = paste("Per-candidate success probability:", p),
             p_fac = fct_rev(p_fac))
    
    #Start with general plot:
    ggplot(filtered2,
           aes(x = d, y = benefit/1e6, group = paste(y, p), col = factor(y))) +
      facet_grid(p_fac~t_fac) + 
      geom_line() +
      scale_color_manual(name = "Difference in\ntrial length (y)", values = pal) + 
      theme(panel.grid.minor = element_blank(),
            panel.background = element_blank()) + 
      labs(x = "Vaccine uptake", y = "Future infections averted (m, discounted)",
           title="Figure 1: Incremental future infections averted (discounted)") +
      theme (plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(size = 14)) +
      
      #Plot a point that corresponds with user value
      geom_point(data=filtered2 %>% filter(p == input$p & t == input$t), aes(x=input$d, y=fig1_benefit()/1e6),
                 color="red",
                 size=3,
                 show.legend=FALSE)
                 
  })
  
  #Create Figure 2: 
  #still need to add on point to show where
  #also need to create button: toggle from benefit risk ratio to quali benefit risk ratio (multiply by 100)
  output$plot2<- renderPlot({
    
    #create dataframe for output
    filtered_p2<- df2_final %>%
      filter(d == input$r, e == input$e, i == 250000, base==T, p %in% c(.06, input$p), t %in% c(1,input$t, 5)) %>%
      mutate(t_fac = paste("Number of candidates:", t),
             p_fac = paste("Per-candidate success probability:", p),
             p_fac = fct_rev(p_fac))
      
    ggplot(filtered_p2,
           aes(x = v, y = y, group = threshold, col = factor(threshold))) + geom_line() + 
      facet_grid(p_fac~t_fac) + 
      ylim(0, 10) + 
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank()) + 
      scale_color_manual(name = "Benefit-risk\nthreshold", values = pal2) + 
      labs(x = "Vaccine uptake", y = "Difference in trial length (y)") #+
    
    #Plot a point that corresponds with user value
 #   geom_point(data=filtered_p2 %>% filter(p==input$p & t==input$t), aes(x=input$d, y=fig2_y()),
#               color="red",
#               size=3,
#               show.legend=FALSE)
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
