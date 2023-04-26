#************************************* Shiny App Code *************************************#
#Edited code, updated with 4/25 edits

#******************************************************************************************#

### SETUP ###

# source global options
source("../global_options.R")

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
        
        # Main parameters
        tabPanel("Main", fluid=TRUE,
                 
                 h4("Vaccine information"),
                 h5("These sliders describe describe the efficacy and uptake of a vaccine, where 1 is 100%."),
                 sliderInput("vu", "Vaccine uptake fraction", min=.1, max=.9, value=.1, step = 0.05),        
                 
                 #radio button:
                 radioButtons("historic", "Historic vaccine uptake", c("Rotavirus", "HPV", "HBV")), #used unweighted
                 sliderInput("e", "Vaccine efficacy fraction", min=.5, max=.9, value=.7, step = 0.1),  
                 
                 h4("Trial information"),
                 h5("These sliders describe challenge trial inputs."),
                 sliderInput("t", "Number of candidates", min=1, max=10, value=3, step = 1),
                 sliderInput("p", "Per-candidate probability of success", min=.06, max=.4, value=.06, step = 0.02),
                 sliderInput("y", "Difference in trial length between traditional and challenge trial", min=2.5, max=20, value=2.5, step = 2.5),
        ),
        
        #Advanced parameters
        tabPanel("Advanced",
                 #advanced parameter
                 h4("Disease information"),
                 sliderInput("i", "Incidence of annual infections", min=250000, max=2000000, value=50000, step=250000),
                 
                 h4("Other"),
                 sliderInput("dr", "Discount rate", min=0, max=0.03,value=0.03, step=0.005), #I THINK THIS NEEDS TO BE A BUTTON?
                 sliderInput("cost", "Cost of treating an HCV infection in LMIC (benchmark cost)", min=100, max=5000, value=500, step=100),
                 sliderInput("tr", "Time from trial initiation until vaccine rollout", min=10, max=50, value=30, step=5),
                 sliderInput("q", "Proportion of acute HCV cases who develop chronic HCV", min=0.7, max=0.9, value=0.8, step=0.05),
                 sliderInput("n", "Number of people in each trial arm", min=20,max=160,value=100, step=10),
                 sliderInput("qb", "QALY benefit-risk ratio", min=10, max=200, value=100, step=10)
        )
      )),
    
    
    
    #Results
    mainPanel(tabsetPanel(type="tabs",
                          
                          #Text output
                          tabPanel("Model Results",
                                   
                                   htmlOutput("overall_info"),
                                   br(),
                                   
                                   htmlOutput("results_summary"),
                                   
                                   textOutput("trial_infections"),
                                   
                                   textOutput("infections_averted_discounted"),
                                   
                                   textOutput("years_saved"),
                                   
                                   textOutput("br_ratio"),
                                   br(),
                                   
                                   
                                   #plot output: figure 1
                                   selectInput("plotType", "Choose Plot Type for Figure 1:",
                                               choices = c("Future infections averted", "Estimated monetary cost")),
                                   
                                   plotOutput("plot"),
                                   br(),
                                   
                                   #plot output: figure 2
                                   selectInput("plotType2", "Choose Plot Type for Figure 2:",
                                               choices = c("Infection benefit-risk ratio", "QALY benefit-risk ratio")),
                                   
                                   plotOutput("plot2"),
                                   br(),
                                  
                          ),
                          
                          
                          #Documentation
                          tabPanel("Documentation",
                                   includeMarkdown("documentation.md"))
    )
    )),
  
  hr(),
  includeMarkdown("footer.md")
)

#Define server logic required to output values
server <- function(input, output, session) {
  
  #connect radio button to slider w/historic vaccine uptake
  observeEvent(input$historic, {
    if (input$historic=="Rotavirus") {
      updateSliderInput(session, "vu", value=0.23)
    }
    else if (input$historic=="HPV") {
      updateSliderInput(session,"vu", value=0.14)
    }
    else {
      updateSliderInput(session,"vu",value=0.9)
    }
  })
  
  #filtered: setting up for text results
  filtered<- reactive({
    expand_grid(
      p = input$p, #prob success
      t = input$t, #num trials
      e = input$e, #efficacy
      vu = input$vu, #vaccine uptake
      i = input$i, #incidence
      y = input$y, #difference in trial length
      dr = if(input$dr == 0) 1e-12 else input$dr, #discount rate
      tr = input$tr, #time till rollout
      q = input$q, #proportion chronic
      n = input$n #number in trial arm
    )
  })
  
  #compute outputs from filtered (for text)
  filtered_computed<- reactive({
    df<-filtered()
    
    #### SIMULATION ####
    #number of tries
    tries = 1:df$t
    val = rep(0, length(tries))
    cost = rep(0, length(tries))
    
    #contribution to expectation if successful in j tries
    for(j in tries){
      val[j] = (1-df$dr)^df$tr*(1-df$p)^(j-1)*df$p*(1-(1-df$dr)^(j*df$y))/(df$dr)
      cost[j] = (1-df$p)^(j-1)*df$p*(df$n*j - 1/2*df$n*df$e)
    }
    
    df$expected_years_saved = sum(val)
    df$infs = sum(cost) + (1-df$p)^(j)*j*df$n
    
    
    df$benefit = df$expected_years_saved*df$vu*df$e*df$i
    df$ratio = df$benefit/df$infs
    
    #return df
    data.frame(
      expected_years_saved = df$expected_years_saved,
      infs = df$infs,
      benefit = df$benefit,
      ratio = df$ratio
    )
  })
  
  #data for plot for figure 1:
  filtered_plot1<- reactive({
    p = c(input$p,0.06) #prob success
    t = c(input$t,1,5) #num trials
    e = input$e #efficacy
    vu = seq(.1, .9, by = .05) #vaccine uptake
    i = input$i #incidence
    y = c(2.5, 5, 7.5, 10, input$y) #difference in trial length
    dr = if(input$dr == 0) 1e-12 else input$dr #discount rate
    tr = input$tr #time till rollout
    q = input$q #proportion chronic
    n = input$n #number in trial arm
   
    df = expand_grid(p, t, y, e, vu, i, dr, tr, q, n) #using values above
    
    #### SIMULATION ####
    for(i in 1:nrow(df)){
      
      # number of tries
      tries = 1:df$t[i]
      val = rep(0, length(tries))
      cost = rep(0, length(tries))
      
      # contribution to expectation if successful in j tries
      for(j in tries){
        val[j] = (1-df$dr[i])^df$tr[i]*(1-df$p[i])^(j-1)*df$p[i]*(1-(1-df$dr[i])^(j*df$y[i]))/(df$dr[i])
        cost[j] = (1-df$p[i])^(j-1)*df$p[i]*(df$n[i]*j - 1/2*df$n[i]*df$e[i])
      }
      
      df$expected_years_saved[i] = sum(val)
    }
    
    df$benefit = df$expected_years_saved*df$vu*df$e*df$i
    
    #return df
    data.frame(
      t = df$t,
      p = df$p,
      vu = df$vu,
      y = df$y,
      expected_years_saved = df$expected_years_saved,
      benefit = df$benefit
    )
  })
  
  #Data for figure 2
  filtered_plot2<- reactive({
    
    # get threshold fcn
    get_threshold = function(y, threshold, t,p,e,i,vu,dr,n, tr){
      # number of tries
      tries = 1:t
      val = rep(0, length(tries))
      cost = rep(0, length(tries))
      
      # contribution to expectation if successful in j tries
      for(j in tries){
        val[j] = (1-p)^(j-1)*p*(1-dr)^tr*(1-(1-dr)^(j*y))/(dr)
        cost[j] = (1-p)^(j-1)*p*(n*j - 1/2*n*e)
      }
      
      expected_years_saved = sum(val)
      infs = sum(cost) + (1-p)^(j)*j*n
      benefit = expected_years_saved*vu*e*i
      ratio = benefit/infs
      
      return(abs(ratio-threshold))
    }
    
    
    df2 = expand_grid(threshold = c(50, 100, 250, 500, 1000, 2500), #threshold value
                      e = input$e,  #vaccine efficacy
                      vu = seq(.1, .9, by = .05), #vaccine uptake
                      t = c(input$t,1,5), #number of trials
                      p = c(input$p,0.06), #probability of success
                      y = NA,
                      dr = if(input$dr == 0) 1e-12 else input$dr, #discount rate
                      i= input$i, #incidence
                      tr = input$tr, #time till rollout
                      n = input$n) %>%  #number of people in trial arm
      
      mutate(t_fac = paste("Number of candidates:", t),
             p_fac = paste("Per-candidate success probability:", p),
             p_fac = fct_rev(p_fac))
    
    for(i in 1:nrow(df2)) {
      df2$y[i] = optim(par = 5, fn = get_threshold, threshold = df2$threshold[i],
                       t = df2$t[i], p = df2$p[i], e = df2$e[i], dr = df2$dr[i],
                       i = df2$i[i], vu = df2$vu[i], tr = df2$tr[i], n = df2$n[i],
                       method = "BFGS")$par
    }
    
    #return df
    data.frame(
      t = df2$t,
      p = df2$p,
      vu = df2$vu,
      y = df2$y,
      threshold = df2$threshold
    )
  })

  
  #overall info:
  output$overall_info <- renderUI({
    HTML("<div style='font-size: 14px;'>
         <strong>What are the risks and benefits of challenge trials for hepatitis C vaccine development?</strong>
         Altering inputs shows that the benefits of a challenge trial increase with more vaccine candidates,
         faster trials, and greater uptake.  
         <em>For more details, see the documentation tab.</em>")
  })
  
  output$trial_infections<-renderText({
    paste0("The expected number of trial infections is ", round(filtered_computed()$infs,digits=0), ".")
  })
  
  output$infections_averted_discounted<-renderText({
    paste0("The expected infections averted (discounted) is ", round(filtered_computed()$benefit, digits=0), ".")
  })
  
  output$years_saved<-renderText({
    paste0("The expected number of years saved by a challenge trial is ", round(filtered_computed()$expected_years_saved, digits=1), ".")
  })
  
  output$br_ratio <- renderText({
    paste0("The benefit-risk ratio is ",round(filtered_computed()$ratio, digits=0), ".")
  })
  
  output$results_summary <- renderUI({
    HTML("<div style='font-size: 14px;'>
         <strong>Results Summary: </strong>")
  })
  
  #output for figure 1:
  #benefit value:  calculates benefit value from filtered data
  fig1_benefit<- reactive({
    filtered_computed()$benefit
  })
  
  #save benefit value in variable called "benefit"
  observe({
    benefit<-fig1_benefit()
  })
  
  #save ratio in reactive (for fig2)
  ratio_val<- reactive({
    ratio<-round(filtered_computed()$ratio, digits=0)
    return(ratio)
  })
  
  
  #Create Figure 1
  output$plot<- renderPlot({
    #Create dataframe
    filtered2<- filtered_plot1() %>%
      mutate(t_fac = paste("Number of candidates:", t),
             p_fac = paste("Per-candidate success probability:", p),
             p_fac = fct_rev(p_fac))
    
    #if-else: plot 1a or plot 1b
    if (input$plotType == "Future infections averted") {
      
      #code for plot 1
      #Start with general plot:
      ggplot(filtered2,
             aes(x = vu, y = benefit/1e6, group = paste(y, p), col = factor(y))) +
        facet_grid(p_fac~t_fac) +
        geom_line() +
        scale_color_manual(name = "Difference in\ntrial length (y)", values = pal) +
        theme(panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
        labs(x = "Vaccine uptake", y = "Future infections averted (m, discounted)",
             title="Incremental future infections averted (discounted)") +
        theme (plot.title = element_text(hjust = 0.5)) +
        theme(plot.title = element_text(size = 14)) +
        
        #Plot a point that corresponds with user value
        geom_point(data=filtered2 %>% filter(p == input$p & t == input$t), aes(x=input$vu, y=fig1_benefit()/1e6),
                   color="red",
                   size=3,
                   show.legend=FALSE)
    }
    else {
      #code for plot 2
      #Start with general plot:
      ggplot(filtered2,
             aes(x = vu, y = (benefit/1e6) * input$cost, group = paste(y, p), col = factor(y))) +
        facet_grid(p_fac~t_fac) +
        geom_line() +
        scale_color_manual(name = "Difference in\ntrial length (y)", values = pal) +
        theme(panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
        labs(x = "Vaccine uptake", y = "Estimated monetary cost",
             title="Estimated monetary cost") +
        theme (plot.title = element_text(hjust = 0.5)) +
        theme(plot.title = element_text(size = 14)) +
        
        #Plot a point that corresponds with user value
        geom_point(data=filtered2 %>% filter(p == input$p & t == input$t), aes(x=input$vu, y=(fig1_benefit()/1e6) * input$cost),
                   color="red",
                   size=3,
                   show.legend=FALSE)
      
    }
  })
  
  #Create Figure 2:
  output$plot2<- renderPlot({
    
    #create dataframe for output
    filtered_p2<- filtered_plot2() %>%
      mutate(t_fac = paste("Number of candidates:", t),
             p_fac = paste("Per-candidate success probability:", p),
             p_fac = fct_rev(p_fac))
    
    #Figure 2: plot A
    if (input$plotType2 == "Infection benefit-risk ratio") {
      #Create base plot
      ggplot(filtered_p2,
             aes(x = vu, y = y, group = threshold, col = factor(threshold))) + geom_line() +
        facet_grid(p_fac~t_fac) +
        ylim(0, 10) + 
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_blank()) +
        scale_color_manual(name = "Benefit-risk\nthreshold", values = pal2) +
        labs(x = "Vaccine uptake",
             y = "Difference in trial length (y)",
             title= "Infection benefit-risk frontiers across different parameter values")  +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.title = element_text(size = 14)) +
        
        #Plot a point that corresponds with user value
        geom_point(data=filtered_p2 %>%
                     filter(t_fac == paste("Number of candidates:", input$t) &
                              p_fac == paste("Per-candidate success probability:", input$p)),
                   aes(x=input$vu, y=input$y),
                   color="red",
                   size=3,
                   show.legend=FALSE) +
        
        #Add label:
        geom_text(data=filtered_p2 %>%
                    filter(t_fac == paste("Number of candidates:", input$t) &
                             p_fac == paste("Per-candidate success probability:", input$p)),  
                  aes(x=input$vu, y=input$y, label=ratio_val()),
                  color="black",
                  size=3,
                  hjust=1.2,
                  vjust=-1)
    }
    else {
      #Figure 2: plot B
      #Create base plot
      ggplot(filtered_p2,
             aes(x = vu, y = y * input$qb, group = threshold, col = factor(threshold))) + geom_line() +
        facet_grid(p_fac~t_fac) +
        ylim(0, 10 * input$qb) + 
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_blank()) +
        scale_color_manual(name = "Benefit-risk\nthreshold", values = pal2) +
        labs(x = "Vaccine uptake",
             y = "Difference in trial length (y)",
             title= "QALY benefit-risk ratio")  +  
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.title = element_text(size = 14)) +
        
        #Plot a point that corresponds with user value
        geom_point(data=filtered_p2 %>%
                     filter(t_fac == paste("Number of candidates:", input$t) &
                              p_fac == paste("Per-candidate success probability:", input$p)),
                   aes(x=input$vu, y=input$y * input$qb),
                   color="red",
                   size=3,
                   show.legend=FALSE) +
        
        #Add label:
        geom_text(data=filtered_p2 %>%
                    filter(t_fac == paste("Number of candidates:", input$t) &
                             p_fac == paste("Per-candidate success probability:", input$p)),  
                  aes(x=input$vu, y=input$y * input$qb, label=ratio_val()),
                  color="black",
                  size=3,
                  hjust=1.2,
                  vjust=-1)
    }
    
  })
  
}



# Run the application
shinyApp(ui = ui, server = server)