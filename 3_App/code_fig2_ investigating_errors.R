#############################################

#Checking code for Fig2
#input values have been edited to match the values in the paper

#PROBLEM:
# why do the plots not go up to y=10? they stop early

#############################################

  # get threshold fcn
  get_threshold = function(y, threshold, t,p,e,i,vu,dr,n,tr){
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
                    e = 0.7,  #vaccine efficacy
                    vu = seq(.1, .9, by = .05), #vaccine uptake
                    t = c(1,5), #number of trials
                    p = 0.06, #probability of success 
                    y = NA,
                    dr = 0.03, #discount rate
                    i= 1350000, #incidence
                    tr = 30, #time till rollout
                    n = 100) %>%  #number of people in trial arm
    
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
  filtered_plot2<-data.frame(
    t = df2$t,
    p = df2$p,
    vu = df2$vu,
    y = df2$y,
    threshold = df2$threshold
  )



  #create dataframe for output
  filtered_p2<- filtered_plot2 %>%
    mutate(t_fac = paste("Number of candidates:", t),
           p_fac = paste("Per-candidate success probability:", p),
           p_fac = fct_rev(p_fac))
  

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
      theme(plot.title = element_text(size = 14)) 
      
   
    