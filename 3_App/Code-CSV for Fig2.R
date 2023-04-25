
# ***********************************************************************

#CSV: for figure 2

# ***********************************************************************

#### SETUP ####
source("global_options.R")

#### PARAMETERS ####

# probability of success
p = c(seq(.05, 1, by = .05), .06, .11)

# number of tries
t = 1:5

# years saved if successful
y = c(2.5, 5, 7.5, 10)

# efficacy
e = c(.5, .7, .9)

# deployment
d = seq(.1, .9, by = .1)

# incidence of annual infections
i = c(1350000, 500000)

# combine parameters
df = expand_grid(p, t, y, e, d, i)

d = .03
R = 30
q = 0.8
n = 100

#### SIMULATION ####
for(i in 1:nrow(df)){
  
  # number of tries
  tries = 1:df$t[i]
  val = rep(0, length(tries))
  cost = rep(0, length(tries))
  
  # contribution to expectation if successful in j tries
  for(j in tries){
    val[j] = (1-d)^R*(1-df$p[i])^(j-1)*df$p[i]*(1-(1-d)^(j*df$y[i]))/(d)
    cost[j] = (1-df$p[i])^(j-1)*df$p[i]*(n*j - 1/2*n*df$e[i])
  }
  
  df$expected_years_saved[i] = sum(val)
  df$expected_years_saved_U[i] = sum(val) + (1-d)^R*(1-df$p[i])^(j)*min((1-(1-d)^(j*df$y[i]))/(d), 10)
  df$infs[i] = sum(cost) + (1-df$p[i])^(j)*j*n
  df$prob_success[i] = 1-(1-df$p[i])^(j)
}

df$benefit = df$expected_years_saved*df$d*df$e*df$i
df$ratio = df$benefit/df$infs


#### frontier plot

# get threshold plot
get_threshold = function(y, threshold, t,p,e,i,v,d = 0.03,n = 100,R = 30, base = T){
  # number of tries
  tries = 1:t
  val = rep(0, length(tries))
  cost = rep(0, length(tries))
  
  # contribution to expectation if successful in j tries
  for(j in tries){
    val[j] = (1-p)^(j-1)*p*(1-d)^R*(1-(1-d)^(j*y))/(d)
    cost[j] = (1-p)^(j-1)*p*(n*j - 1/2*n*e)
  }
  
  expected_years_saved = sum(val)
  expected_years_saved_U = sum(val) + (1-p)^(j)*(1-d)^R*min((1-(1-d)^(j*y))/(d), 10)
  infs = sum(cost) + (1-p)^(j)*j*n
  prob_success = 1-(1-p)^(j)
  
  benefit = expected_years_saved*v*e*i
  benefit2 = expected_years_saved_U*v*e*i
  
  ratio = benefit/infs
  ratio2 = benefit2/infs
  
  if(base == T){
    return(abs(ratio-threshold))
  }else{
    return(abs(ratio2-threshold))
  }
}

# run
df2 = expand_grid(threshold = c(50, 100, 250, 500, 1000, 2500),
                  e = c(.5, .7, .9), 
                  v = seq(.1, .9, by = .1),    #deployment
                  t = seq(1,10,by=1),
                  p = seq(.06, .4, by = .02),
                  y = NA,
                  d = c(0.03, .000000000001), #discount rate
                  i=seq(250000, 2000000, by=250000), 
                  base = c(T,F)) %>%
  mutate(t_fac = paste("Number of candidates:", t),
         p_fac = paste("Per-candidate success probability:", p), 
         p_fac = fct_rev(p_fac),
         chk = as.numeric(!base) + as.numeric(e!=.7) + 
           as.numeric(d!=.03) + as.numeric(i!=1350000)) #%>%
 # filter(chk <= 1)

for(i in 1:nrow(df2)) {
  df2$y[i] = optim(par = 5, fn = get_threshold, threshold = df2$threshold[i],
                   t = df2$t[i], p = df2$p[i], e = df2$e[i], d = df2$d[i], 
                   base = df2$base[i],
                   i = df2$i[i], v = df2$v[i], method = "BFGS")$par
}

View(df2)
#export df2 as csv:
write.csv(df2, "/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/3_App/data_fig2.csv", row.names=FALSE)

df2_final<-read.csv("/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/3_App/data_fig2.csv")

pal2 = c("#fbe392", "#fab24d", "#ec8400", "#d25700", "#b02912", "#311432", "#311572")

# base plot: THIS IS THE PLOT I WANT
#had to edit base, add v, t, p, edit i 
ggplot(df2_final %>% 
         filter(d == 0.03, e == 0.7, i == 250000, base==T, p %in% c(.06, 0.10), t %in% c(1,5)) %>%
         mutate(t_fac = paste("Number of candidates:", t),
                p_fac = paste("Per-candidate success probability:", p)),
                #p_fac = fct_rev(p_fac)),
       aes(x = v, y = y, group = threshold, col = factor(threshold))) + geom_line() + 
  facet_grid(p_fac~t_fac) + 
  ylim(0, 10) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) + 
  scale_color_manual(name = "Benefit-risk\nthreshold", values = pal2) + 
  labs(x = "Vaccine uptake", y = "Difference in trial length (y)")

