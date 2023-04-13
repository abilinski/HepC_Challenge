
# ***********************************************************************

#Creating CSV for app

# ***********************************************************************


#### SETUP ####
source("global_options.R")

#### PARAMETERS ####

# probability of success
p = seq(.06, .4, by = .02)

# number of tries
t = 1:10

# years saved if successful
y = c(2.5, 5, 7.5, 10)

# efficacy
e = c(.5, .7, .9)

# deployment
d = seq(.1, .9, by = .1)

# incidence of annual infections
i = seq(250000, 2000000, by=250000)

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

#export df to csv
write.csv(df, "/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/3_App/data_final.csv", row.names=FALSE)

