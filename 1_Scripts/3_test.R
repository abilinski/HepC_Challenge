#### SETUP ####
source("global_options.R")
load(here("0_Data", "Processed_Data", "output.RData"))

#### STOCHASTIC VERSION OF MODEL ####

# other params
R = 30
n = 100
n_sims = 50000

#### PRIMARY MODEL SIMULATION ####
tic()
for(i in 1:nrow(df)){
  
  # store trial infections
  inf = matrix(0, n_sims, df$t[i])
  
  # store years saved
  years_saved = matrix(0, n_sims, df$t[i])
  
  # run multiple trials
  for(k in 1:n_sims){ 
    
    j = 1
    while(j <= df$t[i]){
    
    # did this trial work
    works_now = rbinom(1, size = 1, prob = df$p[i])
    inf[k,j] = ifelse(works_now, n/2 + n/2*(1-df$e[i]), n)
    
    years_saved[k,j] = ifelse(works_now, df$y[i]*j, 0)
    
    if(works_now) {
      j = df$t[i] + 1
    }else{j = j+1}
    
    }
  }
  
  df$trial_infs[i] = mean(rowSums(inf))
  df$years_total[i] = (1-df$d[i])^R*mean(rowSums(years_saved))
  df$years_disc[i] = (1-df$d[i])^R*mean((1-(1-df$d[i])^rowSums(years_saved))/(df$d[i]))

}

toc()

df$benefit_test = df$years_disc*df$e*df$v*df$i
df_test = df
df_test$ratio_test = df$benefit_test/df$trial_infs
save(df_test, file = here("0_Data", "Processed_Data", "test_data.RData"))

#### COMPARE MODELS ####
load(here("0_Data", "Processed_Data", "test_data.RData"))
df_test %>%
  group_by(p, e, t) %>%
  summarize(a = mean(trial_infs), b = mean(infs), a-b, diff = round(a-b)) %>%
  ungroup() %>% summarize(test = mean(diff == 0))

df_test %>% mutate(ratio_benefit = benefit/benefit_test) %>%
  summarize(min(ratio_benefit), max(ratio_benefit), mean(ratio_benefit), median(ratio_benefit))

#### CHECK THRESHOLDING ####
load(here("0_Data", "Processed_Data", "output2.RData"))
df = df2

for(i in 1:nrow(df)){
  
  # number of tries
  tries = 1:df$t[i]
  val = rep(0, length(tries))
  cost = rep(0, length(tries))
  
  # contribution to expectation if successful in j tries
  for(j in tries){
    val[j] = (1-df$d[i])^R*(1-df$p[i])^(j-1)*df$p[i]*(1-(1-df$d[i])^(j*df$y[i]))/(df$d[i])
    cost[j] = (1-df$p[i])^(j-1)*df$p[i]*(n*j - 1/2*n*df$e[i])
  }
  
  # years saved
  df$expected_years_saved[i] = sum(val)
  
  # years saved (generous)
  df$expected_years_saved_U[i] = sum(val) + (1-df$d[i])^R*(1-df$p[i])^(j)*min((1-(1-df$d[i])^(j*df$y[i]))/(df$d[i]), 10)
  
  # infections in trial
  df$infs[i] = sum(cost) + (1-df$p[i])^(j)*j*n
  
  # success probability
  df$prob_success[i] = 1-(1-df$p[i])^(j)
}

# calculate benefit
df$benefit = df$expected_years_saved*df$v*df$e*df$i

# calculate infection BRR
df$ratio = df$benefit/df$infs
df$ratio2 = (df$expected_years_saved_U*df$v*df$e*df$i)/df$infs

#### TEST 

# base simulations
# filter out non-plotted
chks = df %>% filter(y<=10 & base) %>%
       mutate(test_val = ratio/threshold) 


chks %>%
       group_by(threshold) %>%
       summarize(min(test_val), max(test_val), mean(test_val), median(test_val))

# non-base simulations
# filter out non-plotted
chks = df %>% filter(y<=10 & !base) %>%
  mutate(test_val = ratio2/threshold) 


chks %>%
  group_by(threshold) %>%
  summarize(min(test_val), max(test_val), mean(test_val), median(test_val))

