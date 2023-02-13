#### SETUP ####
source("global_options.R")

#### PARAMETERS ####

# probability of success
p = c(seq(.05, .4, by = .05), .11)

# number of tries
t = 1:5

# years saved if successful
y = c(2.5, 5, 10)

# efficacy
e = c(.5, .7, .9)

# deployment
d = seq(.1, .9, by = .1)

# incidence of annual infections
i = c(1000000, 500000)

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
  val_L = rep(0, length(tries))
  cost = rep(0, length(tries))
  
  # contribution to expectation if successful in j tries
  for(j in tries){
    val[j] = (1-df$p[i])^(j-1)*df$p[i]*(1-d)^R*(1-(1-d)^(j*df$y[i]))/(d)
    val_L[j] = min((1-(1-d)^(j*df$y[i]))/(d), 15)*(1-d)^R*(1-df$p[i])^(j-1)*df$p[i]
    cost[j] = (1-df$p[i])^(j-1)*df$p[i]*(n*j - 1/2*n*df$e[i])
  }
  
  df$expected_years_saved[i] = sum(val)
  df$expected_years_saved_L[i] = sum(val_L)
  df$expected_years_saved_U[i] = sum(val) + (1-df$p[i])^(j)*(1-d)^R*min((1-(1-d)^(j*df$y[i]))/(d), 10)
  df$infs[i] = sum(cost) + (1-df$p[i])^(j)*j*n
  df$prob_success[i] = 1-(1-df$p[i])^(j)
}

df$benefit = df$expected_years_saved*df$d*df$e*df$i
df$ratio = df$benefit/df$infs
View(df)

#export df to csv
write.csv(df, "/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/3_App/data.csv", row.names=FALSE)


pal = c("#fbe392", "#fab24d", "#ec8400", "#d25700", "#b02912", "#311432")


# run plots
df_plots = df %>% gather(var, value, prob_success,
              expected_years_saved, expected_years_saved_L,
              expected_years_saved_U) %>%
  mutate(var2 = ifelse(var=="prob_success", "Probability vaccine developed",
                       "Expected years saved"),
         var2 = ifelse(var=="expected_years_saved_U", "Years saved (generous)", var2),
         var2 = ifelse(var=="expected_years_saved_L", "Years saved (conservative)", var2),
         y_lab = paste("Difference in trial length:", y),
         y_lab = factor(y_lab, levels = paste("Difference in trial length: ", c(2.5, 5, 10), "y", sep = "")))

# plot results - Figure 1
b = ggplot(df_plots %>% filter(var%in%c("expected_years_saved")), 
       aes(x = p, y = value, col = factor(t), group = t)) + 
  geom_line() + 
  scale_color_manual(name = "Maximum number of challenge trials\n(available candidates)", values = pal) + 
  facet_grid(.~y_lab, scales = "free") + theme_bw() + 
  ylim(0, NA) + 
  labs(x = "Probability of success in each trial", y = "",
       title = "Expected years saved (discounted)") + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
          panel.background = element_blank()) +
  geom_vline(xintercept = 0.06, lty = 2, col = "grey") + 
  geom_vline(xintercept = 0.12, lty = 3, col = "grey") 

a = ggplot(df_plots %>% filter(var%in%c("prob_success")), 
       aes(x = p, y = value, col = factor(t), group = t)) + 
  geom_line() + 
  scale_color_manual(name = "Maximum number of challenge trials\n(available candidates)", values = pal) + 
  theme_bw() + 
  ylim(0, NA) + 
  labs(x = "Probability of success in each trial", y = "",
       title = "Probability vaccine developed") + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = 0.06, lty = 2, col = "grey") + 
  geom_vline(xintercept = 0.12, lty = 3, col = "grey") + ylim(0,1)

ggarrange(a, b, common.legend = T, ncol = 1, legend = "bottom")
# save results
ggsave(filename = here("2_Figures", "figure1.png"), width = 10, height = 6)

# plot results - Figure S1
ggplot(df_plots %>% filter(var%in%c("expected_years_saved_U")), 
       aes(x = p, y = value, col = factor(t), group = t)) + 
  geom_line() + 
  scale_color_manual(name = "Maximum number of \nchallenge trials \n(available candidates)", values = pal) + 
  facet_grid(var2~y_lab, scales = "free") + theme_bw() + 
  ylim(0, NA) + 
  labs(x = "Probability of success in each trial", y = "") + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) + 
  geom_vline(xintercept = 0.068, lty = 2, col = "grey") + 
  geom_vline(xintercept = 0.11, lty = 3, col = "grey") + 
  geom_vline(xintercept = 0.4, lty = 4, col = "grey") 


ggsave(filename = here("2_Figures", "figure_s1.png"), width = 9, height = 5)


#### risks and benefits plot
ggplot(df %>% filter(e==.7 & i == 1e6 & t %in% c(1,3,5) & p %in% c(.05, .1)) %>%
         mutate(t_fac = paste("Number of candidates:", t),
                p_fac = paste("Per-candidate success probability:", p)),
       aes(x = d, y = benefit/1e6, group = paste(y, p), col = factor(y))) +
  facet_grid(p_fac~t_fac) + 
  geom_line() +
  scale_color_manual(name = "Difference in trial length (y)", values = pal) + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x = "Vaccine uptake", y = "Future infections averted (m, discounted)") 

#### frontier plot

# get threshold plot
get_threshold = function(y, threshold, t,p,e,i,v,d = 0.03,n = 100,R = 30){
  # number of tries
  tries = 1:t
  val = rep(0, length(tries))
  val_L = rep(0, length(tries))
  cost = rep(0, length(tries))
  
  # contribution to expectation if successful in j tries
  for(j in tries){
    val[j] = (1-p)^(j-1)*p*(1-d)^R*(1-(1-d)^(j*y))/(d)
    val_L[j] = min((1-(1-d)^(j*y))/(d), 15)*(1-d)^R*(1-p)^(j-1)*p
    cost[j] = (1-p)^(j-1)*p*(n*j - 1/2*n*e)
  }
  
  expected_years_saved = sum(val)
  expected_years_saved_L = sum(val_L)
  expected_years_saved_U = sum(val) + (1-p)^(j)*(1-d)^R*min((1-(1-d)^(j*y))/(d), 10)
  infs = sum(cost) + (1-p)^(j)*j*n
  prob_success = 1-(1-p)^(j)
  
  benefit = expected_years_saved*v*e*i
  ratio = benefit/infs
  
  return(abs(ratio-threshold))
}

df2 = data.frame(e = .7, i=1000000) %>% 
  expand_grid(threshold = c(50, 100, 250, 500, 1000, 2500),
              v = seq(.01, .9, by = .001),
              t = c(1,3,5),
              p = c(0.06, 0.12),
              y = NA) %>%
  mutate(t_fac = paste("Number of candidates:", t),
         p_fac = paste("Per-candidate success probability:", p))

for(i in 1:nrow(df2)) {
  df2$y[i] = optim(par = 5, fn = get_threshold, threshold = df2$threshold[i],
                   t = df2$t[i], p = df2$p[i], e = df2$e[i],
                   i = df2$i[i], v = df2$v[i], method = "BFGS")$par
}


ggplot(df2, 
       aes(x = v, y = y, group = threshold, col = factor(threshold))) + geom_line() + 
  facet_grid(p_fac~t_fac) + 
  ylim(0, 10) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) + 
  scale_color_manual(name = "Benefit-risk\nthreshold", values = pal) +
  annotate('rect', xmin = 0, xmax = .2, ymin = 0, ymax = 5,
           alpha = .2, fill = "darkgrey") +
  annotate('rect', xmin = .2, xmax = .9, ymin = 0, ymax = 5,
           alpha = .4, fill = "darkgrey") +
  labs(x = "Vaccine uptake", y = "Difference in trial length (y)")


ggsave(filename = here("2_Figures", "figure3.png"), width = 9, height = 6)




# deployment
# time difference

# efficacy
# incidence

# prob of success
# number of candidates
