#### SETUP ####
source("global_options.R")

#### PARAMETERS ####

# probability of success
p = seq(.05, .4, by = .05)

# number of tries
t = 1:5

# years saved if successful
y = c(2.5, 5, 10)

# efficacy
e = c(.5, .7, .9)

# deployment
d = seq(.1, .9, by = .1)

# deployment
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

# run plots
df_plots = df %>% gather(var, value, prob_success,
              expected_years_saved, expected_years_saved_L,
              expected_years_saved_U) %>%
  mutate(var2 = ifelse(var=="prob_success", "Probability vaccine developed",
                       "Years saved (base)"),
         var2 = ifelse(var=="expected_years_saved_U", "Years saved (generous)", var2),
         var2 = ifelse(var=="expected_years_saved_L", "Years saved (conservative)", var2),
         y_lab = paste("Difference in trial length:", y),
         y_lab = factor(y_lab, levels = paste("Difference in trial length:", c(2.5, 5, 10))))

# plot results - Figure 1
ggplot(df_plots %>% filter(var%in%c("prob_success", "expected_years_saved")), 
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

# save results
ggsave(filename = here("2_Figures", "figure1.png"), width = 9, height = 5)

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

ggplot(df %>% filter(p %in% c(.05, .1) & y==5 & t %in% c(1,3)), 
       aes(x = d, y = ratio, group = paste(t,p), col = p, lty = factor(t))) + geom_line() + 
  facet_wrap(e~i) + ylim(0, 2500)


# deployment
# time difference

# efficacy
# incidence

# prob of success
# number of candidates
