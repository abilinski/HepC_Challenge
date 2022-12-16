#### SETUP ####
source("global_options.R")

#### PARAMETERS ####

# probability of success
p = seq(0, 1, by = .05)

# number of tries
t = 1:5

# years saved if successful
y = c(2.5, 5, 10)

# combine parameters
df = expand_grid(p, t, y)

#### SIMULATION ####
for(i in 1:nrow(df)){

  # number of tries
  tries = 1:df$t[i]
  val = rep(0, length(tries))
  val_L = rep(0, length(tries))
  
  
  # contribution to expectation if successful in j tries
  for(j in tries){
    val[j] = df$y[i]*j*(1-df$p[i])^(j-1)*df$p[i]
    val_L[j] = min(df$y[i]*j, 15)*(1-df$p[i])^(j-1)*df$p[i]
  }
  
  df$expected_years_saved[i] = sum(val)
  df$expected_years_saved_L[i] = sum(val_L)
  df$expected_years_saved_U[i] = sum(val) + min(df$y[i]*(1-df$p[i])^(j), 10)
  df$prob_success[i] = 1-(1-df$p[i])^(j)
}

View(df)

# run plots
df_plots = df %>% gather(var, value, prob_success,
              expected_years_saved, expected_years_saved_L,
              expected_years_saved_U) %>%
  mutate(var2 = ifelse(var=="prob_success", "Probability vaccine developed",
                       "Years saved (base)"),
         var2 = ifelse(var=="expected_years_saved_U", "Years saved (generous)", var2),
         var2 = ifelse(var=="expected_years_saved_L", "Years saved (conservative)", var2))

# plot results - Figure 1
ggplot(df_plots %>% filter(var%in%c("prob_success", "expected_years_saved")), 
       aes(x = p, y = value, col = factor(t), group = t)) + 
  geom_line() + 
  scale_color_manual(name = "Maximum number of \nchallenge trials \n(available candidates)", values = pal) + 
  facet_grid(var2~y, scales = "free") + theme_bw() + 
  ylim(0, NA) + 
  labs(x = "Probability of success in each trial", y = "") + 
  theme(panel.grid.minor = element_blank(),
          panel.background = element_blank())

# save results
ggsave(filename = here("2_Figures", "figure1.png"), width = 9, height = 5)

# plot results - Figure S1
ggplot(df_plots %>% filter(var%in%c("expected_years_saved_L", "expected_years_saved_U")), 
       aes(x = p, y = value, col = factor(t), group = t)) + 
  geom_line() + 
  scale_color_manual(name = "Maximum number of \nchallenge trials \n(available candidates)", values = pal) + 
  facet_grid(var2~y, scales = "free") + theme_bw() + 
  ylim(0, NA) + 
  labs(x = "Probability of success in each trial", y = "") + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave(filename = here("2_Figures", "figure_s1.png"), width = 9, height = 5)
