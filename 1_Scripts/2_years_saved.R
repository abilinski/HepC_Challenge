#### SETUP ####
source("global_options.R")

#### PRIMARY PARAMETERS ####

# probability of success
p = c(seq(.05, .4, by = .05), .07, .11)

# number of tries
t = 1:5

# years saved if successful
y = c(2.5, 5, 10)

# efficacy
e = c(.5, .7, .9)

# deployment
v = seq(.05, .9, by = .05)

# incidence of annual infections
i = c(1350000, 1350000/2)

# discounting
d = c(.03, .000000000001)

# combine parameters
df = expand_grid(p, t, y, e, v, d, i)

# other params
R = 30
n = 100

#### PRIMARY MODEL SIMULATION ####
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
df$ratio2 = (df$expected_years_saved_U*df$d*df$e*df$i)/df$infs

#### FIGURES ####
pal = c("#fbe392", "#fab24d", "#ec8400", "#d25700", "#b02912", "#311432")

# reformat data
df_plots = df %>% gather(var, value, prob_success,
              expected_years_saved, 
              expected_years_saved_U) %>%  
  mutate(var2 = ifelse(var=="prob_success", "Probability vaccine developed",
                       "Years saved (base case)"),
         var2 = ifelse(var=="expected_years_saved_U", "Years saved (generous)", var2),
         y_lab = paste("Difference in trial length: ", y, "y", sep = ""),
         y_lab = factor(y_lab, levels = paste("Difference in trial length: ", c(2.5, 5, 10), "y", sep = ""))) %>%
  filter(d==0.03)

# success probabilities
a = ggplot(df_plots %>% filter(var%in%c("prob_success")), 
       aes(x = p, y = value, col = factor(t), group = t)) + 
  geom_line() + 
  scale_color_manual(name = "Maximum number of challenge trials\n(available candidates)", values = pal) + 
  theme_bw() + 
  ylim(0, NA) + 
  labs(x = "Probability of success in each trial", y = "Probability of successful vaccine development\n(any successful trial)",
       title = "") + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = 0.07, lty = 3, col = "grey") + 
  geom_vline(xintercept = 0.11, lty = 2, col = "grey") + ylim(0,1)

# save results
ggsave(a, filename = here("2_Figures", "figure_prob.png"), width = 10, height = 3)

# expected years saved
b = ggplot(df_plots %>% filter(var%in%c("expected_years_saved", "expected_years_saved_U")), 
           aes(x = p, y = value, col = factor(t), group = t)) + 
  geom_line() + 
  scale_color_manual(name = "Maximum number of challenge trials\n(available candidates)", values = pal) + 
  facet_grid(var2~y_lab, scales = "free") + theme_bw() + 
  ylim(0, NA) + 
  labs(x = "Probability of success in each trial", y = "Expected years saved (discounted)",
       title = "") + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = 0.07, lty = 3, col = "grey") + 
  geom_vline(xintercept = 0.11, lty = 2, col = "grey") 

ggsave(b, filename = here("2_Figures", "figure_ey.png"), width = 10, height = 6)

#### expected infections averted
df_plots2 = df %>% filter(e==.7 & i == 1350000 & t %in% c(1,3,5) & p %in% c(.07, .11) & d == 0.03)
ggplot(df_plots2 %>%
         mutate(t_fac = paste("Number of candidates:", t),
                p_fac = paste("Per-candidate success probability:", p),
                p_fac = fct_rev(p_fac)),
       aes(x = v, y = benefit/1e6, group = paste(y, p), col = factor(y))) +
  facet_grid(p_fac~t_fac) + 
  geom_line() +
  scale_color_manual(name = "Difference in\ntrial length (y)", values = pal[c(1,3,5)]) + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x = "Vaccine uptake", y = "Future infections averted (m, discounted)") 

ggsave(filename = here("2_Figures", "figure_ben.png"), width = 9, height = 6)
ggsave(filename = here("2_Figures", "figure_ben.tiff"), width = 9, height = 6)

#### expected infections averted - monetary terms
ggplot(df_plots2 %>%
         mutate(t_fac = paste("Number of candidates:", t),
                p_fac = paste("Per-candidate success probability:", p),
                p_fac = fct_rev(p_fac)),
       aes(x = v, y = benefit/1e6*500, group = paste(y, p), col = factor(y))) +
  facet_grid(p_fac~t_fac) + 
  geom_line() +
  scale_color_manual(name = "Difference in\ntrial length (y)", values = pal[c(1,3,5)]) + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x = "Vaccine uptake", y = "Costs of achieving future benefits through treatment ($m)") 

ggsave(filename = here("2_Figures", "figure_ben_monetary.png"), width = 9, height = 6)


#### FRONTIER ANALYSIS ####

# get threshold functions
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

# set up params
df2 = expand_grid(threshold = c(50, 100, 250, 500, 1000, 2500),
              e = c(.7, .9), 
              v = seq(.01, .9, by = .001),
              t = c(1,3,5),
              p = c(0.07, 0.11),
              y = NA,
              d = c(0.03, .000000000001),
              i=c(1350000, 1350000/2), 
              base = c(T,F)) %>%
  mutate(t_fac = paste("Number of candidates:", t),
         p_fac = paste("Per-candidate success probability:", p), 
         p_fac = fct_rev(p_fac),
         chk = as.numeric(!base) + as.numeric(e!=.7) + 
           as.numeric(d!=.03) + as.numeric(i!=1350000)) %>%
  filter(chk <= 1)

for(i in 1:nrow(df2)) {
  temp = optim(par = 5, fn = get_threshold, threshold = df2$threshold[i],
                   t = df2$t[i], p = df2$p[i], e = df2$e[i], d = df2$d[i], 
                   base = df2$base[i],
                   i = df2$i[i], v = df2$v[i])

  df2$y[i] = temp$par
  df2$converged[i] = temp$value
}

#### FRONTIER FIGURES ####

# BRRs
ggplot(df2 %>% filter(d == 0.03, e == 0.7, i == 1350000 & base), 
       aes(x = v, y = y, group = threshold, col = factor(threshold))) + geom_line() + 
  facet_grid(p_fac~t_fac) + 
  ylim(0, 10) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) + 
  scale_color_manual(name = "Benefit-risk\nthreshold", values = pal) +
  labs(x = "Vaccine uptake", y = "Difference in trial length (y)")

ggsave(filename = here("2_Figures", "figure_BRR.png"), width = 9, height = 6)

# QALY BRRs
ggplot(df2 %>% filter(d == 0.03, e == 0.7, i == 1350000 & base) %>%
         mutate(threshold = comma(threshold*100),
                threshold = factor(threshold, c("5,000", "10,000", "25,000", "50,000", "100,000", "250,000"))), 
       aes(x = v, y = y, group = threshold, col = (threshold))) + geom_line() + 
  facet_grid(p_fac~t_fac) + 
  ylim(0, 10) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) + 
  scale_color_manual(name = "Benefit-risk\nthreshold (QALY)", values = pal) +
  labs(x = "Vaccine uptake", y = "Difference in trial length (y)")

ggsave(filename = here("2_Figures", "figure_BRR_QALY.png"), width = 9, height = 6)
ggsave(filename = here("2_Figures", "figure_BRR_QALY.tiff"), width = 9, height = 6)

# sens plot (no discount)
ggplot(df2 %>% filter(d < 0.03, e == 0.7, i == 1350000) %>%
         mutate(threshold = comma(threshold*100),
                threshold = factor(threshold, c("5,000", "10,000", "25,000", "50,000", "100,000", "250,000"))), 
       aes(x = v, y = y, group = threshold, col = factor(threshold))) + geom_line() + 
  facet_grid(p_fac~t_fac) + 
  ylim(0, 10) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) + 
  scale_color_manual(name = "Benefit-risk\nthreshold", values = pal) +
  labs(x = "Vaccine uptake", y = "Difference in trial length (y)")

ggsave(filename = here("2_Figures", "figure_BRR_no_discount.png"), width = 9, height = 6)

# sens plot (no discount)
ggplot(df2 %>% filter(d == 0.03, e == 0.9, i == 1350000) %>%
         mutate(threshold = comma(threshold*100),
                threshold = factor(threshold, c("5,000", "10,000", "25,000", "50,000", "100,000", "250,000"))), 
       aes(x = v, y = y, group = threshold, col = factor(threshold))) + geom_line() + 
  facet_grid(p_fac~t_fac) + 
  ylim(0, 10) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) + 
  scale_color_manual(name = "Benefit-risk\nthreshold", values = pal) +
  labs(x = "Vaccine uptake", y = "Difference in trial length (y)")

ggsave(filename = here("2_Figures", "figure_BRR_high_eff.png"), width = 9, height = 6)

# sens plot (half incidence)
ggplot(df2 %>% filter(d == 0.03, e == 0.7, i < 1350000) %>%
         mutate(threshold = comma(threshold*100),
                threshold = factor(threshold, c("5,000", "10,000", "25,000", "50,000", "100,000", "250,000"))), 
       aes(x = v, y = y, group = threshold, col = factor(threshold))) + geom_line() + 
  facet_grid(p_fac~t_fac) + 
  ylim(0, 10) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) + 
  scale_color_manual(name = "Benefit-risk\nthreshold", values = pal) +
  labs(x = "Vaccine uptake", y = "Difference in trial length (y)")

ggsave(filename = here("2_Figures", "figure_BRR_lower_inc.png"), width = 9, height = 6)

# sens plot (half rate)
ggplot(df2 %>% filter(d == 0.03, e == 0.7, i == 1350000 & !base) %>%
         mutate(threshold = comma(threshold*100),
                threshold = factor(threshold, c("5,000", "10,000", "25,000", "50,000", "100,000", "250,000"))), 
       aes(x = v, y = y, group = threshold, col = factor(threshold))) + geom_line() + 
  facet_grid(p_fac~t_fac) + 
  ylim(0, 10) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) + 
  scale_color_manual(name = "Benefit-risk\nthreshold", values = pal) +
  labs(x = "Vaccine uptake", y = "Difference in trial length (y)")

ggsave(filename = here("2_Figures", "figure_BRR_alt.png"), width = 9, height = 6)

#### TEXT ####
# success probability
df %>% filter(t %in% c(1,3,5) & p %in% c(.07, .11)) %>% 
  dplyr::select(p, t, prob_success) %>% unique()

# outcomes
df %>% filter(t %in% c(3) & p %in% c(.07, .11) & i == 1350000 & 
                y == 5 & e == .7 & v %in% c(.2, .9) & d == 0.03) %>% 
  dplyr::select(p, t, v, benefit, infs, ratio) %>% unique() %>%
  mutate(QALY_ratio = ratio*100, cost = benefit*500/1000000, per_inf = cost/infs*1000000)


# varying number of candidates
df %>% filter(t %in% c(1,5) & p %in% c(.11) & i == 1350000 & 
                y == 5 & e == .7 &  v %in% c(.2, .9) & d == 0.03) %>% 
  dplyr::select(p, t, v, benefit, infs, ratio) %>% unique()

# range of ratios
min(df_plots2$ratio)
max(df_plots2$ratio)

# IQRs
quantile(df_plots2$ratio, c(.25, .75))*100
quantile(df_plots2$ratio[df_plots2$p%in%c(.11)], c(.25, .75))*100
quantile(df_plots2$ratio[df_plots2$p%in%c(.07)], c(.25, .75))*100

# impact of BRR assumptions - discounting
k = df %>% filter(p %in% c(.07, .11) & i == 1350000) %>% 
  group_by(p,t,v,e,y) %>% summarize(diff = ratio[1]/ratio[2], n = length(ratio))
median(k$diff)

# impact of BRR assumptions - generous
g = df %>% filter(p %in% c(.07, .11) & i == 1350000 & d==0.03) %>% 
       mutate(diff = ratio2/ratio)

1/median(g$diff)

# ranges
mean(df_plots2$ratio>50)
mean(df_plots2$ratio<2500)
mean(df_plots2$ratio[df_plots2$v>.5]>1000)
