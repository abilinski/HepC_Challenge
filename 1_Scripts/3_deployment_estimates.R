####************************ DEPLOYMENT ESTIMATES **************************####

# set up: 
here::i_am("1_Scripts/3_deployment_estimates.R")
source(here::here("1_Scripts", "0_preliminary_data.R"))

####***************************** HPV **************************************####
df = all_data %>% filter(yr==2019)
View(df) # R: go through countries w/NA population / remove or fill them in manually

#### weighted by population

# percentage of population covered
HPV_perc_global_pop = df %>% group_by(`HPV Vaccine Program`) %>%
  summarize(pop = sum(population, na.rm = T)) %>% # Rachel: who are the NAs here?
  ungroup() %>%
  filter(!is.na(`HPV Vaccine Program`)) %>%
  mutate(perc = pop/sum(pop), tot = sum(pop)) %>% 
  filter(`HPV Vaccine Program`=="Yes") %>% dplyr::select(perc)

# percentage uptake
# df %>%  filter(`HPV Vaccine Program`=="Yes") %>% filter(is.na(`Percent Vaccinated HPV`))
# R: Do we know if there is anything special about these?
# Let's a bit of sensitivity analysis -- if we fill in...?
#df %>%  filter(`HPV Vaccine Program`=="Yes") %>% group_by(is.na(`Percent Vaccinated HPV`)) %>% 
#  summarize(pop = sum(population, na.rm = T)) %>%
#  mutate(perc = pop/sum(pop))
HPV_perc_uptake = df %>% filter(`HPV Vaccine Program`=="Yes") %>%
  filter(!is.na(`Percent Vaccinated HPV`)) %>%
  mutate(pop_weight = population/sum(population, na.rm = T)) %>%
  summarize(perc_vax = sum(pop_weight*`Percent Vaccinated HPV`, na.rm = T))

HPV_perc_global_pop; HPV_perc_uptake

#### weighted by HPV burden

# percentage of population covered
HPV_perc_global_pop_BURDEN = df %>% 
  group_by(`HPV Vaccine Program`) %>%
  summarize(wt = sum(population*`ASIR HPV`, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(`HPV Vaccine Program`)) %>%
  mutate(perc = wt/sum(wt)) %>%
  filter(`HPV Vaccine Program`=="Yes") %>% dplyr::select(perc)

# percentage uptake
HPV_perc_uptake_BURDEN = df %>% filter(`HPV Vaccine Program`=="Yes") %>%
  filter(!is.na(`Percent Vaccinated HPV`)) %>%
  mutate(
    wt = population*`ASIR HPV`,
    pop_weight = wt/sum(wt, na.rm = T)) %>%
  summarize(perc_vax = sum(pop_weight*`Percent Vaccinated HPV`, na.rm = T))

####***************************** HBV **************************************####
####*

# percentage covered
df %>% filter(!is.na(`hepb_vax%`) & !is.na(country)) %>% summarize(sum(population, na.rm = T))
# R: which are missing?

#### weighted by population

HBV_perc_uptake = df %>% filter(!is.na(`hepb_vax%`) & !is.na(country)) %>%
  mutate(pop_weight = population/sum(population, na.rm = T)) %>%
  summarize(perc_vax = sum(pop_weight*`hepb_vax%`, na.rm = T))

#### weighted by burden

HBV_perc_uptake_burden = df %>% filter(!is.na(`hepb_vax%`) & !is.na(country)) %>%
  mutate(pop_weight = (IR_hepb*population)/sum(IR_hepb*population, na.rm = T)) %>%
  summarize(perc_vax = sum(pop_weight*`hepb_vax%`, na.rm = T))

####***************************** DAAs **************************************####

hcv = all_data %>% filter(!is.na(`Annual # Initiated HepC Treatment`)) %>%
  mutate(treated = as.numeric(gsub(",", "", `Annual # Initiated HepC Treatment`)),
         treated = ifelse(is.na(treated), 0, treated),
         diagnosed = as.numeric(gsub(",|*", "", `Annual # Newly Diagnosed`))) 

####*************************** FIGURE 3 ***********************************####

HPV_level = HPV_perc_global_pop_BURDEN*HPV_perc_uptake_BURDEN
HBV_level = HBV_perc_uptake_burden

# format data for a plot
df_plot = expand_grid(eff = seq(.5, .9, by = .2),
                 years = c(1, 2.5, 5),
                 inc = c(1000000, 500000),
                 dep = c(seq(0.05, 1, by = .05), as.numeric(HPV_level)/100, as.numeric(HBV_level)/100)) %>%
  mutate(infs_averted = years*eff*inc*dep,
         inc_fac = ifelse(inc==1e6, "Annual incidence (200/m): \n Projected", "Incidence (100/m): \n Scaled-up prevention & treatment"),
         year_fac = paste("Years saved:", years),
         inc_fac = factor(inc_fac, levels = c("Annual incidence (200/m): \n Projected", "Incidence (100/m): \n Scaled-up prevention & treatment")))

df_plot2 = df_plot %>% filter(dep %in% c(HPV_level/100, HBV_level/100)) %>%
  mutate(lab = ifelse(dep==as.numeric(HPV_level)/100, "HPV", "HBV"))


# make plot
ggplot(df_plot, aes(x = dep, y = infs_averted/1000000, group = eff, col = factor(eff))) + geom_line() +
  facet_grid(inc_fac~year_fac) + labs(x = "Percentage of global population vaccinated",
                                   y = "Infections averted (m)") +
  scale_color_manual(name = "Vaccine efficacy", values = pal) + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  geom_point(data = df_plot2, aes(x = dep, y = infs_averted/1000000, group = eff, col = factor(eff))) + 
  geom_text(data = df_plot2 %>% filter(eff==.9), aes(x = dep, y = infs_averted/1000000+.5, label = lab), col = "black")
  
  
# save
ggsave(filename = here("2_Figures", "figure3.png"), width = 9, height = 6)

