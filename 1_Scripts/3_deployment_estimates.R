####************************ DEPLOYMENT ESTIMATES **************************####

# set up: 
here::i_am("1_Scripts/3_deployment_estimates.R")
source(here::here("1_Scripts", "0_preliminary_data.R"))

####***************************** HPV **************************************####
df = preliminary_data %>% filter(yr==2019) %>%
  mutate(rotavirus_incident_cases = as.numeric(sub(",", "", rotavirus_incident_cases)))
View(df) # R: go through countries w/NA population / remove or fill them in manually

#### weighted by population

# percentage of population covered
HPV_perc_global_pop = df %>% group_by(`HPV Vaccine Program`) %>%
  summarize(pop = sum(population, na.rm = T)) %>% 
  ungroup() %>%
  filter(!is.na(`HPV Vaccine Program`)) %>% # a few countries, affects outcome by < 0.001
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

HPV_perc_global_pop*HPV_perc_uptake

#### weighted by HPV burden

# percentage of population covered
HPV_perc_global_pop_BURDEN = df %>% 
  group_by(`HPV Vaccine Program`) %>%
  summarize(wt = sum(population*`IR HPV`, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(`HPV Vaccine Program`)) %>%
  mutate(perc = wt/sum(wt)) %>%
  filter(`HPV Vaccine Program`=="Yes") %>% dplyr::select(perc)

# percentage uptake
HPV_perc_uptake_BURDEN = df %>% filter(`HPV Vaccine Program`=="Yes") %>%
  filter(!is.na(`Percent Vaccinated HPV`)) %>%
  mutate(
    wt = population*`IR HPV`,
    pop_weight = wt/sum(wt, na.rm = T)) %>%
  summarize(perc_vax = sum(pop_weight*`Percent Vaccinated HPV`, na.rm = T))

HPV_perc_global_pop_BURDEN*HPV_perc_uptake_BURDEN


####***************************** HBV **************************************####
####*

# percentage covered
df %>% filter(!is.na(`hepb_vax%`) & !is.na(country)) %>% 
  summarize(sum(population, na.rm = T)) 
# pretty much all here!
# omissions in prelim script
# df %>% filter(is.na(`hepb_vax%`))

#### weighted by population
HBV_perc_uptake = df %>% filter(!is.na(`hepb_vax%`) & !is.na(country)) %>%
  mutate(pop_weight = population/sum(population, na.rm = T)) %>%
  summarize(perc_vax = sum(pop_weight*`hepb_vax%`, na.rm = T))

#### weighted by burden
HBV_perc_uptake_burden = df %>% filter(!is.na(`hepb_vax%`) & !is.na(country)) %>%
  mutate(pop_weight = (IR_hepb*population)/sum(IR_hepb*population, na.rm = T)) %>%
  summarize(perc_vax = sum(pop_weight*`hepb_vax%`, na.rm = T))

####***************************** ROTAVIRUS **************************************####

# percentage of population covered
RV_perc_global_pop = df %>% group_by(rotavirus_vaccination_program) %>%
  summarize(pop = sum(population, na.rm = T)) %>% 
  ungroup() %>%
  filter(!is.na(rotavirus_vaccination_program)) %>%
  mutate(perc = pop/sum(pop), tot = sum(pop)) %>% 
  filter(rotavirus_vaccination_program=="yes") %>% dplyr::select(perc)

# percentage uptake
RV_perc_uptake = df %>% filter(rotavirus_vaccination_program=="yes") %>%
  filter(!is.na(`rotavirus_vax%`)) %>%
  mutate(pop_weight = population/sum(population, na.rm = T)) %>%
  summarize(perc_vax = sum(pop_weight*`rotavirus_vax%`, na.rm = T))

# percentage of population covered BURDEN
RV_perc_global_pop*RV_perc_uptake
#View(df %>% filter(is.na(rotavirus_incident_cases)))
#View(df %>% filter(is.na(rotavirus_IR_per1000)))

RV_perc_global_pop_BURDEN = df %>% group_by(rotavirus_vaccination_program) %>%
  summarize(pop = sum(rotavirus_incident_cases, na.rm = T)) %>% 
  ungroup() %>%
  filter(!is.na(rotavirus_vaccination_program)) %>%
  mutate(perc = pop/sum(pop), tot = sum(pop)) %>% 
  filter(rotavirus_vaccination_program=="yes") %>% dplyr::select(perc)

# percentage uptake, burden
RV_perc_uptake_BURDEN = df %>% filter(rotavirus_vaccination_program=="yes") %>%
  filter(!is.na(`rotavirus_vax%`)) %>%
  mutate(pop_weight = rotavirus_incident_cases/sum(rotavirus_incident_cases, na.rm = T)) %>%
  summarize(perc_vax = sum(pop_weight*`rotavirus_vax%`, na.rm = T))

RV_perc_global_pop_BURDEN*RV_perc_uptake_BURDEN
