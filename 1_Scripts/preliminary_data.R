#Data: Different Diseases/Burdens/Treatment
#This file will create one dataset for HPV, HepB, HCV

#setup
source("global_options.R")
install.packages("countrycode")
library(countrycode)

##### FIRST: HPV
#### STEP1: Read in data

#existence of HPV campaign
#QUESTION: am I supposed to read this in a different way?
hpv_campaign<-read.csv("/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/0_Data/Raw_Data/hpv_vaxprogram.csv")

hpv_campaign_clean<-hpv_campaign %>%
  mutate(country=countrycode(COUNTRY, "iso3c", "country.name")) %>% #need country name for merge
  select(YEAR,country,Display.Value,REGION) %>%
  rename("HPV Vaccine Program"="Display.Value") %>%
  rename("yr"="YEAR") %>%
  arrange(country, yr) 

#hpv vaccination rates
hpv_vax<-read.csv("/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/0_Data/Raw_Data/hpv.csv")

hpv_vax_clean<- hpv_vax %>%
  select(Location, Period, Value) %>%
  rename("Percent Vaccinated HPV"="Value") %>%
  rename("yr"="Period") %>%
  rename("country"="Location") %>%
  arrange(country, desc(yr)) %>%
  mutate(country=countrycode(country, "country.name", "country.name"))  #to make sure all have same names

#hpv incidence
hpv_incidence<- read.csv("/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/0_Data/Raw_Data/hpv_incidence.csv")

hpv_incidence_clean<- hpv_incidence %>%
  select(Population,ASR..World.) %>%
  rename("ASIR HPV"="ASR..World.") %>%
  rename("country"="Population") %>%
  mutate("yr"=2020) %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  filter(!row_number() %in% 1) #don't want world rate

#hpv mortality
hpv_mortality<-read.csv("/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/0_Data/Raw_Data/hpv_mortality.csv")

hpv_mortality_clean<- hpv_mortality %>%
  select(Population, ASR..World.) %>%
  rename("country"="Population") %>%
  rename("ASmortality_rate_HPV"="ASR..World.") %>%
  filter(!row_number() %in% 1) %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate("yr"=2020)

View(hpv_mortality_clean)

##############
#HBV Data
#hepb vaccination (2000-2020)
hepb_vax<-read.csv("/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/0_Data/Raw_Data/hepb.csv")

hepb_vax_clean<- hepb_vax %>%
  select(Location, Period, Value) %>%
  rename("country"="Location") %>%
  rename("hepb_vax%"="Value") %>%
  rename("yr"="Period") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) 


#hepb incidence (2010-2019 data) - IR per 100,000
hepb_incidence<- read.csv("/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/0_Data/Raw_Data/hepb_incidence.csv")

hepb_incidence_clean <- hepb_incidence %>%
  select(location_name, year, val) %>%
  rename("country"="location_name") %>%
  rename("yr"="year") %>%
  rename("IR_hepb"="val") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) 

#############
#HepC Data
#HepC incidence per 100,000 (2015 and 2019)
hepc_incidence<- read.csv("/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/0_Data/Raw_Data/hepc_incidence.csv")

hepc_incidence_clean <- hepc_incidence %>%
  select(location_name, year, val) %>%
  rename("country"="location_name") %>%
  rename("yr"="year") %>%
  rename("hepC_IR"="val") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) 
  
#HepC tested and treated (only 2015 data)
hepc_diag_treat<-read.csv("/Users/rachelslimovitch/Documents/22-23/Brown/Sem1/AB Research/HepC_Challenge/0_Data/Raw_Data/hepc_lancet_diagnosed_treated.csv")

hepc_diag_treat_clean<- hepc_diag_treat %>%
  rename("country" = "Country") %>%
  rename("% diagnosed HepC" = "Percentage.diagnosed") %>%
  rename("% treated HepC of diagnosed"="Percentage.treated.of.diagnosed") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate("yr"=2015)

##########
#COMBINE INTO ONE DATA FRAME
#this doesn't link mortality/incidence data well b/c it's for the yr 2020, and there aren't many 2020 measurements
all_list<-list(hpv_campaign_clean,hpv_vax_clean, hpv_incidence_clean, hpv_mortality_clean, hepb_vax_clean, hepb_incidence_clean, hepc_diag_treat_clean, hepc_incidence_clean)

all_data <- all_list %>%
  reduce(full_join, by=c("country","yr"))

View(all_data)
