#Data: Different Diseases/Burdens/Treatment
#This file will create one dataset for HPV, HepB, HCV

# set up: 
here::i_am("1_Scripts/0_preliminary_data.R")
source("global_options.R")

##### HPV Data

#existence of HPV campaign
#QUESTION: how do I use here if I'm going to 1_Scripts
hpv_campaign<-read.csv(here("0_Data/Raw_Data", "hpv_vaxprogram.csv"))

hpv_campaign_clean<-hpv_campaign %>%
  mutate(country=countrycode(COUNTRY, "iso3c", "country.name")) %>% #need country name for merge
  select(YEAR,country,Display.Value,REGION) %>%
  rename("HPV Vaccine Program"="Display.Value") %>%
  rename("yr"="YEAR") %>%
  arrange(country, yr) 

#hpv vaccination rates
hpv_vax<-read.csv(here("0_Data/Raw_Data","hpv.csv"))

hpv_vax_clean<- hpv_vax %>%
  select(Location, Period, Value) %>%
  rename("Percent Vaccinated HPV"="Value") %>%
  rename("yr"="Period") %>%
  rename("country"="Location") %>%
  arrange(country, desc(yr)) %>%
  mutate(country=countrycode(country, "country.name", "country.name"))  #to make sure all have same names

#hpv incidence
hpv_incidence<- read.csv(here("0_Data/Raw_Data", "hpv_incidence.csv"))

hpv_incidence_clean<- hpv_incidence %>%
  select(Population,ASR..World.) %>%
  rename("ASIR HPV"="ASR..World.") %>%
  rename("country"="Population") %>%
  mutate("yr"=2019) %>% # from 2020, changed for match
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  filter(!row_number() %in% 1) #don't want world rate

#hpv mortality
hpv_mortality<-read.csv(here("0_Data/Raw_Data","hpv_mortality.csv"))

hpv_mortality_clean<- hpv_mortality %>%
  select(Population, ASR..World.) %>%
  rename("country"="Population") %>%
  rename("ASmortality_rate_HPV"="ASR..World.") %>%
  filter(!row_number() %in% 1) %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate("yr"=2019) # from 2020, changed for match

##############
#HBV Data
#hepb vaccination (2000-2020)
hepb_vax<-read.csv(here("0_Data/Raw_Data", "hepb.csv"))

hepb_vax_clean<- hepb_vax %>%
  select(Location, Period, Value) %>%
  rename("country"="Location") %>%
  rename("hepb_vax%"="Value") %>%
  rename("yr"="Period") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) 


#hepb incidence (2010-2019 data) - IR per 100,000
hepb_incidence<-read.csv(here("0_Data/Raw_Data", "hepb_incidence.csv"))

hepb_incidence_clean <- hepb_incidence %>%
  select(location_name, year, val) %>%
  rename("country"="location_name") %>%
  rename("yr"="year") %>%
  rename("IR_hepb"="val") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) 

#############
#HepC Data
#HepC incidence per 100,000 (2015 and 2019)
hepc_incidence<-read.csv(here("0_Data/Raw_Data", "hepc_incidence.csv"))

hepc_incidence_clean <- hepc_incidence %>%
  select(location, year, val) %>%
  rename("country"="location") %>%
  rename("yr"="year") %>%
  rename("hepC_IR"="val") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) 

#HepC #initiated on treatent, annually newly diagnosed (chronic)
#note- only kept yr for initiated on treatment (yr's generally the same for both)
hepc_diag_tx<- read.csv(here("0_Data/Raw_Data", "hepc_correct_txDiag.csv"))

hepc_diag_tx_clean<- hepc_diag_tx %>%
  select(Country.Territory, Annual.Number.Initiated.on.Treatment, Year.of.Data, Annual.Newly.Diagnosed..Viremic.) %>%
  rename("country"="Country.Territory") %>%
  rename("yr"="Year.of.Data") %>%
  rename("Annual # Initiated HepC Treatment"="Annual.Number.Initiated.on.Treatment")%>%
  rename("Annual # Newly Diagnosed"="Annual.Newly.Diagnosed..Viremic.") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate(yr = as.numeric(yr),
         yr = ifelse(is.na(yr), 2018, yr))


##########
#population of each country: note- there are more "countries" here than in other datasets above
population_per_country<-read.csv(here("0_Data/Raw_Data", "population_per_country.csv"))


#definitely not the most efficient way...
population_clean<- population_per_country %>%
  select(Country.Name,X2000,X2001,X2002,X2003,X2004,X2005,X2006,X2007,X2008,X2009,
         X2010,X2011,X2012,X2013,X2014,X2015,X2016,X2017,X2018,X2019,X2020,X2021) %>%
  rename("country"= "Country.Name") %>%
  rename("2000"="X2000") %>%
  rename("2001"="X2001") %>%
  rename("2002"="X2002") %>%
  rename("2003"="X2003") %>%
  rename("2004"="X2004") %>%
  rename("2005"="X2005") %>%
  rename("2006"="X2006") %>%
  rename("2007"="X2007") %>%
  rename("2008"="X2008") %>%
  rename("2009"="X2009") %>%
  rename("2010"="X2010") %>%
  rename("2011"="X2011") %>%
  rename("2012"="X2012") %>%
  rename("2013"="X2013") %>%
  rename("2014"="X2014") %>%
  rename("2015"="X2015") %>%
  rename("2016"="X2016") %>%
  rename("2017"="X2017") %>%
  rename("2018"="X2018") %>%
  rename("2019"="X2019") %>%
  rename("2020"="X2020") %>%
  rename("2021"="X2021") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) 

population_clean_long<-pivot_longer(population_clean, cols="2000":"2020",names_to="yr", values_to = "population") %>%
  mutate(yr = as.numeric(yr))


##########
#COMBINE INTO ONE DATA FRAME
#this doesn't link mortality/incidence data well b/c it's for the yr 2020, and there aren't many 2020 measurements
all_list<-list(hpv_campaign_clean,hpv_vax_clean, hpv_incidence_clean, hpv_mortality_clean, hepb_vax_clean, hepb_incidence_clean, hepc_diag_tx_clean, hepc_incidence_clean, population_clean_long)

all_data <- all_list %>%
  reduce(full_join, by=c("country","yr"))

View(all_data) # why are there lots of NAs with 99% hep b vax?
View(table(all_data$country, all_data$yr)) # what's up with France?

