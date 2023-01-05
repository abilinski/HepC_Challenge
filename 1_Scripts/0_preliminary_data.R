#Data: Different Diseases/Burdens/Treatment
#This file creates one dataset for HPV, HepB, HCV, rotavirus

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
  arrange(country, yr) %>%
  mutate(yr=as.numeric(yr))


#hpv vaccination rates
hpv_vax<-read.csv(here("0_Data/Raw_Data","hpv.csv"))

hpv_vax_clean<- hpv_vax %>%
  select(Location, Period, Value) %>%
  rename("Percent Vaccinated HPV"="Value") %>%
  rename("yr"="Period") %>%
  rename("country"="Location") %>%
  arrange(country, desc(yr)) %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate(yr=as.numeric(yr))

#hpv incidence
hpv_incidence<- read.csv(here("0_Data/Raw_Data", "hpv_incidence.csv"))

hpv_incidence_clean<- hpv_incidence %>%
  select(Population,Crude.Rate) %>%
  rename("IR HPV"="Crude.Rate") %>%
  rename("country"="Population") %>%
  mutate("yr"=2019) %>% # from 2020, changed for match
  subset(country!="France, Guadeloupe" & country!="France, Martinique" & country!="France, New Caledonia" & !grepl("France, La", country)) %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  filter(!row_number() %in% 1) %>%
  mutate(yr=as.numeric(yr))


#hpv mortality
hpv_mortality<-read.csv(here("0_Data/Raw_Data","hpv_mortality.csv"))

hpv_mortality_clean<- hpv_mortality %>%
  select(Population, Crude.Rate) %>%
  rename("country"="Population") %>%
  rename("mortality_rate_HPV"="Crude.Rate") %>%
  filter(!row_number() %in% 1) %>%
  subset(country!="France, Guadeloupe" & country!="France, Martinique" & country!="France, New Caledonia" & !grepl("France, La", country)) %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate("yr"=2019) %>% # from 2020, changed for match
  mutate(yr=as.numeric(yr))

##############
#HBV Data
#hepb vaccination (2000-2020)
hepb_vax<-read.csv(here("0_Data/Raw_Data", "hepb.csv"))

hepb_vax_clean<- hepb_vax %>%
  select(SpatialDimValueCode, Period, Value) %>%
  rename("country"="SpatialDimValueCode") %>%
  rename("hepb_vax%"="Value") %>%
  rename("yr"="Period") %>%
  mutate(country=countrycode(country, "iso3c", "country.name")) %>%
  mutate(yr=as.numeric(yr))

#hepb incidence (2010-2019 data) - IR per 100,000
hepb_incidence<-read.csv(here("0_Data/Raw_Data", "hepb_incidence.csv"))

hepb_incidence_clean <- hepb_incidence %>%
  select(location_name, year, val) %>%
  rename("country"="location_name") %>%
  rename("yr"="year") %>%
  rename("IR_hepb"="val") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate(yr=as.numeric(yr))

#############
#HepC Data
#HepC incidence per 100,000 (2015 and 2019)
hepc_incidence<-read.csv(here("0_Data/Raw_Data", "hepc_incidence.csv"))

hepc_incidence_clean <- hepc_incidence %>%
  select(location, year, val) %>%
  rename("country"="location") %>%
  rename("yr"="year") %>%
  rename("hepC_IR"="val") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate(yr=as.numeric(yr))

#HepC incidence number (2016 through 2019)
hepc_incidence_number<-read.csv(here("0_Data/Raw_Data", "HepC_incidenceNumber.csv"))

hepc_incidence_number_clean <- hepc_incidence_number %>%
  select(location, year, val) %>%
  rename("country"="location") %>%
  rename("yr"="year") %>%
  rename("acute_HepC_incidence_number"="val") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate(country=ifelse(country=="Taiwan (Province of China)", "Taiwan", country)) %>%
  mutate(yr=as.numeric(yr))

#HepC initiated on treatment, annually newly diagnosed (chronic)
hepc_diag_tx<- read.csv(here("0_Data/Raw_Data", "hepc_correct_txDiag.csv"))

hepc_diag_tx_clean<- hepc_diag_tx %>%
  select(Country.Territory, Annual.Number.Initiated.on.Treatment, Year.of.Data, Annual.Newly.Diagnosed..Viremic.) %>%
  rename("country"="Country.Territory") %>%
  rename("yr"="Year.of.Data") %>%
  rename("Annual_Num_Initiated_HepC_Treatment"="Annual.Number.Initiated.on.Treatment")%>%
  rename("Annual_Num_Newly_Diagnosed"="Annual.Newly.Diagnosed..Viremic.") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate(yr=as.numeric(yr))

hepc_diag_tx_clean<- hepc_diag_tx_clean %>%
  mutate(Annual_Num_Initiated_HepC_Treatment= as.numeric(gsub("," ,"",Annual_Num_Initiated_HepC_Treatment))) %>%
  mutate(Annual_Num_Newly_Diagnosed=as.numeric(gsub("," ,"",Annual_Num_Newly_Diagnosed))) %>%
  mutate(Annual_Num_Initiated_HepC_Treatment=ifelse(country %in% c("China", "Hong Kong SAR China"),
                                                    Annual_Num_Initiated_HepC_Treatment[country="China"]+
                                                    Annual_Num_Initiated_HepC_Treatment[country="Hong Kong SAR China"], Annual_Num_Newly_Diagnosed)) %>%
  filter(!country=="Hong Kong SAR China")


#HepC: % of total world cirrhosis, per country (incidence) *only did for 2019 but could do more
hepc_cirrhosis<- read.csv(here("0_Data/Raw_Data", "hepc_cirrhosis.csv"))

global_cirrhosis2019<-551688.53

hepc_cirrhosis_clean <- hepc_cirrhosis %>%
  select(location, year, val) %>%
  rename("country"="location") %>%
  rename("yr"="year") %>%
  rename("cirrhosis_cases"="val") %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate(cirrhosis_cases=as.numeric(cirrhosis_cases)) %>%
  mutate("cirrhosis_%global"=cirrhosis_cases/global_cirrhosis2019)%>%
  select(-cirrhosis_cases) %>%
  mutate(yr=as.numeric(yr))


##########
#Rotavirus
#Rotavirus vaccine
rotavirus_vax<- read.csv(here("0_Data/Raw_Data", "rotavirus_vaccinated.csv"))

rotavirus_vax_clean<- rotavirus_vax %>%
  subset(COVERAGE_CATEGORY!="ADMIN" & COVERAGE_CATEGORY!="OFFICIAL") %>%
  mutate(CODE=countrycode(CODE, "iso3c", "country.name"))  %>%
  select(YEAR, COVERAGE, CODE) %>%
  rename("country"="CODE") %>%
  rename ("yr"="YEAR") %>%
  rename("rotavirus_vax%"="COVERAGE") %>%
  subset(country!="NA") %>%
  mutate(yr=as.numeric(yr))

#Rotavirus incidence rate - am going to work on this on Sunday!
rotavirus_incidence<-read.csv(here("0_Data/Raw_Data", "rotavirus_incidence.csv"))

rotavirus_incidence_clean <- rotavirus_incidence %>%
  select(Location, Incidence.per.1.000..95..UI., Cases..95..UI.) %>%
  rename("country"="Location") %>%
  rename("rotavirus_IR_per1000"="Incidence.per.1.000..95..UI.") %>%
  mutate(rotavirus_IR_per1000=as.numeric(gsub("," ,"",rotavirus_IR_per1000))) %>%
  rename("rotavirus_incident_cases"="Cases..95..UI.") %>%
  mutate(rotavirus_incident_cases =as.numeric(gsub("," ,"",rotavirus_incident_cases))) %>%
  mutate("yr"=2016) %>%
  subset(country!="") %>%
  add_row(country="United Kingdom", rotavirus_IR_per1000="144.9", rotavirus_incident_cases="586,884", yr=2016) %>% 
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate(yr=as.numeric(yr))

#Rotavirus vaccination campaign
rotavirus_vax_campaign<- read.csv(here("0_Data/Raw_Data", "rotavirus_vax_campaign.csv"))

rotavirus_vax_campaign_clean <- rotavirus_vax_campaign %>%
  select(Benind, Non.Introducer) %>%
  rename("country"="Benind") %>%
  rename("rotavirus_vaccination_program"="Non.Introducer") %>%
  mutate(rotavirus_vaccination_program = if_else(rotavirus_vaccination_program=="Introducer", "yes", "no" )) %>%
  mutate(country=countrycode(country, "country.name", "country.name")) %>%
  mutate("yr"=2016) %>%
  mutate(yr=as.numeric(yr))

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
  mutate(yr = as.numeric(yr)) %>%
  select(country, yr, population)

#population Taiwan
taiwan_population<-read.csv(here("0_Data/Raw_Data", "taiwan_population.csv"))

taiwan_population_clean<- taiwan_population %>%
  rename("yr"="Year") %>%
  rename("population"="Population") %>%
  mutate(country="Taiwan") %>%
  select(-Annual...Change)

#population China and Hong Kong
china_population<-read.csv(here("0_Data/Raw_Data", "china_population.csv"))

china_population_clean<- china_population %>%
  filter(Country.Name=="China") %>%
  rename("country"="Country.Name") %>%
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
  pivot_longer(cols="2000":"2020", names_to="yr", values_to = "population") %>%
  mutate(yr = as.numeric(yr)) %>%
  select(country,yr,population)

population_list<-list(population_clean_long,taiwan_population_clean, china_population_clean)

population_data<- population_list %>%
  reduce(full_join, by=c("yr","population","country")) %>%
  mutate(yr=as.numeric(yr))

##########
#COMBINE INTO ONE DATA FRAME
#this doesn't link mortality/incidence data well b/c it's for the yr 2020, and there aren't many 2020 measurements
all_list<-list(hpv_campaign_clean,hpv_vax_clean, hpv_incidence_clean, hpv_mortality_clean, hepb_vax_clean, hepb_incidence_clean, hepc_diag_tx_clean, 
               hepc_incidence_clean, hepc_cirrhosis_clean, hepc_incidence_number_clean, rotavirus_incidence_clean, rotavirus_vax_clean, rotavirus_vax_campaign_clean, population_data)

all_data <- all_list %>%
  reduce(full_join, by=c("country","yr"))


##########
#Final clean data frame, no NA population or country

preliminary_data = all_data %>% 
  drop_na(population) %>%
  drop_na(country) %>%
  subset(country!="Aruba" & country!="Cayman Islands" & country!="Faroe Islands" & country!="Gibraltar" & country!="Hong Kong SAR China" &
           country!="Isle of Man" & country!="Liechtenstein" & country!="Macao SAR China" & country!= "Saint Martin (French part)" &
           country!="New Caledonia" & country!="French Polynesia" & country!="Sint Maarten" & country!="Turks & Caicos Islands" &
           country!="British Virgin Islands" & country !="Channel Islands" & country!="Kosovo" & !grepl("Cura", country)) 
  
View(preliminary_data)
