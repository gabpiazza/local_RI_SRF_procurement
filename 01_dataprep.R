# Info --------------------------------------------------------------------
##
##Script name: 01_dataprep
##
##Purpose of script: Preparing the data for the paper "Where the God Particles touches the ground - The local economic impact of RI procurement " 
##
##Author: Gabriele Piazza
##
##Date Created: 2023-02-20
## Purpose of the script: Preparing the data for the analysis
##Copyright (c) Gabriele Piazza, 2023
##Email: g.piazza@lse.ac.uk 
##


## Notes:
##   
##


# 1. Setting up -----------------------------------------------------------



#1.1 Install/Load packages ---------------------------------------------------


testing_changestt
# i_am("scripts/01_dataprep.R")
options(scipen = 999)
need<- c("devtools", "remotes", "kbal", "stringi","foreign","haven","sf","eeptools","data.table","readxl" ,"tjbal", "Hmisc", "skimr", "tabulator", "easycsv", "janitor", "tidyverse")
have <- need %in% rownames(installed.packages()) # check packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages
`%notin%` <- Negate(`%in%`)
# require("devtools")
# install.packages("remotes")
# install.packages("kbal")
# install.packages("stringi")
# install.packages("eeptools")
# devtools::install_github("chadhazlett/KBAL")
# remotes::install_github("xuyiqing/tjbal")
# install.packages("Hmisc")
# install.packages("skimr")
# install.packages("tabulator")
# packages <- c("easycsv", "janitor", "tidyverse")
# library(easycsv)
# library(janitor)
# library(eeptools)
# library(stringi)
# library(foreign)
# library(haven)
# library(sf)
# library(readxl)
# library(data.table)
# setwd(here())
# `%notin%` <- Negate(`%in%`)
# Load data ---------------------------------------------------------------


# 1.2 Setting directories------------------------------------------------

data_raw_dir<- "/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_proc/"
asia_data_dir<- paste0(data_raw_dir, "ASIA_data/")
asia_data_2004_11_dir<- paste0(asia_data_dir, "2004-2011/csv/")
asia_data_2012_18_dir<- paste0(asia_data_dir, "2012-2018/")


## Load Employees data ---------------------------------------------------
load_csv <- function(file_path) {
  library(data.table)
  fread(file_path, encoding = "Latin-1")
}


# 1.3 Loading data------------------------------------------------

for (year in 2004:2011) {
  file_path <- paste0(asia_data_2004_11_dir,"addetti_", year, ".csv")
  df <- load_csv(file_path)
  df <- clean_names(df)
  
  # Assign the cleaned data frame to a variable in the global environment
  assign(paste0("addetti_", year), df, envir = .GlobalEnv)
}

# # data_dir<- "ASIA_data/2004-2012"
# test<- loadcsv_multi(asia_data_2004_12_dir, extension ="both") # this loads all the csv files and save them separately 
# dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) # get a list of all the dataframes in the global environment

# Loop through the years 2004 to 2011

for (year in 2004:2011) {
  file_path <- here(data_ASIA, paste0("addetti_", year, ".csv"))
  df <- load_csv(file_path)
  df <- clean_names(df)
  
  # Assign the cleaned data frame to a variable in the global environment
  assign(paste0("addetti_", year), df, envir = .GlobalEnv)
}


### Load 2004-2006 ------------------------------------------------------------




addetti_2004<- load_csv(here(data_ASIA,"addetti_2004.csv"))
addetti_2004<- clean_names(addetti_2004)
addetti_2005<- load_csv(here(data_ASIA, "addetti_2005.csv"))
addetti_2005<-clean_names(addetti_2005)
addetti_2005<- addetti_2005 %>% rename(codice_provincia =codice)
addetti_2006<- load_csv(here(data_ASIA, "addetti_2006.csv"))
addetti_2006<-clean_names(addetti_2006)
addetti_2006$year <- 2006 # the year variable was missing
addetti_2004<- addetti_2004 %>% select(c(names(addetti_2006)))
addetti_2005<- addetti_2005 %>% select(c(names(addetti_2006)))
addetti_2004_2006<-list(addetti_2004, addetti_2005, addetti_2006) # put them together into one list
addetti_2004_2006<-lapply(addetti_2004_2006, clean_names) # names in a nice format
addetti_2004_2006<-rbindlist(addetti_2004_2006, fill=T) # put all the datasets together
addetti_2004_2006<- addetti_2004_2006 %>% select(-codice_provincia, -codice_comune) %>% 
  mutate_at(c(2:15), as.numeric)# some columns are charachater
addetti_2004_2006<- addetti_2004_2006 %>% rename(mining= c, manufacturing =d, electricity_gas_water = e, construction = f,
                                                 wholesale_retail = g, hotels_restaurant= h, transport = i,
                                                 finance = j, business_activities= k, education = m, health_social=n) %>% select(-o) # I have to do this as there was a change in the codes from 2002 to 2007

# The issue here is that they use different definitions. I have used the classification from this website https://www.businessballs.com/glossaries-and-terminology/industrial-classifications-uk-sic-codes/ this refers to the UK SIC code but it should be the same as the Italian one

### Load 2007-2011 ------------------------------------------------------------

# This version is cleaner but I am still working - use for now from 109 onwards
#combine_data <- function(path, years) {
  #dfs <- lapply(years, function(yr) {
   # file <- paste0("addetti_", yr, ".csv")
   # df <- fread(file.path(path, file))
    #df <- clean_names(df)
   # df$year <- yr
   # if (yr == 2009) {
   #   df <- df %>% rename(denominazione_comune = denominazione)
   # } else if (yr == 2011) {
   #   df <- df %>% rename(denominazione_comune = v3)
  #  }
  #  df %>% select(names(addetti_2007))
 # })
 # combined <- rbindlist(dfs)
 # combined <- combined %>% select(-codice_provincia, -codice_comune) %>% mutate_at(c(2:19), decomma) %>% mutate_at(c(2:19), as.numeric)
 # combined <- combined %>% rename(mining= b, manufacturing = c, hotels_restaurant=i, transport= h, finance= k, education = p, health_social= q,
   #                            #   construction = f, wholesale_retail = g) %>% mutate(electricity_gas_water= d+e, business_activities = j+l+m+n)
 # return(combined)
#}
#data_path <- here("data_raw","ASIA_data", "2004-2012","csv")
#years <- c(2007, 2008, 2009, 2010, 2011)
#addetti_2007_2011 <- combine_data(data_path, years)

addetti_2007<- load_csv(here(data_ASIA, "addetti_2007.csv"))
addetti_2007<- clean_names(addetti_2007)
addetti_2007$year<- 2007
addetti_2008<- load_csv(here(data_ASIA,"addetti_2008.csv"))
addetti_2008<-clean_names(addetti_2008)
addetti_2008$year<- 2008
addetti_2008<- addetti_2008 %>% select(c(names(addetti_2007)))
addetti_2009<- load_csv(here(data_ASIA, "addetti_2009.csv"))
addetti_2009<-clean_names(addetti_2009)
addetti_2009<- addetti_2009 %>% rename(denominazione_comune = denominazione)
addetti_2009$year<- 2009
addetti_2009<- addetti_2009 %>% select(c(names(addetti_2007)))
addetti_2010<- load_csv(here(data_ASIA, "addetti_2010.csv"))
addetti_2010<-clean_names(addetti_2010)
addetti_2010$year<- 2010
addetti_2010<- addetti_2010 %>% select(c(names(addetti_2007)))
addetti_2011<- load_csv(here(data_ASIA, "addetti_2011.csv"))
addetti_2011<-clean_names(addetti_2011)
addetti_2011<- addetti_2011 %>% rename(denominazione_comune = v3)
addetti_2011$year<- 2011
addetti_2011<- addetti_2011 %>% select(c(names(addetti_2007)))

addetti_2007_2011<- list(addetti_2007, addetti_2008, addetti_2009, addetti_2010, addetti_2011) # create a list
addetti_2007_2011<-rbindlist(addetti_2007_2011) # bind them together
addetti_2007_2011<- addetti_2007_2011 %>% select(-codice_provincia, -codice_comune) %>% mutate_at(c(2:19), decomma) # the values with thousands had commas and this make it harder 
addetti_2007_2011<- addetti_2007_2011 %>% mutate_at(c(2:19), as.numeric)# some columns are character
addetti_2007_2011<- addetti_2007_2011 %>% rename(mining= b, manufacturing = c, hotels_restaurant=i, transport= h, finance= k, education = p, health_social= q,
                                                 construction = f, wholesale_retail = g)#chage the names of the variable so that they match those for 2004-2008
addetti_2007_2011 <- addetti_2007_2011 %>% mutate(electricity_gas_water= d+e, business_activities = j+l+m+n) # same as above

## Bind 2004-2011 
addetti_2007_2011<- addetti_2007_2011 %>% select(c(names(addetti_2004_2006)))

addetti_2004_2011<-rbind(addetti_2004_2006, addetti_2007_2011)
addetti_2004_2011<- addetti_2004_2011 %>% select(-mining)


### Load 2012-2018 ------------------------------------------------------------
#to load the files, you should do the same as above using fread

data_dir<- here("data_raw", "ASIA_data","2012-2018")
loadcsv_multi(data_dir, extension ="both") # this loads all the csv files and save them separately 
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) # get a list of all the dataframes in the global environment

library(data.table)

load_csv <- function(file_path) {
  fread(file_path, encoding = "Latin-1")
}

data_dir <- here("data_raw", "ASIA_data", "2012-2018")

# Function to load multiple CSV files and save them separately

loadcsv_multi <- function(data_dir, extension="csv"){
  file_list <- list.files(path = data_dir, pattern = paste0("\\.", extension, "$"), full.names = TRUE)
  dfs <- lapply(file_list, function(x) fread(x, encoding = "Latin-1"))
  combined_df <- rbindlist(dfs)
  return(combined_df)
}

addetti_2012_2018 <- loadcsv_multi(data_dir, extension = "csv")
addetti_2012_2018<- addetti_2012_2018 %>% rename(codice_comune= code, denominazione_comune= territorio, totale="10" )
addetti_2012_2018<- clean_names(addetti_2012_2018)
addetti_2012_2018<- addetti_2012_2018 %>% rename(manufacturing = c, hotels_restaurant=i, transport= h, finance= k, education = p, health_social= q, construction = f, wholesale_retail = g)
addetti_2012_2018 <- addetti_2012_2018 %>% mutate(electricity_gas_water= d+e, business_activities = j+l+m+n)
addetti_2012_2018<- addetti_2012_2018 %>% select(c(names(addetti_2004_2011)))

### Create the 2004-2017 panel -------------------------------------------

# 2004-2018 This binds all the data togetehr 

addetti_2004_2018<-rbind(addetti_2004_2011, addetti_2012_2018)
addetti_2004_2018$denominazione_comune<- str_to_upper(addetti_2004_2018$denominazione_comune) # it should remove the accent but it does not manage
addetti_2004_2018[,denominazione_comune:= stri_trans_general(str=denominazione_comune, id="Latin-ASCII")] #
addetti_2004_2018$denominazione_comune<- stri_trans_general(str= addetti_2004_2018$denominazione_comune, id="Latin-ASCII")
addetti_2004_2018<- addetti_2004_2018 %>% rename(municipality = denominazione_comune)


# Addetti north 
# Load lookup for North 

comuni_north <- load_csv(here("data_raw","Comuni", "comuni_lookup.csv"))
colnames(comuni_north)<- c("municipality", "area", "region")
comuni_north$municipality<- str_to_upper(comuni_north$municipality)
comuni_north <-as.data.frame(comuni_north)

comuni_north$municipality<- iconv(comuni_north$municipality, from = "UTF-8", to = "ASCII//TRANSLIT")
comuni_north$municipality <- gsub("`", "", comuni_north$municipality)
addetti_north <- left_join(addetti_2004_2018, comuni_north)
addetti_north<- addetti_north %>% drop_na(area)
schio<- addetti_north %>% filter(municipality=="SCHIO")
counting_obs <- addetti_north %>% group_by(municipality) %>% tally ()
addetti_north<- left_join(addetti_north, counting_obs)
addetti_north <- addetti_north %>% filter(n>13)
addetti_north_full<- addetti_north %>% select( -totale, -area)
#addetti_north_full<-addetti_north_full %>% drop_na()

# Upload CENSUS data for 2011
census_2011<- load_csv(here("data_raw","census_2011_comuni","all_comuni_selected.csv"))
census_2011<- clean_names(census_2011)
census_2011<- census_2011 %>% rename (year = anno_cp, municipality= city)
census_2011$municipality<- str_to_upper(census_2011$municipality)
census_2011$municipality <- stri_trans_general(str= census_2011$municipality, id="Latin-ASCII")


#Upload CERN data
cern_data <- fread(here("data_raw","census_2011_comuni","suppliers_italy_city.csv"))
cern_data <- cern_data %>% rename(cern_procurement = total_CHF, year= order_date, municipality = city)
cern_data_2004 <- cern_data %>% filter(year>2003)

# Get all the data together

northern_municipalities <- left_join(addetti_north_full, census_2011, by=c("municipality")) %>% rename(year = year.x)
northern_municipalities<- left_join(northern_municipalities, cern_data_2004, by= c("municipality", "year"))
northern_municipalities<- northern_municipalities %>% drop_na(pop_2011)
northern_municipalities <- northern_municipalities %>% drop_na (emploment_rate_2011)
list_full_cities <- unique(northern_municipalities$municipality)
northern_municipalities_full<- northern_municipalities %>% filter(municipality %in% list_full_cities)
write.csv(northern_municipalities_full,here("data_proc", "northern_municipalities_all.csv"))


#keep all the mid-size cities
mid_size_northern_municipalities <- northern_municipalities_full %>% filter(pop_2011>10000)
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% filter(pop_2011<100000)
medium_cities<- mid_size_northern_municipalities$municipality


mid_size_northern_municipalities<- northern_municipalities_full %>% filter(municipality %in% medium_cities)
mid_size_northern_municipalities<- unique(mid_size_northern_municipalities)
write.csv(mid_size_northern_municipalities,here("data_proc", "mid_size_northern_municipalities.csv"))


# I am a bit worried that the treatment effect could be a result of subsequent CERN contract.
# For this reason I exclude all the cities with no contracts post 2012. I then look at all the cities that had a contract between 10 million and 100,000 Swiss franc
# over this period. This decision is a bit arbitrary.  

cern_comuni<- mid_size_northern_municipalities %>% filter(year>2010, cern_procurement>0)# this filters all the comunis that have received a contract post 2011
cern_comuni <- cern_comuni %>% 
  group_by(municipality) %>% 
  summarise(cern_procurement = sum(cern_procurement))

cern_comuni<- cern_comuni %>% 
  filter(cern_procurement<10000000, cern_procurement>100000)# I do this because Schio received a small contract during this period. 
cern_municipalities<- cern_comuni$municipality # this gives me  a list of cities 
north_panel_final<-mid_size_northern_municipalities %>% filter(municipality %notin% cern_municipalities)

### Wage data ---------------------------------------------------------------

folder_files<- list.files(recursive = T, full.names= T,pattern = "\\.csv$",here("data_raw", "irpef_data"))
readsubfolder <- function (f){attempt <- fread(f)}
files_folder_extraction<- sapply(folder_files, readsubfolder, simplify = F)
list2env(files_folder_extraction,envir=.GlobalEnv)
selected_columns<- c("Anno di imposta", "Denominazione Comune", "Reddito da lavoro dipendente - Frequenza","Reddito da lavoro dipendente - Ammontare in euro" )


#Clean income data 
names(files_folder_extraction) <- paste0("income_", seq(from = 2000, to = 2018, by = 1)) # rename the different dataframes
files_folder_extraction[[7]]<- files_folder_extraction[[7]] %>% rename(`Reddito da lavoro dipendente - Ammontare in euro` = `Reddito da lavoro dipendente  - Ammontare in euro`,
                                       `Reddito da lavoro dipendente - Frequenza`= `Reddito da lavoro dipendente  - Frequenza`)  # change variable names to make it consistent
files_folder_extraction[[9]]<- files_folder_extraction[[9]] %>% rename(`Reddito da lavoro dipendente e assimilati - Ammontare` = `Reddito da lavoro dipendente e assimilati - Ammontare in euro`)
                      
income_2000_07 <- files_folder_extraction[1:8] # extract the first 8 years - same variable names
income_2008_14 <- files_folder_extraction[9:15] # extract 2008-14 years - same variable names
income_2015_18 <- files_folder_extraction[16:19] # rest of years - similar case 

income_2000_07 <- lapply(income_2000_07, function(df) {
  df_select <- df %>%
    select(1:12) %>% 
    mutate(average_employee_income =`Reddito da lavoro dipendente - Ammontare in euro`/`Reddito da lavoro dipendente - Frequenza` ) %>% 
    select(c("Anno di imposta", "Denominazione Comune", "average_employee_income"))# select columns 
  return(df_select)
})

income_2008_14<- lapply(income_2008_14, function(df) {
  df_select <- df %>%
    select(1:12) %>% 
    mutate(average_employee_income =`Reddito da lavoro dipendente e assimilati - Ammontare`/`Reddito da lavoro dipendente e assimilati - Frequenza` ) %>% 
    select(c("Anno di imposta", "Denominazione Comune", "average_employee_income"))# select columns 
  return(df_select)
})

income_2015_18<- lapply(income_2015_18, function(df) {
  df_select <- df %>%
    select(1:12) %>% 
    mutate(average_employee_income =`Reddito da lavoro dipendente e assimilati - Ammontare in euro`/`Reddito da lavoro dipendente e assimilati - Frequenza` ) %>% 
    select(c("Anno di imposta", "Denominazione Comune", "average_employee_income"))# select columns 
  return(df_select)
})

income_2000_07 <- do.call(rbind, income_2000_07)
income_2008_14 <- do.call(rbind, income_2008_14)
income_2015_18 <- do.call(rbind, income_2015_18)


### Put all the data together 
wage_data_2000_2018<- rbind(income_2000_07, income_2008_14, income_2015_18)
colnames(wage_data_2000_2018)<- c("year", "municipality", "average_employee_income")
wage_data_2000_2018$municipality <- gsub("'", "", wage_data_2000_2018$municipality)
write.csv(wage_data_2000_2018, here("data_proc", "wage_data_2000_2018.csv"))


# Prepare the data for analysis -------------------------------------------

#mid_size_northern_municipalities<- mid_size_northern_municipalities %>% select(-V1)
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% drop_na(manufacturing, business_activities, education, health_social, 
                                                                                pop_2011, pop_2001_2011, electricity_gas_water, high_skilled_share, pop_density_2011,
                                                                                young_pop_degree_share) %>% select(-V1)
mid_size_northern_municipalities[is.na(mid_size_northern_municipalities)]<-0


#### Remove the municipalities that have received large CERN order --------



mid_size_northern_municipalities_no_cern <- mid_size_northern_municipalities %>%dplyr::select(municipality, year, cern_procurement, region) %>% dplyr::filter(year >2009)
mid_size_cern<- mid_size_northern_municipalities_no_cern %>% dplyr::group_by(municipality) %>% 
mutate(total_cern = sum(cern_procurement)) %>% 
filter(municipality != "SCHIO") %>% 
filter(total_cern>1000000) 
cern_cities <- unique(mid_size_cern$municipality)
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% 
  filter(municipality %notin% cern_cities)
#filter(municipality %notin% c("CHIVASSO","GRUGLIASCO", "LEINI","RIVALTA DI TORINO","RIVOLI","BUSTO ARSIZIO", "CERNUSCO SUL NAVIGLIO","CASTEL D'AZZANO","TORRI DI QUARTESOLO","VIGONZA","SCANDIANO","CALDERARA DI RENO", "SANREMO"))# SANREMO HAS no data on wages


#### Remove the municipalities that are much larger/smaller than  --------


population_threshold<- mid_size_northern_municipalities %>% dplyr::filter(year==2011)
population_threshold<- population_threshold %>% filter(municipality=="SCHIO") %>% 
  select(pop_2011)

#population_threshold#39131

population_cities <- subset(mid_size_northern_municipalities, pop_2011>= 0.5 *39131 & pop_2011<=1.5*39131)
population_cities<- population_cities %>% select(municipality) %>% unique()
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% filter(municipality %in% population_cities$municipality)

# Create the tradable and non tradable variables

mid_size_northern_municipalities<- mid_size_northern_municipalities %>% 
  mutate(tradable = manufacturing + finance + business_activities + electricity_gas_water,
  non_tradable = construction + wholesale_retail + transport, #+ education + health_social,
  log_manufacturing = log(manufacturing),
  log_tradable= log(tradable),
  log_non_tradable = log(non_tradable))

mid_size_northern_municipalities_wage <- left_join(mid_size_northern_municipalities, wage_data_2000_2018) %>% 
  filter(municipality != "SANREMO") %>% drop_na(average_employee_income)

# There has been a big road development in 2017 and I don't want to capture this
#Load SSL lookup
sll <- fread(here("data_proc" ,"sll_2011.csv"))
sll<- clean_names(sll)
sll<- sll %>% rename (city = comune_2011) %>% dplyr::select(-pop_2011) 
sll$city <- stri_trans_general(str= sll$city, id="Latin-ASCII") # this gets rid of all the accents
sll_region_lookup <- sll %>% select(city, den_sll_2011) # select only the variables of interest
write_csv(sll_region_lookup, here("data_proc", "sll_region_lookup.csv"))
north_panel <- read_csv(here("data_proc", "north_panel_final.csv"))### I have to find out how I got this 

city_region <-north_panel %>% select(city, region) %>% unique()# select only the variables from city region
sll_region_lookup <- left_join(sll_region_lookup, city_region)
sll_region_lookup<- sll_region_lookup %>% drop_na()
sll_region_lookup <-sll_region_lookup %>% unique()

sll_region_lookup<- sll_region_lookup %>% select(den_sll_2011, region, city)
sll_region_lookup<- sll_region_lookup %>% unique()
schio_sll <- sll_region_lookup %>% filter(den_sll_2011== "SCHIO")
no_schio_sll<- schio_sll %>% filter(city !="SCHIO") %>% rename(municipality= city)
no_schio_sll<- no_schio_sll$municipality

# Before starting with the estimation, I want to exclude all the municipalities that are neighbouring Schio to avoid spillover effects
 mid_size_northern_municipalities_wage<- mid_size_northern_municipalities_wage %>% filter(!municipality %in% no_schio_sll)

### Create file here 
 

 #2012 treatment 
mid_size_northern_municipalities_wage<- mid_size_northern_municipalities_wage %>% mutate(treat_2012 = case_when(municipality =="SCHIO" & year>2012 ~ 1, year<2013 & municipality =="SCHIO"~ 0,
                                                                                                           municipality!="SCHIO"~0))# This creates a treatment variable

# 2010 treatment 
mid_size_northern_municipalities_wage<- mid_size_northern_municipalities_wage %>% mutate(treat_2010 = case_when (municipality =="SCHIO" & year> 2010 ~1, year<2011 & municipality =="SCHIO" ~ 0, 
                                                                                                            municipality !="SCHIO"~0))# This creates a treatment variable

mid_size_northern_municipalities_wage<- mid_size_northern_municipalities_wage %>% drop_na(average_employee_income)
mid_size_northern_municipalities_2004_17 <- mid_size_northern_municipalities_wage %>% filter(year<2018) 
mid_size_northern_municipalities_2004_17<- unique(mid_size_northern_municipalities_2004_17)
counting_obs <- mid_size_northern_municipalities_2004_17 %>% group_by(municipality) %>% tally ()
full_list_cities <- counting_obs %>% filter(n==14)

mid_size_northern_municipalities_2004_17<- mid_size_northern_municipalities_2004_17 %>% 
  filter(municipality %in% full_list_cities$municipality) %>% 
  select(-year.y,-n, -region )

mid_size_northern_municipalities_2004_17<- unique(mid_size_northern_municipalities_2004_17)

write.csv(mid_size_northern_municipalities_2004_17, here("data_proc", "mid_size_northern_municipalities.csv"))

#Load orbis data and joining the data---------------------------------------------------------------

# Load data

italy_address_v <- read_dta(here("data_raw","ORBIS","Address IT.dta")) # load the addresses
colnames(italy_address_v)<- c("bvd_id_number", "postcode", "city", "city_native") #change the column names 
italy_address_v <- italy_address_v %>% mutate(check = city == city_native)# this is not necessary - it just checks whether the city and city native variables are the same.
italy_address_v <- italy_address_v %>% filter(city !="None")
italy_gdp_deflator_2020<- read_csv(here("data_raw", "GDP_deflator", "Italy_GDP_deflated_2020.csv"))
italy_gdp_deflator_2020<- clean_names(italy_gdp_deflator_2020)

#it_sales <- read_dta("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_FINANCIALS/Fin IT.dta")
# Load the NACE data. it was initially in STATA 15 and issues with the haven package
italy_nace <- read.csv(here("data_raw","ORBIS", "NACE IT.csv"))
italy_nace <- italy_nace %>% rename(bvd_id_number = bvdidnumber)

# Load the Orbis data
it_orbis <- read_dta(here("data_raw", "ORBIS", "Gabriele IT.dta"))
# Load postocodes
#it_postcodes <- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/listacomuni 2.csv")
#it_postcodes<- clean_names(it_postcodes)
#it_postcodes<- it_postcodes %>% rename(postcode= cap) %>% select(postcode, comune)

# Joining the different datasets together 
it_orbis_address<- left_join(it_orbis, italy_address_v)
it_orbis_address <- it_orbis_address %>% select(bvd_id_number,city,consolidation_code, filing_type,  number_of_employees, operating_revenue_turnover_, p_l_before_tax, p_l_after_tax,
                                                ebitda, postcode, closing_date)


rm(italy_address_v, it_orbis) # I remove the file as I don't have much space
#it_orbis_address_postcode<- left_join(it_orbis_address, it_postcodes) # Match with the postcodes
it_orbis_address <- it_orbis_address %>% filter(!is.na(postcode)) %>% select(-number_of_employees,-p_l_before_tax, p_l_after_tax) # I get rid of some variables and drop those without postcodes

italy_nace <- italy_nace %>% select(bvd_id_number, nacerev2primarycodes) %>% filter(!is.na(nacerev2primarycodes))
#rm(it_orbis, italy_address_v)# Already removed
it_orbis_address_nace <- left_join(it_orbis_address, italy_nace) # Join with the NACE code
rm(italy_nace) # remove the italy_nace
rm(it_orbis_address) # remove the it orbis address to get more space
#rm(it_orbis_address_postcode)
it_orbis_address_nace<- it_orbis_address_nace %>% mutate(two_digit = floor(nacerev2primarycodes/100)) # I do this to get better codes
it_orbis_address_nace <- it_orbis_address_nace %>% filter(!is.na(operating_revenue_turnover_))
it_orbis_address_nace_nona <- it_orbis_address_nace%>% drop_na(closing_date)
rm(it_orbis_address_nace)

# Assign the date following Kalemli-Ozcan et al.  -------------------------

it_orbis_address_nace_nona<- it_orbis_address_nace_nona %>% 
  mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z"),
         closing_date_format = as.Date(closing_date_format)) 

it_orbis_address_nace_nona<- it_orbis_address_nace_nona %>% 
  mutate(year_orbis = year(closing_date_format))

it_orbis_address_nace_nona<- it_orbis_address_nace_nona %>% 
  mutate(month_orbis = month(closing_date_format))

#We re-construct the YEAR variable based on the following convention. 
#If the closing date is after or on June 1st, the current year is assigned (if CLOSEDATE is 4th of August, 2003, the year is 2003). Otherwise, 
#the previous year is assigned (if CLOSEDATE is 25th of May, 2003, the year is 2002)

it_orbis_address_nace_nona$year[it_orbis_address_nace_nona$month_orbis<6]<-it_orbis_address_nace_nona$year_orbis -1
it_orbis_address_nace_nona$year[it_orbis_address_nace_nona$month_orbis>5]<-it_orbis_address_nace_nona$year_orbis
#it_orbis_address_nace_nona<- it_orbis_address_nace_nona %>% select(-comune)
it_orbis_address_nace_nona<- it_orbis_address_nace_nona %>% distinct()
it_orbis_address_nace_nona<- it_orbis_address_nace_nona %>% filter(!is.na(city))
it_orbis_address_nace_nona <- it_orbis_address_nace_nona%>% drop_na(closing_date)
it_orbis_address_nace_nona_2018 <- it_orbis_address_nace_nona %>% 
  filter(year >2000 & year <2020)
rm(it_orbis_address_nace_nona)

it_orbis_address_nace_nona_2018<- it_orbis_address_nace_nona_2018 %>% select(-nacerev2primarycodes) %>% distinct()
it_orbis_address_nace_nona_2018<- it_orbis_address_nace_nona_2018 %>% distinct(bvd_id_number, year, consolidation_code, operating_revenue_turnover_, .keep_all = TRUE)
it_orbis_address_nace_nona_2018<- it_orbis_address_nace_nona_2018 %>% group_by(bvd_id_number, year) %>% mutate(number_of_filings = n()) %>% ungroup()
it_orbis_address_nace_nona_2018$consolidation_l <- substr(it_orbis_address_nace_nona_2018$consolidation_code,1,1)
it_orbis_address_nace_nona_2018<- it_orbis_address_nace_nona_2018 %>% filter(number_of_filings==1 | consolidation_l=="C") %>% select(-number_of_filings)

it_orbis_address_nace_nona_2018<- it_orbis_address_nace_nona_2018 %>% group_by(bvd_id_number, year) %>% 
  mutate(number_of_filings= n()) %>% 
  ungroup %>% filter(number_of_filings ==1 | consolidation_l =="U") %>% 
  select(-number_of_filings)

it_orbis_address_nace_nona_2018<- it_orbis_address_nace_nona_2018 %>% group_by(bvd_id_number, year_orbis) %>% 
  mutate(number_of_filings= n()) %>% 
  ungroup %>% filter(number_of_filings ==1 | filing_type =="Annual report")



it_orbis_address_nace_nona_2018<-it_orbis_address_nace_nona_2018 %>% group_by(bvd_id_number, year, city, consolidation_l) %>% 
  mutate(max_turnover = max(operating_revenue_turnover_)) %>%
  ungroup()

it_orbis_address_nace_nona_2018 <- it_orbis_address_nace_nona_2018 %>% 
  mutate(city = case_when(
    city == "62 Concesio" ~ "Concesio",
    city == "47 Levada Ponte di Piave" ~ "Levada Ponte di Piave",
    city == "(Iio Entrata Via Faggiana) Latina" ~ "Latina",
    city == "(Ingresso Anche Da Via Hermada 6) Genova" ~ "Genova",
    city == "(Secondo Ingr. Via Santi 1) Medesano" ~ "Medesano",
    city == "00 Treviso" ~ "Treviso",
    city == "20 Gorle" ~ "Gorle",
    city == "28 Bologna" ~ "Bologna",
    city == "43 Castiglione Delle Stiviere" ~ "Castiglione Delle Stiviere",
    city == "47levada Ponte Di Piave" ~ "Levada Ponte Di Piave",
    city == "77 Roma" ~ "Roma",
    city == "80 Prevalle" ~ "Prevalle",
    city == "92025 Casteltermini" ~ "Casteltermini",
    city == "A Roma" ~ "Roma",
    TRUE ~ city
  ))







it_orbis_2018<- it_orbis_address_nace_nona_2018 %>% distinct( max_turnover,.keep_all = TRUE)

# Check whether you have a high number of bvd_ids with more observations per year



check<- it_orbis_2018 %>% group_by (bvd_id_number, year, city, consolidation_code) %>% count()
check_duplicates<- check %>% filter(n>1)# 0 this is a good sign


# Summarize the data ------------------------------------------------------

#ebitda_sector_city <- it_orbis_address_nace %>% 
#group_by(comune, two_digit, year) %>% 
#summarize(average_ebitda = mean(ebitda))

turnover_sector_city<- it_orbis_address_nace_nona_2018 %>% group_by(city, two_digit, year) %>% 
  summarize(average_turnover= mean(max_turnover))
turnover_sector_city<- turnover_sector_city %>% filter(city != "")

turnover_sector_city_supply <- turnover_sector_city %>% filter(two_digit %in% c(25, 45, 69,72,28))
turnover_sector_city_supply<- left_join(turnover_sector_city_supply, italy_gdp_deflator_2020)
turnover_sector_city_supply<- turnover_sector_city_supply %>% mutate(average_turnover_2020 = (average_turnover/prices_2020)*100)

turnover_sector_city_supply_2004<- turnover_sector_city_supply %>% filter(year >2003) %>% select(city, year, two_digit, average_turnover_2020)
turnover_sector_city_supply_2004<- turnover_sector_city_supply_2004 %>% mutate(log_average_turnover_2020 = log(average_turnover_2020))



#I do this select those sectors that using the OECD 
# the website is here https://stats.oecd.org/Index.aspx?DataSetCode=IOTS_2021#

# From long to wide

### 25

turnover_sector_city_supply_2004_25<- turnover_sector_city_supply_2004 %>% 
  filter(two_digit=='25') %>% 
  filter(city !="")


turnover_supply_wide_25<- turnover_sector_city_supply_2004_25 %>% 
  select(-log_average_turnover_2020) %>% 
  pivot_wider(names_from = two_digit, values_from = average_turnover_2020) 


turnover_supply_wide_25<- turnover_supply_wide_25 %>% drop_na() %>% 
  filter(year<2018)

turnover_supply_wide_number_25<- turnover_supply_wide_25 %>% group_by(city) %>% count() %>% # this is just to check that I have enough observations for the city
  ungroup() %>% 
  filter(n>13)

list_cities<- unique(turnover_supply_wide_number_25$city)
turnover_supply_wide_2004_2017_25<- turnover_supply_wide_25 %>% filter(city %in% list_cities)


turnover_supply_wide_2004_2017_25$city<- toupper(turnover_supply_wide_2004_2017_25$city)
turnover_supply_wide_2004_2017_25<- turnover_supply_wide_2004_2017_25 %>% 
  rename(municipality= city)

## 28
turnover_sector_city_supply_2004_28<- turnover_sector_city_supply_2004 %>% 
  filter(two_digit=='28') %>% 
  filter(city !="")

turnover_supply_wide_28<- turnover_sector_city_supply_2004_28 %>%
  select(-log_average_turnover_2020) %>% 
  pivot_wider(names_from = two_digit, values_from = average_turnover_2020)  

turnover_supply_wide_28<- turnover_supply_wide_28 %>% drop_na() %>% 
  filter(year<2018)

turnover_supply_wide_number_28<- turnover_supply_wide_28 %>% group_by(city) %>% count() %>% 
  ungroup() %>% 
  filter(n>13)

list_cities<- unique(turnover_supply_wide_number_28$city)
turnover_supply_wide_2004_2017_28<- turnover_supply_wide_28 %>% filter(city %in% list_cities)


turnover_supply_wide_2004_2017_28$city<- toupper(turnover_supply_wide_2004_2017_28$city)


## Write the data ----------------------------------------------------------

write.csv(turnover_supply_wide_2004_2017_25, here("data_proc", "turnover_25_2004_17.csv"))
write.csv(turnover_supply_wide_2004_2017_28, here("data_proc", "turnover_28_2004_17.csv"))

turnover_supply_wide_2004_2017_25<- read_csv(here("data_proc", "turnover_25_2004_17.csv"))
turnover_supply_wide_2004_2017_25<- turnover_supply_wide_2004_2017_25 %>% 
  select(-'...1')
mid_size_northern_municipalities_2004_17<- read_csv(here("data_proc", "mid_size_northern_municipalities.csv")) 
mid_size_northern_municipalities_orbis <- left_join(mid_size_northern_municipalities_2004_17, turnover_supply_wide_2004_2017_25) %>% drop_na()
write.csv(mid_size_northern_municipalities_orbis, here("data_proc", "mid_size_northern_municipalities_orbis.csv"))

# ### Create data for the spillover analysis  -----------------------------

northern_municipalities_spillover<- read_csv(here("data_proc", "northern_municipalities_all.csv")) %>% select(-n)
northern_municipalities_spillover_no_cern <- northern_municipalities_spillover %>%
  select(municipality, year, cern_procurement, region) %>%
  filter(year >2009)

northern_cern<- northern_municipalities_spillover_no_cern %>% group_by(municipality) %>% 
  mutate(total_cern = sum(cern_procurement)) %>% 
  filter(municipality != "SCHIO") %>% 
  filter(total_cern>1000000) 

cern_cities <- unique(northern_cern$municipality)
northern_municipalities_spillover<- northern_municipalities_spillover %>% filter(year<2018)

#Load Sistemi Locali del Lavoro lookup
sll<- read_csv(here("data_proc", "sll_region_lookup.csv"))
sll<- clean_names(sll)
sll$city <- stringi::stri_trans_general(str= sll$city, id="Latin-ASCII") # this gets rid of all the accents
sll_region_lookup <- sll %>% select(city, den_sll_2011) # select only the variables 
sll_region_lookup<- sll_region_lookup %>% drop_na()
sll_region_lookup <-sll_region_lookup %>% unique()
sll_region_lookup<- sll_region_lookup %>% rename(ttwa_2011= den_sll_2011)

sll_region_lookup<- sll_region_lookup %>% select(ttwa_2011,city)
sll_region_lookup<- sll_region_lookup %>% unique()
schio_sll <- sll_region_lookup %>% filter(ttwa_2011== "SCHIO")

counting_obs <- northern_municipalities_spillover %>% group_by(municipality) %>% tally ()

full_list_cities <- counting_obs %>% filter(n==14)
northern_municipalities_spillover<- northern_municipalities_spillover %>% filter(municipality %in% full_list_cities$municipality)
sll_region_lookup<- sll_region_lookup %>% rename(municipality = city)
northern_municipalities_spillover<- left_join(northern_municipalities_spillover,sll_region_lookup)
#mid_size_northern_municipalities_wage_spillover<- dplyr::left_join(mid_size_northern_municipalities_wage_spillover,sll_region_lookup)


# Consumption sectors
northern_municipalities_spillover<- northern_municipalities_spillover %>% 
  mutate(log_consumption = log(construction + hotels_restaurant+0.000001), 
         manufacturing = replace_na(manufacturing, 0), 
         health_social = replace_na(health_social,0),
         education = replace_na(education,0), 
         wholesale_retail = replace_na(wholesale_retail, 0),
         transport = replace_na(transport, 0),
         hotels_restaurant = replace_na(hotels_restaurant,0),
         construction = replace_na(construction, 0))


northern_municipalities_spillover<- northern_municipalities_spillover %>% 
  mutate(log_non_tradable = log(wholesale_retail+ hotels_restaurant +education + health_social+
                                  transport + construction+0.0000000000000001 ))



    
# Construction 

northern_municipalities_spillover<- northern_municipalities_spillover %>% 
  mutate(log_construction = log(construction))


northern_municipalities_spillover<- northern_municipalities_spillover %>% 
  mutate(manufacturing = manufacturing +0.001,
         log_manufacturing = log(manufacturing))



#I first remove all the municipalities that have received a CERN procurement contract

northern_municipalities_spillover_no_cern<- subset(northern_municipalities_spillover,!(municipality %in% cern_cities))




###  Assign treatment to 2012 ----------------------------------------

#2012 treatment 
northern_municipalities_spillover_no_cern<- northern_municipalities_spillover_no_cern %>% mutate(treat_2012 = case_when(ttwa_2011 =="SCHIO" & year>2012 ~ 1, year<2013 & ttwa_2011 =="SCHIO"~ 0,
                                                                                                      municipality!="SCHIO"~0))# This creates a treatment variable


northern_municipalities_spillover_no_cern <- northern_municipalities_spillover_no_cern %>% filter(municipality !="SCHIO")
northern_municipalities_spillover_no_cern<- northern_municipalities_spillover_no_cern %>% select(-region) %>%  distinct_all()

write.csv(northern_municipalities_spillover_no_cern, here("data_proc", "northern_municipalities_spillover.csv"))


# ### Schio_bordering_municipalities --------------------------------------


italy<- st_read(here("data_raw","LAU_RG_01M_2020_4326.shp", "LAU_RG_01M_2020_4326.shp"))
italy<- italy   %>% filter(CNTR_CODE == "IT")
plot(st_geometry(italy))

borders<- st_intersects(italy, italy)
namesof = lapply(borders, function(n){italy$LAU_NAME[n]})
id_of =lapply(borders,function(n){italy$LAU_ID[n]})
schio_neighbours<-as.vector(namesof[2984])

schio_neighbours<- schio_neighbours[[1]]
schio_neighbours<- str_to_upper(schio_neighbours)
schio_boundaries<- italy %>% filter(LAU_NAME=="Schio")

# Create the dataset for the neighbouring  municipalities as treated 
`%notin%` <- Negate(`%in%`)
northern_municipalities_2012_neighbours <- northern_municipalities_spillover_no_cern %>% mutate(treat_neighbours = case_when(municipality %in% schio_neighbours & year>2012 ~ 1, year<2013 & municipality %in% schio_neighbours~ 0,
                                                                                                                 municipality %notin% schio_neighbours~0))
northern_municipalities_2012_neighbours<- northern_municipalities_2012_neighbours %>% filter(municipality!="SCHIO")
write_csv(northern_municipalities_2012_neighbours, here("data_proc", "northern_municipalities_bordering.csv"))

# ###SLL data -------------------------------------------------------------

ttwa_grouped <- read_excel(here("data_proc", "ttwa_grouped_2012.xls")) %>% select(-treat) %>% 
  mutate(log_manufacturing = log(manufacturing))




pop_data_sll_2011<- fread(here("data_raw","SLL", "SLL2011_CENPOP2011.csv"))
pop_data_sll_2011<- clean_names(pop_data_sll_2011)
pop_data_sll_2011<- pop_data_sll_2011 %>% filter(popolazione_residente_totale != "P1")
#SELECT THE COLUMNS that I want to keep numeric
sll_names <- pop_data_sll_2011 %>% select(codice_del_sll_2011, denominazione_del_sll_2011)
pop_data_sll_2011<- pop_data_sll_2011 %>% mutate_all(~as.numeric(str_remove_all(as.character(.x), '\\.')))
pop_data_sll_2011<- pop_data_sll_2011 %>% select(-denominazione_del_sll_2011, -codice_del_sll_2011)
pop_data_sll_2011<- cbind(sll_names, pop_data_sll_2011)
pop_data_sll_2011<- pop_data_sll_2011%>%
  mutate(year= 2011) %>% 
  rename(ttwa_2011 = denominazione_del_sll_2011)

colnames(pop_data_sll_2011) <- paste0(colnames(pop_data_sll_2011),"_2011")

pop_data_sll_2011<- pop_data_sll_2011 %>% 
  rename(ttwa_2011= ttwa_2011_2011) %>% 
  select(-codice_del_sll_2011_2011)

ttwa_grouped_pop <- left_join(ttwa_grouped, pop_data_sll_2011) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


#2012 treatment 
ttwa_grouped_2012<- ttwa_grouped_pop %>% mutate(treat_2012 = case_when(ttwa_2011 =="SCHIO" & year>2011 ~ 1, year<2012 & ttwa_2011 =="SCHIO"~ 0,ttwa_2011 !="SCHIO"~0))# This creates a treatment variable
# 2010 treatment 
ttwa_grouped_2012<- ttwa_grouped_2012 %>% mutate(treat_2010 = case_when (ttwa_2011 =="SCHIO" & year> 2009 ~1, year<2010 & ttwa_2011 =="SCHIO" ~ 0, 
                                                                        ttwa_2011 !="SCHIO"~0))# This creates a treatment variable


ttwa_grouped_2017 <- ttwa_grouped_2012 %>% filter(year<2018)







write_csv(ttwa_grouped_2017, here("data_proc", "ttwa_grouped_final.csv"))


# ## Schio Sectorial analysis ---------------------------------------------
schio_sector_analysis<- fread("/Users/gabrielepiazza/Dropbox/PhD/Synthetic control/Schio data analysis.csv",header= TRUE)
schio_sector_analysis[schio_sector_analysis =="13 - Industrie tessili"]<- "13 - Textile Production"
schio_sector_analysis[schio_sector_analysis =="17 - Fabbricazione di carta"]<- "17 - Papermaking"
schio_sector_analysis[schio_sector_analysis =="25- Prodotti in metallo"] <- "25 - Metal products"
schio_sector_analysis[schio_sector_analysis =="28 - Macchinari ed apparecchiature"]<- "28 - Machinery & Equipment"
schio_sector_analysis[schio_sector_analysis =="31 - Fabbricazione di mobili"]<- "31 - Furniture manufacturing"
schio_sector_analysis[schio_sector_analysis =="33 - Riparazione di macchine"]<- "33 - Machine repair"

schio_sector_analysis<- schio_sector_analysis %>% mutate(zanon_sector =case_when(Code =="28 - Machinery & Equipment" ~ 1,                                                                                                           Code !="28 - Machinery & Equipmente" ~ 0)) 

schio_sector_analysis$zanon_sector<- as.factor(schio_sector_analysis$zanon_sector)
write_csv(schio_sector_analysis, here("data_proc", "schio_sector_analysis.csv"))



