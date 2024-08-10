# Info --------------------------------------------------------------------
##
##Script name: 01_dataprep_B
##
##Purpose of script: Preparing the data for the paper "Where the God Particles touches the ground - The local economic impact of RI procurement " 
## This creates the northern municipalities 
##Author: Gabriele Piazza
##
##Date Created: 2024-08-10
## Purpose of the script: Preparing the data for the analysis
##Copyright (c) Gabriele Piazza, 2024
##Email: g.piazza@lse.ac.uk 
##


## Notes:
##   
##


# 1. Setting up -----------------------------------------------------------



##1.1 Install/Load packages ---------------------------------------------------



options(scipen = 999)
need<- c("devtools", "remotes", "kbal", "stringi","foreign","haven","sf","eeptools","data.table","readxl" ,"tjbal", "Hmisc", "skimr", "tabulator", "easycsv", "janitor", "tidyverse")
have <- need %in% rownames(installed.packages()) # check packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages



## 1.2 Setting directories------------------------------------------------

data_raw_dir<- "/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_proc/"
orbis_dir <- paste0(data_raw_dir, "ORBIS/")
GDP_deflator_dir <- paste0(data_raw_dir, "GDP_deflator/")

## 1.3 Create functions ---------------------------------------------------
load_csv <- function(file_path) {
  library(data.table)
  fread(file_path, encoding = "Latin-1")
}

`%notin%` <- Negate(`%in%`)
loadcsv_multi <- function(data_dir, extension="csv"){
  file_list <- list.files(path = data_dir, pattern = paste0("\\.", extension, "$"), full.names = TRUE)
  dfs <- lapply(file_list, function(x) fread(x, encoding = "Latin-1"))
  combined_df <- rbindlist(dfs)
  return(combined_df)
}

readsubfolder <- function (f){attempt <- fread(f)}
## 1.4 Loading data------------------------------------------------

###1.41 Address Data ------------------------------------------------------
italy_address<- read_dta(paste0(orbis_dir, "Address IT.dta"))

###1.42 GDP deflator ------------------------------------------------------
italy_gdp_deflator_2020 <- read_csv(paste0(GDP_deflator_dir, "Italy_GDP_deflated_2020.csv"))

###1.43 NACE data ------------------------------------------------------
italy_nace <- read_csv(paste0(orbis_dir, "NACE IT.csv"))

###1.44 Financial data ------------------------------------------------------
italy_financial<- read_dta(paste0(orbis_dir, "Gabriele IT.dta"))


# 2. Preparing the data ----------------------------------------------------

##2.1 Cleaning the datasets ----------------------------------------------------

###2.11 Address -------------------------------------------------------------
colnames(italy_address)<- c("bvd_id_number", "postcode", "city", "city_native") #change the column names 
italy_address <- italy_address %>% mutate(check = city == city_native)# this is not necessary - it just checks whether the city and city native variables are the same.
italy_address <- italy_address %>% filter(city !="None")

###2.12 GDP deflator -------------------------------------------------------------
italy_gdp_deflator_2020<- clean_names(italy_gdp_deflator_2020)

###2.13 NACE -------------------------------------------------------------

italy_nace <- italy_nace %>% rename(bvd_id_number = bvdidnumber)
italy_nace <- italy_nace %>% select(bvd_id_number, nacerev2primarycodes) %>% filter(!is.na(nacerev2primarycodes))

# # Load the Orbis data
# it_orbis <- read_dta(here("data_raw", "ORBIS", "Gabriele IT.dta"))
# # Load postocodes
# #it_postcodes <- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/listacomuni 2.csv")
# #it_postcodes<- clean_names(it_postcodes)
# #it_postcodes<- it_postcodes %>% rename(postcode= cap) %>% select(postcode, comune)

##2.2 Putting the data together ------------------------------------------

#Merging financial data and address

it_orbis_address<- left_join(italy_financial, italy_address)
it_orbis_address <- it_orbis_address %>% select(bvd_id_number,city,consolidation_code, filing_type,  number_of_employees, operating_revenue_turnover_, p_l_before_tax, p_l_after_tax,
                                                ebitda, postcode, closing_date)


rm(italy_address, italy_financial) # I remove the file as I don't have much space
#it_orbis_address_postcode<- left_join(it_orbis_address, it_postcodes) # Match with the postcodes
it_orbis_address <- it_orbis_address %>% filter(!is.na(postcode)) %>% select(-number_of_employees,-p_l_before_tax, p_l_after_tax) # I get rid of some variables and drop those without postcodes

#Merging with the NACE 
it_orbis_address_nace <- left_join(it_orbis_address, italy_nace) # Join with the NACE code
rm(italy_nace) # remove the italy_nace
it_orbis_address_nace<- it_orbis_address_nace %>% mutate(two_digit = floor(nacerev2primarycodes/100)) # I do this to get better codes
it_orbis_address_nace <- it_orbis_address_nace %>% filter(!is.na(operating_revenue_turnover_))
it_orbis_address_nace_nona <- it_orbis_address_nace%>% drop_na(closing_date)
rm(it_orbis_address_nace)

##2.3 Creating the time variable ------------------------------------------


# Assign the date following Kalemli-Ozcan et al.  

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


## 2.4 Creating the turnover variable ------------------------------------------------------

#ebitda_sector_city <- it_orbis_address_nace %>% 
#group_by(comune, two_digit, year) %>% 
#summarize(average_ebitda = mean(ebitda))

turnover_sector_city<- it_orbis_2018 %>% group_by(city, two_digit, year) %>% 
  summarize(average_turnover= mean(max_turnover))
turnover_sector_city<- turnover_sector_city %>% filter(city != "")


# 10 - Manufacture of food products
# 13 - Textile products
# 16 - Manufacture of wood
# 17 - Manufucture of paper products
# 19 - Manufacture of coke products
# 20 - Manufacture of chemical products
# 21 - Manufacture of pharmaceutical products
# 22 - Manufacture of rubber products
# 23 - Manufacture of non-metallic products
# 24 - Manufacture of basic metal products
# 25 - Manufacture of fabricated metal products
# 26 - Manufacture of computer electronic products
# 27 - Manufacture of electrical equipment

turnover_sector_city_supply <- turnover_sector_city %>% filter(two_digit %in% c(10, 13, 16, 17, 19, 20, 21, 22, 23, 24, 25, 26, 27, 45, 69,72,28))
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
