# Info --------------------------------------------------------------------
##
##Script name: 01_dataprep_A
##
##Purpose of script: Preparing the data for the paper "Where the God Particles touches the ground - The local economic impact of RI procurement " 
## This creates the northern municipalities 
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



##1.1 Install/Load packages ---------------------------------------------------



options(scipen = 999)
need<- c("devtools", "remotes", "kbal", "stringi","foreign","haven","sf","eeptools","data.table","readxl" ,"tjbal", "Hmisc", "skimr", "tabulator", "easycsv", "janitor", "tidyverse")
have <- need %in% rownames(installed.packages()) # check packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages



##1.2 Setting directories------------------------------------------------

data_raw_dir<- "/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_proc/"
asia_data_dir<- paste0(data_raw_dir, "ASIA_data/")
asia_data_2004_11_dir<- paste0(asia_data_dir, "2004-2011/csv/")
asia_data_2012_18_dir<- paste0(asia_data_dir, "2012-2018/")
comuni_north_dir <- "~/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_raw/Comuni/comuni_lookup.csv"
north_panel_dir <- "~/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_raw/Comuni/north_panel_final.csv"
census_2011_dir <- "~/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_raw/census_2011_comuni/all_comuni_selected.csv"
cern_suppliers_dir<- "~/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_raw/CERN_suppliers/suppliers_italy_city.csv"
sll_dir <- "~/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_proc/SLL/sll_2011.csv"
  
  
##1.3 Create functions ---------------------------------------------------
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
##1.4 Loading data------------------------------------------------

###1.41 Addetti ----------------------------------------------------------


for (year in 2004:2011) {
  file_path <- paste0(asia_data_2004_11_dir,"addetti_", year, ".csv")
  df <- load_csv(file_path)
  df <- clean_names(df)
  
  # Assign the cleaned data frame to a variable in the global environment
  assign(paste0("addetti_", year), df, envir = .GlobalEnv)
}

addetti_2012_2018 <- loadcsv_multi(asia_data_2012_18_dir, extension = "csv")


###1.42 Comuni ----------------------------------------------------------

comuni_north <- load_csv(comuni_north_dir)

###1.43 Census ----------------------------------------------------------

census_2011<- load_csv(census_2011_dir)

###1.44 CERN suppliers ----------------------------------------------------------

cern_suppliers<- load_csv(cern_suppliers_dir)

###1.45 Wage data ----------------------------------------------------------

folder_files<- list.files(recursive = T, full.names= T,pattern = "\\.csv$",paste0(data_raw_dir, "irpef_data/"))

files_folder_extraction<- sapply(folder_files, readsubfolder, simplify = F)
list2env(files_folder_extraction,envir=.GlobalEnv)
selected_columns<- c("Anno di imposta", "Denominazione Comune", "Reddito da lavoro dipendente - Frequenza","Reddito da lavoro dipendente - Ammontare in euro" )
names(files_folder_extraction) <- paste0("income_", seq(from = 2000, to = 2018, by = 1)) # rename the different dataframes
files_folder_extraction[[7]]<- files_folder_extraction[[7]] %>% rename(`Reddito da lavoro dipendente - Ammontare in euro` = `Reddito da lavoro dipendente  - Ammontare in euro`,
                                                                       `Reddito da lavoro dipendente - Frequenza`= `Reddito da lavoro dipendente  - Frequenza`)  # change variable names to make it consistent
files_folder_extraction[[9]]<- files_folder_extraction[[9]] %>% rename(`Reddito da lavoro dipendente e assimilati - Ammontare` = `Reddito da lavoro dipendente e assimilati - Ammontare in euro`)

income_2000_07 <- files_folder_extraction[1:8] # extract the first 8 years - same variable names
income_2008_14 <- files_folder_extraction[9:15] # extract 2008-14 years - same variable names
income_2015_18 <- files_folder_extraction[16:19] # rest of years - similar case 

###1.46 SLL ----------------------------------------------------------

sll <- fread(sll_dir)

###1.47 North panel ----------------------------------------------------------

north_panel <- read_csv(paste0(data_raw_dir, "Comuni/","north_panel_final.csv"))### I have to find out how I got this

# 2.Cleaning the data------------------------------------------------

##2.1 Addetti ------------------------------------------------------------

#### 2004-2011 -----------------------------------------------------------



addetti_2005<- addetti_2005 %>% 
  rename(codice_provincia = codice)
addetti_2006$year <- 2006 # the year variable was missing
addetti_2004<- addetti_2004 %>% select(c(names(addetti_2006)))
addetti_2005<- addetti_2005 %>% select(c(names(addetti_2006)))
addetti_2004_2006 <- list(addetti_2004, addetti_2005, addetti_2006) %>%
  rbindlist(fill = TRUE) %>%
  select(-codice_provincia, -codice_comune) %>%
  mutate_at(2:15, as.numeric) %>% 
  rename(mining= c, manufacturing =d, electricity_gas_water = e, construction = f,
         wholesale_retail = g, hotels_restaurant= h, transport = i,
         finance = j, business_activities= k, education = m, health_social=n) %>% select(-o) # I have to do this as there was a change in the codes from 2002 to 2007
# I have to do this as there was a change in the codes from 2002 to 2007

# The issue here is that they use different definitions. 
#I have used the classification from this website https://www.businessballs.com/glossaries-and-terminology/industrial-classifications-uk-sic-codes/ this refers to the UK SIC code but it should be the same as the Italian one



list_2007_2011<- c("addetti_2007", "addetti_2008","addetti_2009", "addetti_2010", "addetti_2011")
#add year to the dataset 
years<- 2007:2011
for (i in seq_along(list_2007_2011)) {
  df <- get(list_2007_2011[i])  # Get the dataframe by name
  df$year <- years[i]  # Add the year column
  assign(list_2007_2011[i], df)  # Reassign the modified dataframe back to its original name
}
addetti_2009<- addetti_2009 %>% 
  rename(denominazione_comune = denominazione)
addetti_2011<- addetti_2011 %>% 
  rename(denominazione_comune = v3)
addetti_2008_2011<- c("addetti_2008", "addetti_2009", "addetti_2010", "addetti_2011")

for (name in addetti_2008_2011){
  df <- get(name) # get the dataframes by name
  df <- df %>% select(c(names(addetti_2007)))#select columns based on 2007
  assign(name, df)
}
addetti_2007_2011<- list(addetti_2007, addetti_2008, addetti_2009, addetti_2010, addetti_2011) # create a list
addetti_2007_2011<-rbindlist(addetti_2007_2011) %>%  # bind them together
  select(-codice_provincia, -codice_comune) %>% mutate_at(c(2:19), decomma) %>%  # the values with thousands had commas and this make it harder 
  mutate_at(c(2:19), as.numeric) %>% # some columns are character
  rename(mining= b, manufacturing = c, hotels_restaurant=i, transport= h, finance= k, education = p, health_social= q,
                                                 construction = f, wholesale_retail = g) %>% #chage the names of the variable so that they match those for 2004-2008
  mutate(electricity_gas_water= d+e, business_activities = j+l+m+n) # same as above

## Bind 2004-2011 
addetti_2007_2011<- addetti_2007_2011 %>% select(c(names(addetti_2004_2006)))

addetti_2004_2011<-rbind(addetti_2004_2006, addetti_2007_2011)%>%
  select(-mining)


### 2012-2018 ------------------------------------------------------------
#to load the files, you should do the same as above using fread
addetti_2012_2018<- addetti_2012_2018 %>% rename(codice_comune= code, denominazione_comune= territorio, totale="10" ) %>% 
  clean_names %>% 
  rename(manufacturing = c, hotels_restaurant=i, transport= h, finance= k, education = p, health_social= q, construction = f, wholesale_retail = g) %>% 
  mutate(electricity_gas_water= d+e, business_activities = j+l+m+n) %>% 
  select(c(names(addetti_2004_2011)))

### Create the 2004-2017 panel -------------------------------------------

# 2004-2018 This binds all the data togetehr 

addetti_2004_2018<-rbind(addetti_2004_2011, addetti_2012_2018)
addetti_2004_2018$denominazione_comune<- str_to_upper(addetti_2004_2018$denominazione_comune) # it should remove the accent but it does not manage
addetti_2004_2018[,denominazione_comune:= stri_trans_general(str=denominazione_comune, id="Latin-ASCII")] #
addetti_2004_2018$denominazione_comune<- stri_trans_general(str= addetti_2004_2018$denominazione_comune, id="Latin-ASCII")
addetti_2004_2018<- addetti_2004_2018 %>% rename(municipality = denominazione_comune)


## 2.2 Comuni North --------------------------------------------------------


colnames(comuni_north)<- c("municipality", "area", "region")
comuni_north$municipality<- str_to_upper(comuni_north$municipality)
comuni_north <-as.data.frame(comuni_north)

comuni_north$municipality<- iconv(comuni_north$municipality, from = "UTF-8", to = "ASCII//TRANSLIT")
comuni_north$municipality <- gsub("`", "", comuni_north$municipality)
addetti_north <- left_join(addetti_2004_2018, comuni_north)
# addetti_north<- addetti_north %>% drop_na(area)
schio<- addetti_north %>% filter(municipality=="SCHIO") # This is not necessary
counting_obs <- addetti_north %>% group_by(municipality) %>% tally ()
addetti_north<- left_join(addetti_north, counting_obs)
addetti_north <- addetti_north %>% filter(n>13)


##2.3 Census -------------------------------------------------------------
census_2011<- clean_names(census_2011)
census_2011<- census_2011 %>% rename (year = anno_cp, municipality= city)
census_2011$municipality<- str_to_upper(census_2011$municipality)
census_2011$municipality <- stri_trans_general(str= census_2011$municipality, id="Latin-ASCII")




##2.4 CERN data ---------------------------------------------------------

cern_suppliers <- cern_suppliers %>% rename(cern_procurement = total_CHF, year= order_date, municipality = city) %>%
  filter(year>2003)

##2.5 Wage data ---------------------------------------------------------
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
write.csv(wage_data_2000_2018, paste0(data_proc_dir,"wage_data/", "wage_data_2000_2018.csv"))

##2.5 SLL data ---------------------------------------------------------

sll<- clean_names(sll)
sll<- sll %>% rename (city = comune_2011) %>% dplyr::select(-pop_2011) 
sll$city <- stri_trans_general(str= sll$city, id="Latin-ASCII") # this gets rid of all the accents
sll_region_lookup <- sll %>% select(city, den_sll_2011) # select only the variables of interest
write_csv(sll_region_lookup, here("data_proc", "sll_region_lookup.csv"))


##2.6 North Panel  ---------------------------------------------------------

city_region <-north_panel %>% select(city, region) %>% distinct()# select only the variables from city region
sll_region_lookup <- left_join(sll_region_lookup, city_region)
sll_region_lookup<- sll_region_lookup %>% drop_na()
sll_region_lookup <-sll_region_lookup %>% unique()

sll_region_lookup<- sll_region_lookup %>% select(den_sll_2011, region, city)
sll_region_lookup<- sll_region_lookup %>% distinct()
write_csv(sll_region_lookup, paste0(data_proc_dir,"SLL/", "sll_region_lookup.csv"))

# 3. Preparing the data for the analysis ----------------------------------


##3.1 Getting the data together ------------------------------------------

northern_municipalities <- left_join(addetti_north, census_2011, by =c("municipality")) %>% 
  rename(year = year.x)
northern_municipalities<- left_join(northern_municipalities, cern_suppliers, by = c("municipality", "year"))
northern_municipalities<- northern_municipalities %>% drop_na(pop_2011, emploment_rate_2011)
list_full_cities<- unique(northern_municipalities$municipality)
northern_municipalities_full<- northern_municipalities %>% filter(municipality %in% list_full_cities)
write.csv(northern_municipalities_full, )
# Get all the data together

northern_municipalities <- left_join(addetti_north_full, census_2011, by=c("municipality")) %>% rename(year = year.x)
northern_municipalities<- left_join(northern_municipalities, cern_suppliers, by= c("municipality", "year"))
northern_municipalities<- northern_municipalities %>% drop_na(pop_2011)
northern_municipalities <- northern_municipalities %>% drop_na (emploment_rate_2011)
list_full_cities <- unique(northern_municipalities$municipality)
northern_municipalities_full<- northern_municipalities %>% filter(municipality %in% list_full_cities)
write.csv(northern_municipalities_full,paste0(data_proc_dir,"municipalities/" ,"northern_municipalities_all.csv"))

##3.2 Further data cleaning------------------------------------------

###3.21 Keep only mid-size cities------------------------------------------ 
#this might be unnecessary as I am doing this later on in 3.24

mid_size_northern_municipalities <- northern_municipalities_full %>% filter(pop_2011>10000)
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% filter(pop_2011<100000)
medium_cities<- mid_size_northern_municipalities$municipality


mid_size_northern_municipalities<- northern_municipalities_full %>% filter(municipality %in% medium_cities)
mid_size_northern_municipalities<- distinct(mid_size_northern_municipalities)
write.csv(mid_size_northern_municipalities,paste0(data_proc_dir,"municipalities/" ,"mid_size_northern_municipalities.csv"))

###3.22 Drop NAs------------------------------------------

mid_size_northern_municipalities<- mid_size_northern_municipalities %>% select(-V1)
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% drop_na(manufacturing, business_activities, education, health_social, 
                                                                                pop_2011, pop_2001_2011, electricity_gas_water, high_skilled_share, pop_density_2011,
                                                                                young_pop_degree_share) %>% select(-V1)
mid_size_northern_municipalities[is.na(mid_size_northern_municipalities)]<-0


###3.23 Keep only cities that have not received CERN procurement ------------------------------------------

# I am a bit worried that the treatment effect could be a result of subsequent CERN contract.
# For this reason I exclude all the cities with no contracts post 2012. I then look at all the cities that had a contract between 10 million and 100,000 Swiss franc
# over this period. This decision is a bit arbitrary.  


mid_size_northern_municipalities_no_cern <- mid_size_northern_municipalities %>%dplyr::select(municipality, year, cern_procurement, region) %>% dplyr::filter(year >2009)
mid_size_cern<- mid_size_northern_municipalities_no_cern %>% dplyr::group_by(municipality) %>% 
  mutate(total_cern = sum(cern_procurement)) %>% 
  filter(municipality != "SCHIO") %>% 
  filter(total_cern>1000000) 
cern_cities <- unique(mid_size_cern$municipality)
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% 
  filter(municipality %notin% cern_cities)


###3.24 Remove the municipalities that are more than 50% larger or smaller than Schio ------------------------------------------

population_threshold<- mid_size_northern_municipalities %>% dplyr::filter(year==2011, municipality =="SCHIO") %>% 
  pull(pop_2011)
#population_threshold #39131 - Schio
population_cities <- subset(mid_size_northern_municipalities, pop_2011>= 0.5 *population_threshold & pop_2011<=1.5*population_threshold)
population_cities<- population_cities %>% select(municipality) %>% distinct()
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% filter(municipality %in% population_cities$municipality)

###3.25 Remove the municipalities that are in Schio's local labour market ------------------------------------------


#There has been a big road development in 2017 and I don't want to capture this
#Load SSL lookup

city_region <-north_panel %>% select(city, region) %>% unique()# select only the variables from city region
sll_region_lookup <- left_join(sll_region_lookup, city_region)
sll_region_lookup<- sll_region_lookup %>% drop_na()
sll_region_lookup <-sll_region_lookup %>% distinct()

sll_region_lookup<- sll_region_lookup %>% select(den_sll_2011, region, city)
sll_region_lookup<- sll_region_lookup %>% unique()
schio_sll <- sll_region_lookup %>% filter(den_sll_2011== "SCHIO")
no_schio_sll<- schio_sll %>% filter(city !="SCHIO") %>% rename(municipality= city)
no_schio_sll<- no_schio_sll$municipality

# Before starting with the estimation, I want to exclude all the municipalities that are neighbouring Schio to avoid spillover effects
 mid_size_northern_municipalities<- mid_size_northern_municipalities %>% filter(!municipality %in% no_schio_sll)

 
###3.26 Merge with wage data ------------------------------------------

mid_size_northern_municipalities_wage <- left_join(mid_size_northern_municipalities, wage_data_2000_2018) %>% 
   filter(municipality != "SANREMO") %>% drop_na(average_employee_income)
 
##3.3 Create new variables ------------------------------------------
###3.31 Create the tradable and non tradable variables ------------------------------------------
 
 mid_size_northern_municipalities_wage<- mid_size_northern_municipalities_wage %>% 
   mutate(tradable = manufacturing + finance + business_activities + electricity_gas_water,
          non_tradable = construction + wholesale_retail + transport, #+ education + health_social,
          log_manufacturing = log(manufacturing),
          log_tradable= log(tradable),
          log_non_tradable = log(non_tradable))
 

 
 ###3.33 Create treatment variables ------------------------------------------
 

 #2012 treatment 
mid_size_northern_municipalities_wage<- mid_size_northern_municipalities_wage %>% mutate(treat_2012 = case_when(municipality =="SCHIO" & year>2012 ~ 1, year<2013 & municipality =="SCHIO"~ 0,
                                                                                                           municipality!="SCHIO"~0))# This creates a treatment variable

# 2010 treatment 
mid_size_northern_municipalities_wage<- mid_size_northern_municipalities_wage %>% mutate(treat_2010 = case_when (municipality =="SCHIO" & year> 2010 ~1, year<2011 & municipality =="SCHIO" ~ 0, 
                                                                                                            municipality !="SCHIO"~0))# This creates a treatment variable

mid_size_northern_municipalities_wage<- mid_size_northern_municipalities_wage %>% drop_na(average_employee_income)
mid_size_northern_municipalities_2004_17 <- mid_size_northern_municipalities_wage %>% filter(year<2018) 
counting_obs <- mid_size_northern_municipalities_2004_17 %>% group_by(municipality) %>% tally ()
full_list_cities <- counting_obs %>% filter(n==14)

mid_size_northern_municipalities_2004_17<- mid_size_northern_municipalities_2004_17 %>% 
  filter(municipality %in% full_list_cities$municipality) 

##3.4 Save File------------------------------------------


write.csv(mid_size_northern_municipalities_2004_17, paste0(data_proc_dir,"municipalities/", "mid_size_northern_municipalities.csv"))




