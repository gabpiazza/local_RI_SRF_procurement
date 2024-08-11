# Info --------------------------------------------------------------------
##
##Script name: 01_dataprep_C
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
##This is for the spillover analysis 
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

northern_municipalities_spillover<- read_csv(paste0(data_proc_dir,"municipalities/", "northern_municipalities_all.csv")) %>% select(-n)
sll<- read_csv(paste0(data_proc_dir, "SLL/", "sll_region_lookup.csv"))
italy<- st_read(paste0(data_raw_dir,"LAU_RG_01M_2020_4326.shp/", "LAU_RG_01M_2020_4326.shp"))

# 2. Preparing the data  --------------------------------------------------


##2.1 Cleaning the datasets ----------------------------------------------

###2.11 Northern municipalities ----------------------------------------------
northern_municipalities_spillover_no_cern <- northern_municipalities_spillover %>%
  select(municipality, year, cern_procurement) %>%
  filter(year >2009)

# Identify the cities that received CERN procurement
northern_cern<- northern_municipalities_spillover_no_cern %>% group_by(municipality) %>% 
  mutate(total_cern = sum(cern_procurement)) %>% 
  filter(municipality != "SCHIO") %>% 
  filter(total_cern>1000000) 

cern_cities <- unique(northern_cern$municipality)
northern_municipalities_spillover<- northern_municipalities_spillover %>% filter(year<2018)

###2.12 SLL  ----------------------------------------------
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

###2.13 Merging the data together  ----------------------------------------------
northern_municipalities_spillover<- left_join(northern_municipalities_spillover,sll_region_lookup)


###2.14 Schio neighbours  ----------------------------------------------

italy<- italy   %>% filter(CNTR_CODE == "IT")
plot(st_geometry(italy))

borders<- st_intersects(italy, italy)
namesof = lapply(borders, function(n){italy$LAU_NAME[n]})
id_of =lapply(borders,function(n){italy$LAU_ID[n]})
schio_neighbours<-as.vector(namesof[2984])

schio_neighbours<- schio_neighbours[[1]]
schio_neighbours<- str_to_upper(schio_neighbours)
schio_boundaries<- italy %>% filter(LAU_NAME=="Schio")
##2.2 Creating variables ----------------------------------------------

###2.21 Log variables ----------------------------------------------

northern_municipalities_spillover<- northern_municipalities_spillover %>% 
  mutate(log_consumption = log(construction + hotels_restaurant+0.000001), 
         manufacturing = replace_na(manufacturing, 0), 
         health_social = replace_na(health_social,0),
         education = replace_na(education,0), 
         wholesale_retail = replace_na(wholesale_retail, 0),
         transport = replace_na(transport, 0),
         hotels_restaurant = replace_na(hotels_restaurant,0),
         construction = replace_na(construction, 0),
         log_non_tradable = log(wholesale_retail + hotels_restaurant + education + health_social +
                                  transport+ construction + 0.0000000001),
         log_construction = log(construction), 
         log_manufacturing = log(manufacturing + 0.0000000001))


###2.22 Removing CERN municipalities and creating treatment variables ----------------------------------------------


#I first remove all the municipalities that have received a CERN procurement contract

northern_municipalities_spillover_no_cern<-northern_municipalities_spillover %>% 
  filter(municipality %notin% cern_cities)


###  Assign treatment to 2012 ----------------------------------------

#2012 treatment 
northern_municipalities_spillover_no_cern<- northern_municipalities_spillover_no_cern %>% mutate(treat_2012 = case_when(ttwa_2011 =="SCHIO" & year>2012 ~ 1, year<2013 & ttwa_2011 =="SCHIO"~ 0,
                                                                                                                        municipality!="SCHIO"~0))# This creates a treatment variable


northern_municipalities_spillover_no_cern <- northern_municipalities_spillover_no_cern %>% filter(municipality !="SCHIO")  %>% 
  distinct()



###2.23 Neighbour municipalities ----------------------------------------------


# Create the dataset for the neighbouring  municipalities as treated 

northern_municipalities_2012_neighbours <- northern_municipalities_spillover_no_cern %>% mutate(treat_neighbours = case_when(municipality %in% schio_neighbours & year>2012 ~ 1, year<2013 & municipality %in% schio_neighbours~ 0,
                                                                                                                             municipality %notin% schio_neighbours~0))
northern_municipalities_2012_neighbours<- northern_municipalities_2012_neighbours %>% filter(municipality!="SCHIO")


# 3. Save files -----------------------------------------------------------

write.csv(northern_municipalities_spillover_no_cern, paste0(data_proc_dir,"municipalities/", "northern_municipalities_spillover.csv"))
write_csv(northern_municipalities_2012_neighbours, paste0(data_proc_dir,"municipalities/", "northern_municipalities_bordering.csv"))
