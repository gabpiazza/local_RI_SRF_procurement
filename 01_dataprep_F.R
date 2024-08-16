# Info --------------------------------------------------------------------
##
##Script name: 01_dataprep_F
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


siab_reg_ind_yr<- read_csv(paste0(data_raw_dir, "German_data/", "siab_reg_ind_year.csv"))
districts_lookup <- read_csv(paste0(data_raw_dir, "German_data/","Kreis_lookup.csv"))



# 2. Preparing the data ---------------------------------------------------

##2.1 Data cleanig and merging ---------------------------------------------------

siab_reg_ind_yr<- siab_reg_ind_yr %>% select (-"...1")
siab_reg_ind_yr$ao_region<- as.character(siab_reg_ind_yr$ao_region)

districts_lookup$SIAB_R_7517_SUF<- as.character(districts_lookup$SIAB_R_7517_SUF)
districts_lookup<- districts_lookup %>% rename(ao_region = SIAB_R_7517_SUF )

siab_reg_ind_yr_district <- left_join(siab_reg_ind_yr, districts_lookup)



siab_wide<- siab_reg_ind_yr_district %>%
  pivot_wider(names_from = w08_gen_gr, values_from = n)  


siab_wide <-siab_wide %>% 
  rename(manufact_2 = "2",
         manufact_3 = "3",
         manufact_4 = "4",
         district_region = distrinct_region) %>% 
  mutate(log_manufact_2 = log(manufact_2),
         log_manufact_3 = log(manufact_3), 
         log_manufact_4 = log(manufact_4))


siab_wide<-siab_wide %>%filter(!is.na(district_region))
siab_wide_2004<-siab_wide %>% filter(year >2003)

manufacturing_na<- siab_wide_2004 %>% filter(is.na(manufact_3))
check_districts<- unique(manufacturing_na$district_region)
siab_wide_2004<- siab_wide_2004 %>% filter(district_region %notin% check_districts)


# manufacturing 3 is the sector that includes 
manufacturing_3_observations<- siab_wide_2004 %>% filter(year>2003) %>% select(district_region, manufact_3) %>% 
  filter(!is.na(manufact_3)) %>% 
  group_by(district_region) %>% 
  count()


##2.2 Create the HQ dataset -----------------------------------------------
# Only the HQ is treated

siab_wide_2004_HQtreated<- siab_wide_2004 %>% 
  mutate(treat_2012 = case_when(district_region=="Rhei ni sch-Bergischer Kreis" & year>2012 ~1, 
                                district_region=="Rhei ni sch-Bergischer Kreis" & year<2013 ~0,
                                district_region!="Rhei ni sch-Bergischer Kreis"~0))





#I also remove Dortmund to address the issue of interpolation bias

siab_wide_2004_HQtreated<- siab_wide_2004_HQtreated %>% 
  filter(district_region != "Dortmund, City")


##2.3 Two treated -----------------------------------------------



treated_district <- c("Rhei ni sch-Bergischer Kreis", "Dortmund, City")
siab_wide_2004_twotreated<- siab_wide_2004 %>% 
  mutate(treat_2012 = case_when(district_region %in% treated_district & year>2012 ~1, 
                                district_region %in% treated_district & year<2013 ~0,
                                district_region %notin% "Rhei ni sch-Bergischer Kreis"~0))

##2.4 Dortmund treated -----------------------------------------------

siab_wide_2004_dortmund<- siab_wide_2004 %>% 
  mutate(treat_2012 = case_when(district_region =="Dortmund, City" & year>2012 ~1, 
                                district_region == "Dortmund, City" & year<2013 ~0,
                                district_region !="Dortmund, City"~0))


#I use the industry information from this website https://doku.iab.de/fdz/reporte/2019/DR_04-19_EN.pdf.The industry in which Research Instruments operates is manufacturing 3




# 3. Saving the data ------------------------------------------------------
write_rds(siab_wide_2004_HQtreated, paste0(data_proc_dir, "German_data/", "siab_wide_2004_HQtreated.rds"))
write_rds(siab_wide_2004_twotreated, paste0(data_proc_dir, "German_data/", "siab_wide_2004_twotreated.rds"))
write_rds(siab_wide_2004_dortmund, paste0(data_proc_dir, "German_data/", "siab_wide_2004_dortmund.rds"))

