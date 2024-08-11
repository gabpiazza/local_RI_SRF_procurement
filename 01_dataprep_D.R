# Info --------------------------------------------------------------------
##
##Script name: 01_dataprep_D
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
##This is for SLL analysis
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


####SLL data -------------------------------------------------------------

ttwa_grouped <- read_excel(paste0(data_proc_dir,"SLL/" ,"ttwa_grouped_2012.xls")) 
pop_data_sll_2011<- fread(paste0(data_raw_dir, "SLL/", "SLL2011_CENPOP2011.csv"))

# 2. Preparing the data -----------------------------------------------------------
##2.1 Cleaning the datasets -----------------------------------------------------------

ttwa_grouped<- ttwa_grouped %>%  select(-treat) %>% 
mutate(log_manufacturing = log(manufacturing))


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


##2.2 Create a treatment variable -----------------------------------------------------------


#2012 treatment 
ttwa_grouped_2012<- ttwa_grouped_pop %>% mutate(treat_2012 = case_when(ttwa_2011 =="SCHIO" & year>2011 ~ 1, year<2012 & ttwa_2011 =="SCHIO"~ 0,ttwa_2011 !="SCHIO"~0))# This creates a treatment variable
# 2010 treatment 
ttwa_grouped_2012<- ttwa_grouped_2012 %>% mutate(treat_2010 = case_when (ttwa_2011 =="SCHIO" & year> 2009 ~1, year<2010 & ttwa_2011 =="SCHIO" ~ 0, 
                                                                         ttwa_2011 !="SCHIO"~0))# This creates a treatment variable

ttwa_grouped_2017 <- ttwa_grouped_2012 %>% filter(year<2018)



##2.3 Save the data -----------------------------------------------------------

write_csv(ttwa_grouped_2017, paste0(data_proc_dir,"SLL/" ,"ttwa_grouped_final.csv"))
