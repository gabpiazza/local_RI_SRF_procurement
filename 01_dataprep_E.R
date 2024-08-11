# Info --------------------------------------------------------------------
##
##Script name: 01_dataprep_E
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



#1.1 Install/Load packages ---------------------------------------------------



options(scipen = 999)
need<- c("devtools", "remotes", "kbal", "stringi","foreign","haven","sf","eeptools","data.table","readxl" ,"tjbal", "Hmisc", "skimr", "tabulator", "easycsv", "janitor", "tidyverse")
have <- need %in% rownames(installed.packages()) # check packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages



# 1.2 Setting directories------------------------------------------------

data_raw_dir<- "/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_proc/"


# 1.3 Create functions ---------------------------------------------------
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
# 1.4 Loading data------------------------------------------------
# ## Schio Sectorial analysis ---------------------------------------------

schio_sector_analysis<- read_csv(paste0(data_proc_dir, "Schio/","schio_sector_analysis.csv"))

#2. Prepare the data  ---------------------------------------------------

# 2.1 Changing the labels -----------------------------------------------


schio_sector_analysis[schio_sector_analysis =="13 - Industrie tessili"]<- "13 - Textile Production"
schio_sector_analysis[schio_sector_analysis =="17 - Fabbricazione di carta"]<- "17 - Papermaking"
schio_sector_analysis[schio_sector_analysis =="25- Prodotti in metallo"] <- "25 - Metal products"
schio_sector_analysis[schio_sector_analysis =="28 - Macchinari ed apparecchiature"]<- "28 - Machinery & Equipment"
schio_sector_analysis[schio_sector_analysis =="31 - Fabbricazione di mobili"]<- "31 - Furniture manufacturing"
schio_sector_analysis[schio_sector_analysis =="33 - Riparazione di macchine"]<- "33 - Machine repair"

schio_sector_analysis<- schio_sector_analysis %>% mutate(zanon_sector =case_when(Code =="28 - Machinery & Equipment" ~ 1,
                                                                                 Code !="28 - Machinery & Equipmente" ~ 0)) 

schio_sector_analysis$zanon_sector<- as.factor(schio_sector_analysis$zanon_sector)

# 2.2 Saving the data -----------------------------------------------
write_csv(schio_sector_analysis, paste0(data_proc_dir,"Schio/","schio_sector_analysis.csv"))


