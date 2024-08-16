# Info --------------------------------------------------------------------
##
##Script name: 02_analysis_D
##
##Purpose of script: Main analysis for "Where the God Particles touches the ground - The local economic impact of RI procurement " 
##
##Author: Gabriele Piazza
##
##Date Created: 2023-02-20
##
##Copyright (c) Gabriele Piazza, 2023
##Email: g.piazza@lse.ac.uk 
##

##
## Notes: This is the script for spillover effects
##   
##
# 1. Setting up -----------------------------------------------------------
##1.1 Install/Load packages ---------------------------------------------------
require(devtools)
set.seed(123456)
need <- c(
  "panelView",
  "Synth",
  "foreach",
  "tidyverse",
  "here",
  "tjbal",
  "doParallel",
  "tidyverse",
  "fixest",
  "progress",
  "beepr",
  "logger",
  "SCtools"
)

have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

options(scipen = 999)

##1.2 Setting directories------------------------------------------------
data_raw_dir<- "/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/data_proc/"
results_dir <-"/Users/gabrielepiazza/Dropbox/PhD/Local_RI_SRF_procurement/Analysis/results/"

##1.3 Create functions -----------------------------------------------------------
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
rmse <- function(y){sqrt(mean(y^2))}

##1.4 Loading data------------------------------------------------

mid_size_northern_municipalities <- read_csv(paste0(data_proc_dir, "municipalities/", "mid_size_northern_municipalities.csv"))
northern_municipalities_spillover<- read_csv(paste0(data_proc_dir, "municipalities/", "northern_municipalities_spillover.csv"))
northern_municipalities_bordering <- read_csv(paste0(data_proc_dir, "municipalities/", "northern_municipalities_bordering.csv"))


# #2. Displacement Analysis ------------------------------------------------------------

##2.1 Municipalities in the same LMA ---------------------------------------------------------
###2.11 Setting up  ---------------------------------------------------------

# Specifying the columns where you want to replace NAs
columns_to_replace <- c("finance", "business_activities", "electricity_gas_water", 
                        "wholesale_retail", "hotels_restaurant", "education", 
                        "health_social", "transport", "construction")

# Replacing NAs with 0 in those columns
northern_municipalities_spillover[columns_to_replace] <- lapply(northern_municipalities_spillover[columns_to_replace], function(x) ifelse(is.na(x), 0, x))

northern_municipalities_spillover<- northern_municipalities_spillover %>% 
  mutate(log_tradable= log(finance + business_activities+ electricity_gas_water+ 0.0000000000000001), 
         log_non_tradable = log(wholesale_retail+ hotels_restaurant +education + health_social+
                                  transport + construction+0.0000000000000001 ))





###2.12 Estimate the effect   ---------------------------------------------------------
####2.121 Manufacturing   ---------------------------------------------------------

out_northern_manufacturing_spillover_2012.kbal <- tjbal(data = northern_municipalities_spillover, Y = "log_manufacturing", D = "treat_2012", Y.match.time = c(2004:2012),
                                                        X = c("pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                        X.avg.time = list(c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                        index = c("municipality","year"), demean = T, estimator = "meanfirst", vce="jack")



saveRDS(out_northern_manufacturing_spillover_2012.kbal ,file = paste0(results_dir, "output/", "out_manufacturing_spillover.rds"))
out_northern_manufacturing_spillover_2012.kbal<-outputout_manufacturing_spillover
####2.122 Tradable   ---------------------------------------------------------

out_northern_tradable_spillover_2012.kbal <- tjbal(data = northern_municipalities_spillover, Y = "log_tradable", D = "treat_2012", Y.match.time = c(2004:2012),
                                                   X = c("pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                   X.avg.time = list(c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                   index = c("municipality","year"), demean = T, estimator = "meanfirst")



beep()

saveRDS(out_northern_tradable_spillover_2012.kbal ,file = paste0(results_dir, "output/", "out_tradable_spillover.rds"))
out_northern_tradable_spillover_2012.kbal<-outputout_tradable_spillover
####2.123 Non-tradable   ---------------------------------------------------------

out_northern_non_tradable_spillover_2012.kbal <- tjbal(data = northern_municipalities_spillover, Y = "log_non_tradable", D = "treat_2012", Y.match.time = c(2004:2012),
                                                       X = c("pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                       X.avg.time = list(c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                       index = c("municipality","year"), demean = T, estimator = "meanfirst")

beep()
saveRDS(out_northern_non_tradable_spillover_2012.kbal ,file = paste0(results_dir,"output/", "out_non_tradable_spillover.rds"))

###2.13 Save the results   ---------------------------------------------------------
saveRDS(out_northern_manufacturing_spillover_2012.kbal ,file = paste0(results_dir, "output/", "out_manufacturing_spillover.rds"))


##2.2 Bordering municipalities ---------------------------------------------------------

###2.21 Setting up ---------------------------------------------------------

# Specifying the columns where you want to replace NAs
columns_to_replace <- c("finance", "business_activities", "electricity_gas_water", 
                        "wholesale_retail", "hotels_restaurant", "education", 
                        "health_social", "transport", "construction")

# Replacing NAs with 0 in those columns
northern_municipalities_bordering[columns_to_replace] <- lapply(northern_municipalities_bordering[columns_to_replace], function(x) ifelse(is.na(x), 0, x))



northern_municipalities_bordering<- northern_municipalities_bordering %>% 
  mutate(log_tradable= log(finance + business_activities+ electricity_gas_water+ 0.0000000000000001), 
         log_non_tradable = log(wholesale_retail+ hotels_restaurant +education + health_social+
                                  transport + construction+0.0000000000000001 ))


###2.22 Estimating the effect ---------------------------------------------------------

####2.221 Manufacturing ---------------------------------------------------------

northern_municipalities_bordering<- northern_municipalities_bordering %>% 
  select(-year.y)
out_bordering_manufacturing_spillover_2012.kbal <- tjbal(data = northern_municipalities_bordering, Y = "log_manufacturing", D = "treat_neighbours", Y.match.time = c(2004:2012),
                                                         X = c("pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                         X.avg.time = list(c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                         index = c("municipality","year"), demean = T, estimator = "meanfirst", vce="jack")

beep()

####2.222 Tradable ---------------------------------------------------------


out_bordering_tradable_spillover_2012.kbal <- tjbal(data = northern_municipalities_bordering, Y = "log_tradable", D = "treat_neighbours", Y.match.time = c(2004:2012),
                                                    X = c("log_manufacturing","pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                    X.avg.time = list(c(2004:2011),c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                    index = c("municipality","year"), demean = T, estimator = "meanfirst",vce="jack")
beep()


####2.223 Non-tradable ---------------------------------------------------------


out_bordering_non_tradable_spillover_2012.kbal <- tjbal(data = northern_municipalities_bordering, Y = "log_non_tradable", D = "treat_neighbours", Y.match.time = c(2004:2012),
                                                        X = c("log_manufacturing","pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                        X.avg.time = list(c(2004:2011),c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                        index = c("municipality","year"), demean = T, estimator = "meanfirst")

beep()
###2.23 Save the results  ---------------------------------------------------------

saveRDS(out_bordering_manufacturing_spillover_2012.kbal  ,file = paste0(results_dir,"output/", "out_manufacturing_bordering.rds"))
saveRDS(out_bordering_tradable_spillover_2012.kbal ,file = paste0(results_dir,"output/", "out_tradable_bordering.rds"))
saveRDS(out_bordering_non_tradable_spillover_2012.kbal  ,file = paste0(results_dir,"output/", "out_non_tradable_bordering.rds"))
beep()

##2.3 Putting the data together  ---------------------------------------------------------
###2.31 Setting up  ---------------------------------------------------------

spillover_manufacturing <- print(out_northern_manufacturing_spillover_2012.kbal)
spillover_tradable<- print(out_northern_tradable_spillover_2012.kbal)
spillover_non_tradable<- print(out_northern_non_tradable_spillover_2012.kbal)
bordering_manufacturing<- print(out_bordering_manufacturing_spillover_2012.kbal)
bordering_tradable<- print(out_bordering_tradable_spillover_2012.kbal)
bordering_non_tradable <- print(out_bordering_non_tradable_spillover_2012.kbal)

row.names(spillover_non_tradable)<- "Log Non-tradable Employees"
row.names(spillover_manufacturing)<- "Log Manufacturing Employees"
row.names(spillover_tradable)<- "Log Tradable Employees"
row.names(bordering_manufacturing)<- "Log Manufacturing Employees"
row.names(bordering_tradable)<- "Log Tradable Employees"
row.names(bordering_non_tradable)<- "Log Non-tradable Employees"




spillover_results <- rbind.data.frame(spillover_manufacturing, spillover_tradable, spillover_non_tradable,
                                      bordering_manufacturing, bordering_tradable, bordering_non_tradable)

###2.32 Save the data  ---------------------------------------------------------

write_csv(spillover_results, paste0(results_dir, "output/", "spillover_results.csv"))
