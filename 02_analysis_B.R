# Info --------------------------------------------------------------------
##
##Script name: 02_analysis_B
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
## Notes: This is for tradable and non-tradable sector
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
lma_2012<- read_csv(paste0(data_proc_dir, "SLL/", "ttwa_grouped_final.csv"))
turnover_sector_city_supply_2004_2017_wide<- read_csv(paste0(data_proc_dir, "turnover/", "turnover_sector_2004_2017_wide.csv"))
mid_size_northern_municipalities_orbis<- read_csv(paste0(data_proc_dir, "turnover/", "mid_size_northern_municipalities_orbis.csv"))

# 2. Analysis -------------------------------------------------------------

## 2.1 Linked industries -------------------------------------------------------------
### 2.11 Estimate the effect  -------------------------------------------------------------

mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% 
  mutate(log_turnover_25 = log(`25`+0.000001))

id <- rep(1:9, each = 14)
mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% 
  arrange(municipality)
mid_size_northern_municipalities_orbis<- cbind(id, mid_size_northern_municipalities_orbis)
mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% select(-'...1',-'...2')

mid_size_northern_municipalities_orbis$log_average_employee_income<- log(mid_size_northern_municipalities_orbis$average_employee_income)
mid_size_northern_municipalities_orbis<- as.data.frame(mid_size_northern_municipalities_orbis)
out_manufacturing_shift_2012_orbis_25_cov.kbal <- tjbal(data = mid_size_northern_municipalities_orbis, Y = "log_turnover_25", D = "treat_2012", Y.match.time = c(2004:2012),
                                                        X = c("log_average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011"),
                                                        X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011)),
                                                        index = c("municipality","year"), demean = T, estimator = "meanfirst")

### 2.12 Save the results  -------------------------------------------------------------

saveRDS(out_manufacturing_shift_2012_orbis_25_cov.kbal,file = paste0(results_dir,"output/", "out_manufacturing_orbis_25.rds"))


### 2.13 Placebo  -------------------------------------------------------------

##### A. Setting up -----------------------------------------------------
set.seed(123456)
y<-14
max<-1288
n<-(max/y)
t <-10
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:92)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_orbis_m<-cbind(mid_size_northern_municipalities_orbis, M) 

SCHIO<- out_manufacturing_shift_2012_orbis_25_cov.kbal$att

# loop across control units
storegaps_orbis_25<- 
  matrix(NA,
         length(1:y),
         length(unique(mid_size_orbis_m$id))
  )
rownames(storegaps_orbis_25) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)


##### B. Estimate the placebo effect -----------------------------------------------------

results_orbis_25<- foreach(k= 1:n ) %dopar% {
  manufacturing_placebo_25 <- tjbal(data = mid_size_orbis_m, "log_turnover_25", D =   paste("treat_", k, sep = "" ), Y.match.time = c(2004:2012),
                                    X = c("average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011"),
                                    X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011)),
                                    index = c("municipality","year"), demean = T, estimator = "meanfirst")
  
  storegaps_orbis_25[,i] <- 
    i <- i + 1
  manufacturing_placebo_25$att
  
} # close loop over control units
results_orbis_25<- as.data.frame(do.call(cbind, results_orbis_25))
beep()
colnames(results_orbis_25) <- unique(mid_size_orbis_m$municipality) 


storegaps1_orbis_25<- results_orbis_25[,-76]
storegaps_orbis_25 <- cbind(SCHIO,storegaps1_orbis_25)

##### C. RMSE function  -----------------------------------------------------

preloss_orbis_mun <- apply(storegaps_orbis_25[1:9,],2,rmse)

postloss_orbis_mun <- apply(storegaps_orbis_25[10:14,],2,rmse)
check_2012_covariates_orbis_25 <- as.data.frame(postloss_orbis_mun/preloss_orbis_mun)
check_2012_covariates_orbis_25$rank <- rank(-check_2012_covariates_orbis_25$`postloss_orbis_mun/preloss_orbis_mun`)
mun_names <- names(postloss_orbis_mun)

check_2012_covariates_orbis_25<- cbind(mun_names, check_2012_covariates_orbis_25)

##### D. Save the placebo results -----------------------------------------------------

write.csv(check_2012_covariates_orbis_25,file = here("results","output","out_placebo_orbis_25.csv"))



## 2.2 Tradable sector  -------------------------------------------------------------

### 2.21 Estimate the effect  -------------------------------------------------------------

mid_size_northern_municipalities<- mid_size_northern_municipalities %>% 
  mutate(log_tradable = log(finance + business_activities + electricity_gas_water))

id <- rep(106, each = 14)
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% 
  arrange(municipality)
mid_size_northern_municipalities<- cbind(id, mid_size_northern_municipalities)

out_tradable_mun_2012_covariates.kbal <- tjbal(data = mid_size_northern_municipalities, Y = "log_tradable", D = "treat_2012", Y.match.time = c(2004:2012),
                                               X = c("log_manufacturing", "average_employee_income","log_non_tradable", "pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011", "emploment_rate_2011"),
                                               X.avg.time = list(c(2004:2012),c(2004:2012), c(2004:2012),c(2011), c(2011), c(2011), c(2011), c(2011)),
                                               index = c("municipality","year"), demean = T, estimator = "meanfirst")

### 2.22 Save the result  -------------------------------------------------------------

saveRDS(out_tradable_mun_2012_covariates.kbal,file = paste0(results_dir,"output/", "out_tradable_mun.rds"))# 


#output_tradable_mun<- capture.output(out_tradable_mun_2012_covariates.kbal)
#save(output_tradable_mun,file = here("results","output", "out_tradable_mun.RData"))# 


### 2.23 Placebo  -------------------------------------------------------------

##### A. Setting up -----------------------------------------------------

## I have to include the balance table here
# create a matrix that assigns the treatment to the units in the donor pool
y <- 14 # number of years
max <- 106 * 14
n <- (max / y)
t <- 10 # treatment time
M <- matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:106) # number of municipalities
my.names <- paste(prefix, suffix, sep = "")
colnames(M) <- my.names

for (col in 1:n) {
  idRow = (t + y * (col - 1)):(y * col)
  M[idRow, col] <- 1
}

mid_size_m <- cbind(mid_size_northern_municipalities, M)

SCHIO <- out_tradable_mun_2012_covariates.kbal$att

# loop across control units
storegaps_tradable_mun <- 
  matrix(NA,
         length(1:y),
         length(unique(mid_size_m$id))
  )
rownames(storegaps_tradable_mun) <- 1:14

i <- 1
numCores <- detectCores() # not necessary 
registerDoParallel(numCores) # not necessary

##### B. Estimate for all the other units in the donor pool -------------------------------------------------------------

results_tradable_mun_12 <- foreach(k = 1:n) %dopar% {
  tradable_placebo_mun <- tjbal(data = mid_size_m, "log_tradable", D = paste("treat_", k, sep = ""), 
                                Y.match.time = c(2004:2012),
                                X = c("log_manufacturing", "average_employee_income", "pop_2011", 
                                      "pop_density_2011", "high_skilled_share", "pop_2001_2011", 
                                      "emploment_rate_2011"),
                                X.avg.time = list(c(2004:2012), c(2004:2012), c(2011), c(2011), 
                                                  c(2011), c(2011), c(2011)),
                                index = c("municipality", "year"), demean = TRUE, 
                                estimator = "meanfirst")
  
  storegaps_tradable_mun[, i] <- tradable_placebo_mun$att
  i <- i + 1
  tradable_placebo_mun$att
} # close loop over control units

results_tradable_mun_12 <- as.data.frame(do.call(cbind, results_tradable_mun_12))
beep()
colnames(results_tradable_mun_12)<- unique(mid_size_m$municipality)
storegaps1_tradable_mun<- results_tradable_mun_12[,-88]# exclude the treated unit 
storegaps_tradable_mun <- cbind(SCHIO,storegaps1_tradable_mun) # attach the initial result from the main estimation 

#####C. RMSE function and calculate this for the pre and post treatment period-------------------------------------------------------------


preloss_tradable_mun<- apply(storegaps_tradable_mun[1:9,],2,rmse)
postloss_tradable_mun <- apply(storegaps_tradable_mun[10:14,],2,rmse)

check_2017_covariates_tradable_mun <- as.data.frame(postloss_tradable_mun/preloss_tradable_mun)
check_2017_covariates_tradable_mun$rank <- rank(-check_2017_covariates_tradable_mun$`postloss_tradable_mun/preloss_tradable_mun`)
mun_names <- names(postloss_tradable_mun)
check_2017_covariates_tradable_mun<- cbind(mun_names, check_2017_covariates_tradable_mun)

### I prepare the data for the plot
schio_tradable_MSPE<- cbind.data.frame(municipalities_names, preloss_tradable_mun, postloss_tradable_mun)
schio_tradable_MSPE<- schio_tradable_MSPE %>% mutate(RMSPE  = postloss_tradable_mun/preloss_tradable_mun)
schio_tradable_MSPE$rank<-rank(-schio_tradable_MSPE$RMSPE)

output_tradable_placebo<- capture.output(results_tradable_mun_12)

##### D. Save the placebo results -----------------------------------------------------

save(output_tradable_placebo,file = paste0(results_dir,"output/", "out_tradable_placebo.RData"))# 


## 2.3 Non-tradable sector  -------------------------------------------------------------
### 2.31 Estimate the effect -------------------------------------------------------------


out_non_tradable_mun_2012_covariates.kbal <- tjbal(data = mid_size_northern_municipalities, Y = "log_non_tradable", D = "treat_2012", Y.match.time = c(2004:2012),
                                                   X = c("log_manufacturing", "average_employee_income", "pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011", "emploment_rate_2011"),
                                                   X.avg.time = list(c(2004:2012),c(2004:2012),c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                   index = c("municipality","year"), demean = T, estimator = "meanfirst")

### 2.32 Save the results -------------------------------------------------------------

saveRDS(out_non_tradable_mun_2012_covariates.kbal,file = paste0(results_dir,"output/", "out_non_tradable_mun.rds"))

### 2.33 Placebo effects -------------------------------------------------------------
##### A. Setting up -----------------------------------------------------

## I have to include the balance table here
# create a matrix that assigns the treatment to the units in the donor pool
y <- 14 # number of years
max <- 106 * 14
n <- (max / y)
t <- 10 # treatment time
M <- matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:106) # number of municipalities
my.names <- paste(prefix, suffix, sep = "")
colnames(M) <- my.names

for (col in 1:n) {
  idRow = (t + y * (col - 1)):(y * col)
  M[idRow, col] <- 1
}

mid_size_m <- cbind(mid_size_northern_municipalities, M)

SCHIO<- out_non_tradable_mun_2012_covariates.kbal$att

# loop across control units
storegaps_non_tradable_2012_covariates<- 
  matrix(NA,
         length(1:y),
         length(unique(mid_size_m$id))
  )
rownames(storegaps_non_tradable_2012_covariates) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)



##### B. Estimate the placebo effect -----------------------------------------------------

results_mun_non_tradable <- foreach(k=1:n) %dopar% {
  non_tradable_placebo_mun <- tjbal(
    data = mid_size_m, 
    "log_non_tradable", 
    D = paste("treat_", k, sep = "" ), 
    Y.match.time = c(2004:2012),
    X = c(
      "log_manufacturing", 
      "average_employee_income", 
      "pop_2011", 
      "pop_density_2011", 
      "high_skilled_share",
      "pop_2001_2011", 
      "emploment_rate_2011"
    ),
    X.avg.time = list(c(2004:2012),c(2004:2012),c(2011), c(2011), c(2011), c(2011), c(2011)),
    index = c("municipality","year"), 
    demean = T, 
    estimator = "meanfirst"
  )
  
  storegaps_non_tradable_2012_covariates[,i] <- non_tradable_placebo_mun$att
  i <- i + 1
  
  non_tradable_placebo_mun$att
}




beep()
results_mun_non_tradable<- as.data.frame(do.call(cbind, results_mun_non_tradable))

colnames(results_mun_non_tradable)<- unique(mid_size_m$municipality)
storegaps_non_tradable_2017_mun<- results_mun_non_tradable
storegaps1_non_tradable_2017_mun<- storegaps_non_tradable_2017_mun[,-88]
storegaps_non_tradable_2017_mun <- cbind(SCHIO,storegaps1_non_tradable_2017_mun)


##### C. RMSE function -----------------------------------------------------

preloss_non_tradable_mun<- apply(storegaps_non_tradable_2017_mun[1:9,],2,rmse)
postloss_non_tradable_mun <- apply(storegaps_non_tradable_2017_mun[10:14,],2,rmse)

check_2017_covariates_non_tradable_mun <- as.data.frame(postloss_non_tradable_mun/preloss_non_tradable_mun)
check_2017_covariates_non_tradable_mun$rank <- rank(-check_2017_covariates_non_tradable_mun$`postloss_non_tradable_mun/preloss_non_tradable_mun`)
mun_names <- names(postloss_non_tradable_mun)
check_2017_covariates_non_tradable_mun<- cbind(mun_names, check_2017_covariates_non_tradable_mun)

### I prepare the data for the plot
schio_non_tradable_MSPE<- cbind.data.frame(municipalities_names, preloss_non_tradable_mun, postloss_non_tradable_mun)
schio_non_tradable_MSPE<- schio_non_tradable_MSPE %>% mutate(RMSPE  = postloss_non_tradable_mun/preloss_non_tradable_mun)
schio_non_tradable_MSPE$rank<-rank(-schio_non_tradable_MSPE$RMSPE)

##### D. Save the placebo results -----------------------------------------------------

write.csv(schio_non_tradable_MSPE,file = paste0(results_dir,"output/","out_placebo_non_tradable.csv"))



## 2.4 Upstream sector total  -------------------------------------------------------------
### 2.41 Estimate the effect  -------------------------------------------------------------

mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% 
  select(-id) %>% 
  mutate(log_upstream_total = log(upstream_total))

id <- rep(1:95, each = 14)
mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% 
  arrange(municipality)
mid_size_northern_municipalities_orbis<- cbind(id, mid_size_northern_municipalities_orbis)
mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% select(-'...1',-'...2')

mid_size_northern_municipalities_orbis$log_average_employee_income<- log(mid_size_northern_municipalities_orbis$average_employee_income)
mid_size_northern_municipalities_orbis<- as.data.frame(mid_size_northern_municipalities_orbis)
out_manufacturing_shift_2012_orbis_upstream_cov.kbal <- tjbal(data = mid_size_northern_municipalities_orbis, Y = "log_upstream_total", D = "treat_2012", Y.match.time = c(2004:2012),
                                                        X = c("log_average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011"),
                                                        X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011)),
                                                        index = c("municipality","year"), demean = T, estimator = "meanfirst")

### 2.42 Save the results  -------------------------------------------------------------

saveRDS(out_manufacturing_shift_2012_orbis_upstream_cov.kbal,file = paste0(results_dir,"output/", "out_manufacturing_upstream.rds"))


### 2.43 Placebo  -------------------------------------------------------------

##### A. Setting up -----------------------------------------------------
set.seed(123456)
y<-14
max<-14*95
n<-(max/y)
t <-10
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:95)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_orbis_m<-cbind(mid_size_northern_municipalities_orbis, M) 

SCHIO<- out_manufacturing_shift_2012_orbis_upstream_cov.kbal$att

# loop across control units
storegaps_orbis_upstream<- 
  matrix(NA,
         length(1:y),
         length(unique(mid_size_orbis_m$id))
  )
rownames(storegaps_orbis_upstream) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)


##### B. Estimate the placebo effect -----------------------------------------------------

results_orbis_upstream<- foreach(k= 1:n ) %dopar% {
  manufacturing_placebo_upstream <- tjbal(data = mid_size_orbis_m, "log_upstream", D =   paste("treat_", k, sep = "" ), Y.match.time = c(2004:2012),
                                    X = c("average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011"),
                                    X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011)),
                                    index = c("municipality","year"), demean = T, estimator = "meanfirst")
  
  storegaps_orbis_upstream[,i] <- 
    i <- i + 1
  manufacturing_placebo_upstream$att
  
} # close loop over control units
results_orbis_upstream<- as.data.frame(do.call(cbind, results_orbis_upstream))
beep()
colnames(results_orbis_upstream) <- unique(mid_size_orbis_m$municipality) 


storegaps1_orbis_upstream<- results_orbis_upstream[,-79]
storegaps_orbis_upstream <- cbind(SCHIO,storegaps1_orbis_upstream)

##### C. RMSE function  -----------------------------------------------------

preloss_orbis_mun <- apply(storegaps_orbis_upstream[1:9,],2,rmse)

postloss_orbis_mun <- apply(storegaps_orbis_upstream[10:14,],2,rmse)
check_2012_covariates_orbis_upstream <- as.data.frame(postloss_orbis_mun/preloss_orbis_mun)
check_2012_covariates_orbis_upstream$rank <- rank(-check_2012_covariates_orbis_upstream$`postloss_orbis_mun/preloss_orbis_mun`)
mun_names <- names(postloss_orbis_mun)

check_2012_covariates_orbis_upstream<- cbind(mun_names, check_2012_covariates_orbis_upstream)

##### D. Save the placebo results -----------------------------------------------------

write.csv(check_2012_covariates_orbis_upstream,file = paste0(results_dir,"output/","out_placebo_orbis_upstream.csv"))

## 2.5 Upstream sector total  -------------------------------------------------------------
### 2.51 Estimate the effect  -------------------------------------------------------------

mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% 
  mutate(log_upstream_domestic = log(upstream_domestic))

id <- rep(1:95, each = 14)
mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% 
  arrange(municipality)
mid_size_northern_municipalities_orbis<- cbind(id, mid_size_northern_municipalities_orbis)
mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% select(-'...1',-'...2')

mid_size_northern_municipalities_orbis$log_average_employee_income<- log(mid_size_northern_municipalities_orbis$average_employee_income)
mid_size_northern_municipalities_orbis<- as.data.frame(mid_size_northern_municipalities_orbis)
out_manufacturing_shift_2012_orbis_upstream_domestic_cov.kbal <- tjbal(data = mid_size_northern_municipalities_orbis, Y = "log_upstream_domestic", D = "treat_2012", Y.match.time = c(2004:2012),
                                                              X = c("log_average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011"),
                                                              X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011)),
                                                              index = c("municipality","year"), demean = T, estimator = "meanfirst")

### 2.52 Save the results  -------------------------------------------------------------

saveRDS(out_manufacturing_shift_2012_orbis_upstream_domestic_cov.kbal,file = paste0(results_dir,"output/", "out_manufacturing_upstream.rds"))


### 2.53 Placebo  -------------------------------------------------------------

##### A. Setting up -----------------------------------------------------
set.seed(123456)
y<-14
max<-14*95
n<-(max/y)
t <-10
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:95)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_orbis_m<-cbind(mid_size_northern_municipalities_orbis, M) 

SCHIO<- out_manufacturing_shift_2012_orbis_upstream_domestic_cov.kbal$att

# loop across control units
storegaps_orbis_upstream_domestic<- 
  matrix(NA,
         length(1:y),
         length(unique(mid_size_orbis_m$id))
  )
rownames(storegaps_orbis_upstream_domestic) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)


##### B. Estimate the placebo effect -----------------------------------------------------

results_orbis_upstream_domestic<- foreach(k= 1:n ) %dopar% {
  manufacturing_placebo_upstream_domestic <- tjbal(data = mid_size_orbis_m, "log_upstream_domestic", D =   paste("treat_", k, sep = "" ), Y.match.time = c(2004:2012),
                                          X = c("average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011"),
                                          X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011)),
                                          index = c("municipality","year"), demean = T, estimator = "meanfirst")
  
  storegaps_orbis_upstream_domestic[,i] <- 
    i <- i + 1
  manufacturing_placebo_upstream_domestic$att
  
} # close loop over control units
results_orbis_upstream_domestic<- as.data.frame(do.call(cbind, results_orbis_upstream_domestic))
beep()
colnames(results_orbis_upstream_domestic) <- unique(mid_size_orbis_m$municipality) 


storegaps1_orbis_upstream_domestic<- results_orbis_upstream_domestic[,-79]
storegaps_orbis_upstream_domestic <- cbind(SCHIO,storegaps1_orbis_upstream_domestic)

##### C. RMSE function  -----------------------------------------------------

preloss_orbis_domestic_mun <- apply(storegaps_orbis_upstream_domestic[1:9,],2,rmse)

postloss_orbis_domestic_mun <- apply(storegaps_orbis_upstream_domestic[10:14,],2,rmse)
check_2012_covariates_orbis_upstream_domestic <- as.data.frame(postloss_orbis_domestic_mun/preloss_orbis_domestic_mun)
check_2012_covariates_orbis_upstream_domestic$rank <- rank(-check_2012_covariates_orbis_upstream_domestic$`postloss_orbis_domestic_mun/preloss_orbis_domestic_mun`)
mun_names <- names(postloss_orbis_domestic_mun)

check_2012_covariates_orbis_upstream_domestic<- cbind(mun_names, check_2012_covariates_orbis_upstream_domestic)

##### D. Save the placebo results -----------------------------------------------------

write.csv(check_2012_covariates_orbis_upstream_domestic,file = paste0(results_dir,"output/","out_placebo_orbis_upstream_domestic.csv"))

## 2.6 Upstream sector domestic percentage -------------------------------------------------------------
### 2.61 Estimate the effect  -------------------------------------------------------------

mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% 
  mutate(log_upstream_domestic_perc = log(upstream_domestic_perc))

id <- rep(1:95, each = 14)
mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% 
  arrange(municipality)
mid_size_northern_municipalities_orbis<- cbind(id, mid_size_northern_municipalities_orbis)
mid_size_northern_municipalities_orbis<- mid_size_northern_municipalities_orbis %>% select(-'...1')

mid_size_northern_municipalities_orbis$log_average_employee_income<- log(mid_size_northern_municipalities_orbis$average_employee_income)
mid_size_northern_municipalities_orbis<- as.data.frame(mid_size_northern_municipalities_orbis)
out_manufacturing_shift_2012_orbis_upstream_domestic_perc_cov.kbal <- tjbal(data = mid_size_northern_municipalities_orbis, Y = "log_upstream_domestic_perc", D = "treat_2012", Y.match.time = c(2004:2012),
                                                                       X = c("log_average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011"),
                                                                       X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011)),
                                                                       index = c("municipality","year"), demean = T, estimator = "meanfirst")

### 2.52 Save the results  -------------------------------------------------------------

saveRDS(out_manufacturing_shift_2012_orbis_upstream_domestic_perc_cov.kbal,file = paste0(results_dir,"output/", "out_manufacturing_upstream_domestic_perc.rds"))


### 2.53 Placebo  -------------------------------------------------------------

##### A. Setting up -----------------------------------------------------
set.seed(123456)
y<-14
max<-14*95
n<-(max/y)
t <-10
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:95)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_orbis_m<-cbind(mid_size_northern_municipalities_orbis, M) 

SCHIO<- out_manufacturing_shift_2012_orbis_upstream_domestic_perc_cov.kbal$att

# loop across control units
storegaps_orbis_upstream_domestic_perc<- 
  matrix(NA,
         length(1:y),
         length(unique(mid_size_orbis_m$id))
  )
rownames(storegaps_orbis_upstream_domestic_perc) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)


##### B. Estimate the placebo effect -----------------------------------------------------

results_orbis_upstream_domestic_perc<- foreach(k= 1:n ) %dopar% {
  manufacturing_placebo_upstream_domestic_perc <- tjbal(data = mid_size_orbis_m, "log_upstream_domestic_perc", D =   paste("treat_", k, sep = "" ), Y.match.time = c(2004:2012),
                                                   X = c("average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011"),
                                                   X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011)),
                                                   index = c("municipality","year"), demean = T, estimator = "meanfirst")
  
  storegaps_orbis_upstream_domestic_perc[,i] <- 
    i <- i + 1
  manufacturing_placebo_upstream_domestic_perc$att
  
} # close loop over control units
results_orbis_upstream_domestic_perc<- as.data.frame(do.call(cbind, results_orbis_upstream_domestic_perc))
beep()
colnames(results_orbis_upstream_domestic_perc) <- unique(mid_size_orbis_m$municipality) 


storegaps1_orbis_upstream_domestic_perc<- results_orbis_upstream_domestic_perc[,-79]
storegaps_orbis_upstream_domestic_perc <- cbind(SCHIO,storegaps1_orbis_upstream_domestic_perc)

##### C. RMSE function  -----------------------------------------------------

preloss_orbis_domestic_perc_mun <- apply(storegaps_orbis_upstream_domestic_perc[1:9,],2,rmse)

postloss_orbis_domestic_perc_mun <- apply(storegaps_orbis_upstream_domestic_perc[10:14,],2,rmse)
check_2012_covariates_orbis_upstream_domestic_perc <- as.data.frame(postloss_orbis_domestic_perc_mun/preloss_orbis_domestic_perc_mun)
check_2012_covariates_orbis_upstream_domestic_perc$rank <- rank(-check_2012_covariates_orbis_upstream_domestic_perc$`postloss_orbis_domestic_perc_mun/preloss_orbis_domestic_perc_mun`)
mun_names <- names(postloss_orbis_domestic_perc_mun)

check_2012_covariates_orbis_upstream_domestic_perc<- cbind(mun_names, check_2012_covariates_orbis_upstream_domestic_perc)

##### D. Save the placebo results -----------------------------------------------------

write.csv(check_2012_covariates_orbis_upstream_domestic_perc,file = paste0(results_dir,"output/","out_placebo_orbis_upstream_domestic_perc.csv"))


