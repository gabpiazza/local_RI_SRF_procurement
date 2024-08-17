# Info --------------------------------------------------------------------
##
##Script name: 02_analysis
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
## Notes: This script is for the main results in manufacturing
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
# turnover_sector_city_supply_2004_2017_wide<- read_csv(paste0(data_proc_dir, "turnover", "turnover_sector_2004_2017_wide.csv"))


# analysis_log <- logger()
# here("~/Dropbox/PhD/Local_RI_SRF_procurement")
# here("Analysis")
# mid_size_northern_municipalities<- read_csv(here("data_proc", "mid_size_northern_municipalities.csv")) %>% select(-'...1') # for main analysis
# lma_2012<- read_csv(here("data_proc", "ttwa_grouped_final.csv")) # Labour Market Area
# turnover_25<- read_csv(here("data_proc", "turnover_25_2004_17.csv")) # This was for the linked sectors
# mid_size_northern_municipalities_orbis<- read_csv(here("data_proc","mid_size_northern_municipalities_orbis.csv")) #??
# northern_municipalities_spillover<- read_csv(here("data_proc", "northern_municipalities_spillover.csv")) %>% select(-'...1','...2') # this is for the spillover analsysis (same LMA)
# northern_municipalities_bordering<- read_csv(here("data_proc", "northern_municipalities_bordering.csv")) # this is for spillover (bordering)


# 2. Analysis -------------------------------------------------------------
##2.1 Diff-in-Diff  -------------------------------------------------------------
###2.11 Set up for the diff-in-diff -------------------------------------------------------------

# I begin with the  diff-in-diff
# This created a "dymanic treatment"
mid_size_northern_municipalities_twfe<- mid_size_northern_municipalities %>% 
  mutate(treat_2013 = case_when(municipality =="SCHIO" & year>2013 ~ 1, year<2014 & municipality =="SCHIO"~ 0,municipality!="SCHIO"~0),
         treat_2014 = case_when(municipality=="SCHIO" & year >2014 ~1, year <2015 & municipality =="SCHIO" ~0, 
                                municipality != "SCHIO"~0),
         treat_2015 = case_when(municipality=="SCHIO" & year >2015 ~1, year <2016 & municipality =="SCHIO" ~0, 
                                municipality != "SCHIO"~0),
         treat_2016 = case_when(municipality=="SCHIO" & year >2016 ~1, year <2017 & municipality =="SCHIO" ~0, 
                                municipality != "SCHIO"~0),
         treat_2012 = treat_2012)

###2.12 Run diff-in_diff -------------------------------------------------------------

# Run static TWFE, with SEs clustered at the municipality level
twfe_manufacturing_2012 <- feols(log_manufacturing ~ treat_2012| municipality + year,
                                 data = mid_size_northern_municipalities_twfe, cluster = "municipality")



twfe_manufacturing_dynamic <- feols(log_manufacturing ~ treat_2012 + treat_2013 +treat_2014 +treat_2015 + treat_2016 | municipality + year, 
                            data = mid_size_northern_municipalities_twfe, cluster = "municipality")

###2.13 Save the results -------------------------------------------------------------

saveRDS(twfe_manufacturing_2012,file = paste0(results_dir,"output/", "twfe_manufacturing_2012.rds"))
saveRDS(twfe_manufacturing_dynamic,file = paste0(results_dir,"output/", "twfe_manufacturing_dynamic.rds"))

output_twfe_2012 <- capture.output(summary(twfe_manufacturing_2012))
writeLines(output_twfe_2012, paste0(results_dir,"tables/","twfe_manufacturing_2012_output.txt"))

output_twfe_dynamic <- capture.output(summary(twfe_manufacturing_dynamic))
writeLines(output_twfe_dynamic, paste0(results_dir,"tables/","twfe_manufacturing_dynamic_output.txt"))

##2.2 Trajectory Balancing  -------------------------------------------------------------
###2.21 Municipalities -------------------------------------------------------------

####2.211 Set up -------------------------------------------------------------

id <- rep(1:106, each = 14)
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% 
  arrange(municipality)
mid_size_northern_municipalities<- cbind(id, mid_size_northern_municipalities)

mid_size_northern_municipalities$year <-as.numeric(mid_size_northern_municipalities$year)
panelview(log_manufacturing ~ average_employee_income + pop_2011, pop_density_2011, high_skilled_share,pop_2001_2011, 
          data = mid_size_northern_municipalities, index = c("municipality","year"), 
          xlab = "Year", ylab = "municipality")

####2.212 Run the analysis -------------------------------------------------------------
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% mutate(log_average_employee_income = log(average_employee_income))
mid_size_northern_municipalities <- mid_size_northern_municipalities %>% mutate(log_average_employee_income = log(average_employee_income))
out_manufacturing_shift_2012_covariates.kbal <- tjbal(data = mid_size_northern_municipalities, Y = "log_manufacturing", D = "treat_2012", Y.match.time = c(2004:2012),
                                                      X = c("log_average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011", "emploment_rate_2011"),
                                                      X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                      index = c("municipality","year"), demean = T, estimator = "meanfirst")



####2.213 Table weights -------------------------------------------------------------

mid_size_northern_municipalities<- mid_size_northern_municipalities %>%  arrange(municipality)
#mid_size_northern_municipalities<- cbind(id, mid_size_northern_municipalities)
#mid_size_northern_municipalities<- mid_size_northern_municipalities %>% select(-'...1')


municipalities<-as.matrix(unique(mid_size_northern_municipalities$municipality))
municipalities<- municipalities[-88]
municipalities<-as.matrix(municipalities)
weights<-out_manufacturing_shift_2012_covariates.kbal$weights.co
weights<-as.matrix(weights)
rownames(weights)<-municipalities

weights <- as.matrix(weights[ order(row.names(weights)), ])
SCHIO <- 0
weights_manufact<-rbind(SCHIO,weights);weights
weights_manufact<- as.data.frame(weights)
top_10_manufact_mun <- slice_max(weights_manufact,order_by = V1, n=10) %>% rename("TBM weights" = V1)
top_10_manufact_mun_list <- row.names(top_10_manufact_mun)
top_10_manufact_mun_list<- str_to_title(top_10_manufact_mun_list)
####2.214 Save the results -------------------------------------------------------------

saveRDS(out_manufacturing_shift_2012_covariates.kbal,file = paste0(results_dir,"output/", "out_manufacturing_2012_mun.rds"))# 
write_csv(top_10_manufact_mun, paste0(results_dir, "output/", "top_10_mun_weights.csv"))

####2.215 Placebo -------------------------------------------------------------
#####A. Setting up-------------------------------------------------------------

## I have to include the balance table here
# create a matrix that assign the treatment to the units in the donot pool
y<-14 # number of years
max<-106*14
n<-(max/y)
t <-10 # treatment time
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:106)# number of municipalities
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_m<-cbind(mid_size_northern_municipalities, M) 

SCHIO<- out_manufacturing_shift_2012_covariates.kbal$att

# loop across control units
storegaps_manufacturing_2012_covariates<- 
  matrix(NA,
         length(1:y),
         length(unique(mid_size_m$id))
  )
rownames(storegaps_manufacturing_2012_covariates) <- 1:14

i <- 1
numCores <- detectCores()# not necessary 
registerDoParallel(numCores)# not necessary



#####B. Estimate for all the other units in the donor pool-------------------------------------------------------------

#  
results_manufact_mun_12<- foreach(k= 1:n ) %dopar% {
  manufacturing_placebo_2012_covariates <- tjbal(data = mid_size_m, "log_manufacturing", D =   paste("treat_", k, sep = "" ), Y.match.time = c(2004:2012),
                                                 X = c( "average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                 X.avg.time = list(c(2004:2012), c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                 index = c("municipality","year"), demean = T, estimator = "meanfirst")
  storegaps_manufacturing_2012_covariates[,i] <- 
    i <- i + 1
  manufacturing_placebo_2012_covariates$att
 
} # close loop over control units\
results_manufact_mun_12<- as.data.frame(do.call(cbind, results_manufact_mun_12))
beep()


colnames(results_manufact_mun_12) <- unique(mid_size_m$municipality) 
storegaps1_manufacturing_2012_covariates<- results_manufact_mun_12[,-88]# exclude the treated unit 
storegaps_manufacturing_2012_covariates <- cbind(SCHIO,storegaps1_manufacturing_2012_covariates) # attach the initial result from the main estimation 

#####C. RMSE function and calculate this for the pre and post treatment period-------------------------------------------------------------

preloss_manufacturing <- apply(storegaps_manufacturing_2012_covariates[1:9,],2,rmse)

postloss_manufacturing <- apply(storegaps_manufacturing_2012_covariates[10:14,],2,rmse)
check_2012_covariates_manufacturing <- as.data.frame(postloss_manufacturing/preloss_manufacturing)
check_2012_covariates_manufacturing$rank <- rank(-check_2012_covariates_manufacturing$`postloss_manufacturing/preloss_manufacturing`)
municipalities_names <- names(postloss_manufacturing)

check_2012_covariates_manufacturing<- cbind(municipalities_names, check_2012_covariates_manufacturing)


#####D. Save the placebo results------------------------------------------------------------


write.csv(check_2012_covariates_manufacturing,file = paste0(results_dir,"output/","out_placebo_manufact.csv"))


###2.22 Impact on LMAs ----------------------------------------------------------

lma_2012$year<- as.numeric(lma_2012$year)
lma_2012<- lma_2012 %>% select(-year_2)
lma_2012<- lma_2012 %>% 
  mutate(tradable = manufacturing + finance + business_activities + electricity_gas_water,
         non_tradable = construction + wholesale_retail + transport, #+ education + health_social,
         log_manufacturing = log(manufacturing),
         log_tradable= log(tradable),
         log_non_tradable = log(non_tradable))


####2.221 Set up ----------------------------------------------------------


out_manufacturing_lma_shift_2012.kbal <- tjbal(data = lma_2012, Y = "log_manufacturing", D = "treat_2012", Y.match.time = c(2004:2012), 
                                                     X = c("log_non_tradable","pop_2011","popolazione_residente_totale_da_6_anni_e_pi_laurea_vecchio_e_nuovo_ordinamento_diplomi_universitari_diplomi_terziari_di_tipo_non_universitario_vecchio_e_nuovo_ordinamento_2011","edifici_e_complessi_di_edifici_utilizzati_ad_uso_produttivo_commerciale_direzionale_terziario_turistico_ricettivo_servizi_altro_2011"), 
                                                     X.avg.time = list(c(2004:2011), c(2011),c(2011), c(2011)),
                                                     index = c("ttwa_2011","year"), demean = TRUE,estimator = "meanfirst")



ttwa_names<-as.matrix(unique(lma_2012$ttwa_2011))
ttwa_names<- ttwa_names[-176]
ttwa_names<-as.matrix(ttwa_names)
weights_manufact_ttwa<-out_manufacturing_lma_shift_2012.kbal$weights.co
weights_manufact_ttwa<-as.matrix(weights_manufact_ttwa)
rownames(weights_manufact_ttwa)<-ttwa_names

weights_manufact_ttwa <- as.matrix(weights_manufact_ttwa[ order(row.names(weights_manufact_ttwa)), ])
SCHIO <- 0
weights_manufact_ttwa<-rbind(SCHIO,weights_manufact_ttwa);weights_manufact_ttwa
weights_manufact_ttwa<- as.data.frame(weights_manufact_ttwa)
top_10_manufact_ttwa <- slice_max(weights_manufact_ttwa,order_by = V1, n=10) %>% rename("TBM weights" = V1)


####2.222 Save the results ----------------------------------------------------------

saveRDS(out_manufacturing_lma_shift_2012.kbal,file = paste0(results_dir,"output/", "out_manufacturing_2012_lma.rds"))# 
write_csv(top_10_manufact_ttwa, paste0(results_dir, "output/", "top_10_manufact_ttwa.csv"))

#lma_2012<- lma_2012 %>% select(-'...1')

####2.223 Placebo ----------------------------------------------------------
#####A. Set up ----------------------------------------------------------
id <- rep(1:216, each = 14)
lma_2012<- lma_2012 %>% 
  arrange(ttwa_2011)
lma_2012<- cbind(id, lma_2012)
y<-14
max<-3024
n<-(max/y)
t <-10
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:216)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
lma_m<-cbind(lma_2012, M) 

SCHIO<- out_manufacturing_lma_shift_2012.kbal$att

# loop across control units
storegaps_manufacturing_2012_lma_covariates<- 
  matrix(NA,
         length(1:y),
         length(unique(lma_m$id))
  )
rownames(storegaps_manufacturing_2012_lma_covariates) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)


#####B. Estimate the placebo effect ----------------------------------------------------------

results_manufact_lma_12<- foreach(k= 1:n ) %dopar% {
  manufacturing_placebo_2012_lma_covariates <- tjbal(data = lma_m, "log_manufacturing", D =   paste("treat_", k, sep = "" ), Y.match.time = c(2004:2012),
                                                     X = c("log_non_tradable","pop_2011","popolazione_residente_totale_da_6_anni_e_pi_laurea_vecchio_e_nuovo_ordinamento_diplomi_universitari_diplomi_terziari_di_tipo_non_universitario_vecchio_e_nuovo_ordinamento_2011","edifici_e_complessi_di_edifici_utilizzati_ad_uso_produttivo_commerciale_direzionale_terziario_turistico_ricettivo_servizi_altro_2011"), 
                                                     X.avg.time = list(c(2004:2011), c(2011),c(2011), c(2011)),
                                                     index = c("ttwa_2011","year"), demean = TRUE,estimator = "meanfirst")
  storegaps_manufacturing_2012_lma_covariates[,i] <- 
    i <- i + 1
  manufacturing_placebo_2012_lma_covariates$att
  
} # close loop over control units
results_manufact_lma_12<- as.data.frame(do.call(cbind, results_manufact_lma_12))
beep()
colnames(results_manufact_lma_12) <- unique(lma_m$ttwa_2011) 
storegaps1_manufacturing_2012_lma_covariates<- results_manufact_lma_12[,-176]# Exclude the treated unit
storegaps_manufacturing_2012_lma_covariates <- cbind(SCHIO,storegaps1_manufacturing_2012_lma_covariates)

#####C.  RMSE function and calculate this for the pre and post treatment period----------------------------------------------------------

preloss_lma_manufacturing <- apply(storegaps_manufacturing_2012_lma_covariates[1:9,],2,rmse)

postloss_lma_manufacturing <- apply(storegaps_manufacturing_2012_lma_covariates[10:14,],2,rmse)
check_2012_covariates_lma_manufacturing <- as.data.frame(postloss_lma_manufacturing/preloss_lma_manufacturing)
check_2012_covariates_lma_manufacturing$rank <- rank(-check_2012_covariates_lma_manufacturing$`postloss_lma_manufacturing/preloss_lma_manufacturing`)
lma_names <- names(postloss_lma_manufacturing)

check_2012_covariates_lma_manufacturing<- cbind(lma_names, check_2012_covariates_lma_manufacturing)


#####D.  Save the results----------------------------------------------------------


write.csv(check_2012_covariates_lma_manufacturing,file = paste0(results_dir,"output/","out_placebo_lma_manufact.csv"))

###2.23 Synthetic control municipalities----------------------------------------------------------

  id <- rep(1:106, each = 14)
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% 
  arrange(municipality)
mid_size_northern_municipalities<- cbind(id, mid_size_northern_municipalities)

municipalities_list <- mid_size_northern_municipalities %>% select(municipality) %>% distinct() %>% pull(municipality)
id <- 1:106
municipalities_id <- cbind(municipalities_list, id)
municipalities_id<- as.data.frame(municipalities_id)
municipalities_id$id<-as.numeric(municipalities_id$id)
colnames(municipalities_id)<- c("municipality", "id")

mid_size_northern_municipalities<- mid_size_northern_municipalities %>% 
  select(-id) %>% left_join(municipalities_id)
mid_size_northern_municipalities<- as.data.frame(mid_size_northern_municipalities)

dataprep_out_manufacturing <- dataprep(
  foo = mid_size_northern_municipalities,
  predictors = c("average_employee_income"),
  predictors.op = "mean",
  time.predictors.prior = 2004:2012,
  special.predictors = list(
    list("pop_2011",2011 , "mean"),
    list("pop_2001_2011", 2011, "mean"),
    list("pop_density_2011", 2011, "mean"),
    list("young_pop_degree_share", 2011, "mean"),
    list("emploment_rate_2011", 2011, "mean"),
    list("high_skilled_share", 2011, "mean")), 
  dependent = "log_manufacturing",
  unit.variable = "id",
  unit.names.variable = "municipality",
  time.variable = "year",
  treatment.identifier = 88,
  controls.identifier = c(1:87, 89:106),
  time.optimize.ssr = 2004:2012,
  time.plot = 2004:2017
)


synth_out_manufacturing <- synth(data.prep.obj = dataprep_out_manufacturing, Sigf.ipop =3,optimxmethod = "All")
saveRDS(synth_out_manufacturing ,file = paste0(results_dir,"output/", "synth_out_manufacturing.rds"))
saveRDS(dataprep_out_manufacturing, file = paste0(results_dir, "output/", "prep_out_manufacturing.rds"))
synth.tables_manufacturing <- synth.tab(dataprep.res =dataprep_out_manufacturing, 
                                        synth.res = synth_out_manufacturing)#create table with all the results 

balance_table_manufacturing <- synth.tables_manufacturing$tab.pred 
placebo_manufacturing <- generate_placebos(dataprep_out_manufacturing, synth_out_manufacturing, Sigf.ipop = 2, strategy ="multiprocess")
mspe.plot(placebo_manufacturing , discard.extreme = TRUE, mspe.limit = 10, plot.hist = TRUE)+geom_histogram(bins=30)



# ### In-time placebo  ----------------------------------------------------

# 2008 
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% mutate(treat_2008 = case_when(municipality =="SCHIO" & year>2008 ~ 1, year<2009 & municipality =="SCHIO"~ 0,
                                                                                                                        municipality!="SCHIO"~0))# This creates a treatment variable
out_manufacturing_2008_covariates.kbal <- tjbal(data = mid_size_northern_municipalities, Y = "log_manufacturing", D = "treat_2008", Y.match.time = c(2004:2008),
                                                X = c( "log_average_employee_income"),
                                                X.avg.time = list(c(2004:2008)),
                                                index = c("municipality","year"), demean = T, estimator = "meanfirst")



saveRDS(out_manufacturing_2008_covariates.kbal,file = here("results","output", "out_manufacturing_2008.rds"))

# ### Placebo in-time effect ----------------------------------------------


y<-14
max<-1484
n<-(max/y)
id<-rep(c(1:n), each=14)
mid_size_northern_municipalities <- mid_size_northern_municipalities %>% arrange(municipality, year)
t <-6
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:106)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_m<-cbind(mid_size_northern_municipalities, M) 
SCHIO<- out_manufacturing_2008_covariates.kbal$att

# loop across control units
storegaps_manufact_2008_mun<-
  matrix(NA,
         length(1:y),
         length(unique(mid_size_northern_municipalities$id))
  )
rownames(storegaps_manufact_2008_mun) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)

results_mun_manufact_2008<- foreach(k= 1:n ) %dopar% { 
  manufact_placebo_2008_mun<- tjbal(data = mid_size_m, "log_manufacturing", D = paste("treat_", k, sep = "" ), Y.match.time = c(2004:2008),
                                    X = c( "log_average_employee_income"),
                                    X.avg.time = list(c(2004:2008)),
                                    index = c("municipality","year"), demean = T, estimator = "meanfirst")
  
  
  storegaps_manufact_2008_mun[,i] <- 
    i <- i + 1
  manufact_placebo_2008_mun$att
} # close loop over control units


beep()
results_mun_manufact_2008<- as.data.frame(do.call(cbind, results_mun_manufact_2008))

colnames(results_mun_manufact_2008)<- unique(mid_size_m$municipality)
storegaps_manufact_2008_mun<- results_mun_manufact_2008
storegaps1_manufact_2008<- storegaps_manufact_2008_mun[,-88]
storegaps_manufact_2008_mun <- cbind(SCHIO,storegaps1_manufact_2008)
year<- c(2004:2017)
spaghetti_data_2008<- as.data.frame(storegaps_manufact_2008_mun)
spaghetti_data_2008<- cbind(year, spaghetti_data_2008)
spaghetti_data_2008_long <- spaghetti_data_2008 %>% pivot_longer(cols = -1, names_to = "City", values_to = "Value") %>%
  rename(Year = year)
write.csv(spaghetti_data_2008_long,file = here("results","output","spaghetti_data_2008.csv"))

# Transform data to long format
spaghetti_data_long <- spaghetti_data_2008 %>% pivot_longer(cols = -1, names_to = "City", values_to = "Value") %>%
  rename(Year = year)
year<- c(2004:2017)
names(data_long)[1] <- "Year"
spaghe

# Create the plot
ggplot(data_long, aes(x = Year, y = Value, group = City)) +
  geom_line(data = filter(data_long, City != "SCHIO"), aes(color = "Other Cities"), size = 0.2) + # Set other cities to light grey
  geom_line(data = filter(data_long, City == "SCHIO"), aes(color = "SCHIO"), size = 1) + # Highlight Schio with a thicker, dark grey line
  geom_vline(xintercept = 2008, color = "grey", linetype = "dashed", size = 0.5) + # Treatment line, thinner
  scale_color_manual(values = c("Schio" = "darkgrey", "Other Cities" = "lightgrey")) + # Define manual colors
  theme_minimal(base_size = 12) + # Use a minimal theme
  theme(legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_blank(), # Remove legend title
        plot.title = element_text(size = 14, face = "bold"), # Customize title
        axis.title = element_text(size = 12), # Customize axis titles
        axis.text = element_text(size = 10)) + # Customize axis text
  labs(title = "Spaghetti Graph for Synthetic Control Analysis",
       x = "Year",
       y = "Placebo gaps in (log) Manufacturing Employees")

##### RMSPE

rmse <- function(y){sqrt(mean(y^2))}
preloss_manufact_08<- apply(storegaps_manufact_2008_mun[1:5,],2,rmse)
postloss_manufact_08 <- apply(storegaps_manufact_2008_mun[6:14,],2,rmse)

check_covariates_manufact_08 <- as.data.frame(postloss_manufact_08/preloss_manufact_08)
check_covariates_manufact_08$rank <- rank(-check_covariates_manufact_08$`postloss_manufact_08/preloss_manufact_08`)
mun_names <- names(postloss_manufact_08)
check_covariates_manufact_08<- cbind(mun_names, check_covariates_manufact_08)

### I prepare the data for the plot
schio_manufact_08_MSPE<- cbind.data.frame(mun_names, preloss_manufact_08, postloss_manufact_08)
schio_manufact_08_MSPE<- schio_manufact_08_MSPE %>% mutate(RMSPE  = postloss_manufact_08/preloss_manufact_08)
schio_manufact_08_MSPE$rank<-rank(-schio_manufact_08_MSPE$RMSPE)

write.csv(schio_manufact_08_MSPE,file = here("results","output","out_placebo_manufact_2008.csv"))




  
  
  
#2010

out_manufacturing_2010_covariates.kbal <- tjbal(data = mid_size_northern_municipalities, Y = "log_manufacturing", D = "treat_2010", Y.match.time = c(2004:2010),
                                                X = c( "log_average_employee_income"),
                                                X.avg.time = list(c(2004:2010)),
                                                index = c("municipality","year"), demean = T, estimator = "meanfirst")


plot(out_manufacturing_2010_covariates.kbal, type = "counterfactual", xlab = "Year",
     ylab = "Manufacturing employees", main="Panel a: Trends in (log) manufacturing, municipality",
     legend.pos = "top",
     legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, ylim = c(8.7,9.1),
     cex.lab = 1 ,count = FALSE)

saveRDS(out_manufacturing_2010_covariates.kbal,file = here("results","output", "out_manufacturing_2010.rds"))

# ### Placebo in-time effect ----------------------------------------------


y<-14
max<-1484
n<-(max/y)
id<-rep(c(1:n), each=14)
mid_size_northern_municipalities <- mid_size_northern_municipalities %>% arrange(municipality, year)
t <-8
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:106)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_m<-cbind(mid_size_northern_municipalities, M) 
SCHIO<- out_manufacturing_2010_covariates.kbal$att

# loop across control units
storegaps_manufact_2010_mun<-
  matrix(NA,
         length(1:y),
         length(unique(mid_size_northern_municipalities$id))
  )
rownames(storegaps_manufact_2010_mun) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)

results_mun_manufact_2010<- foreach(k= 1:n ) %dopar% { 
  manufact_placebo_2010_mun<- tjbal(data = mid_size_m, "log_manufacturing", D = paste("treat_", k, sep = "" ), Y.match.time = c(2004:2010),
                                    X = c( "log_average_employee_income"),
                                    X.avg.time = list(c(2004:2009)),
                                    index = c("municipality","year"), demean = T, estimator = "meanfirst")
  
  
  storegaps_manufact_2010_mun[,i] <- 
    i <- i + 1
  manufact_placebo_2010_mun$att
} # close loop over control units


beep()
results_mun_manufact_2010<- as.data.frame(do.call(cbind, results_mun_manufact_2010))

colnames(results_mun_manufact_2010)<- unique(mid_size_m$municipality)
storegaps_manufact_2010_mun<- results_mun_manufact_2010
storegaps1_manufact_2010<- storegaps_manufact_2010_mun[,-88]
storegaps_manufact_2010_mun <- cbind(SCHIO,storegaps1_manufact_2010)

spaghetti_data_2010<- as.data.frame(storegaps_manufact_2010_mun
                                    )

year<- c(2004:2017)
spaghetti_data_2010<- cbind(year, spaghetti_data_2010)

spaghetti_data_2010_long <- spaghetti_data_2010 %>%
  pivot_longer(cols = -1, names_to = "City", values_to = "Value") %>%
  rename(Year = year) # Ensure the year column is numeric
write.csv(spaghetti_data_2010_long, here("results", "output", "spaghetti_data_2010"))


##### RMSPE

rmse <- function(y){sqrt(mean(y^2))}
preloss_manufact_10<- apply(storegaps_manufact_2010_mun[1:7,],2,rmse)
postloss_manufact_10 <- apply(storegaps_manufact_2010_mun[8:14,],2,rmse)

check_covariates_manufact_10 <- as.data.frame(postloss_manufact_10/preloss_manufact_10)
check_covariates_manufact_10$rank <- rank(-check_covariates_manufact_10$`postloss_manufact_10/preloss_manufact_10`)
mun_names <- names(postloss_non_tradable_mun)
check_covariates_manufact_10<- cbind(mun_names, check_covariates_manufact_10)

### I prepare the data for the plot
schio_manufact_10_MSPE<- cbind.data.frame(municipalities_names, preloss_manufact_10, postloss_manufact_10)
schio_manufact_10_MSPE<- schio_non_tradable_MSPE %>% mutate(RMSPE  = postloss_manufact_10/preloss_manufact_10)
schio_manufact_10_MSPE$rank<-rank(-schio_manufact_10_MSPE$RMSPE)

write.csv(schio_manufact_10_MSPE,file = here("results","output","out_placebo_manufact_2010.csv"))




# ### Placebo in-time effect centered around 2010----------------------------------------------
# First step would be to center it around 2008 by truncating the data
#2010

out_manufacturing_2010_centered_covariates.kbal <- tjbal(data = mid_size_northern_municipalities_centered_2014, Y = "log_manufacturing", D = "treat_2010", Y.match.time = c(2004:2010),
                                                X = c( "log_average_employee_income"),
                                                X.avg.time = list(c(2004:2010)),
                                                index = c("municipality","year"), demean = T, estimator = "meanfirst")

saveRDS(out_manufacturing_2010_centered_covariates.kbal,file = here("results","output", "out_manufacturing_2010_centered.rds"))




mid_size_northern_municipalities_centered_2014<- mid_size_northern_municipalities %>% filter(year<2015)
mid_size_northern_municipalities_centered_2012<- mid_size_northern_municipalities %>% filter(year<2013)

y<-11
max<-1166
n<-(max/y)
id<-rep(c(1:n), each=11)
mid_size_northern_municipalities_centered_2014 <- mid_size_northern_municipalities_centered_2014 %>% arrange(municipality, year)
t <-8
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:106)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_m<-cbind(mid_size_northern_municipalities_centered_2014, M) 
SCHIO<- out_manufacturing_2010_covariates.kbal$att

# loop across control units
storegaps_manufact_2010_centered_mun<-
  matrix(NA,
         length(1:y),
         length(unique(mid_size_northern_municipalities_centered_2014$id))
  )
rownames(storegaps_manufact_2010_centered_mun) <- 1:11

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)

results_mun_manufact_2010_centered<- foreach(k= 1:n ) %dopar% { 
  manufact_placebo_2010_centered_mun<- tjbal(data = mid_size_m, "log_manufacturing", D = paste("treat_", k, sep = "" ), Y.match.time = c(2004:2010),
                                    X = c( "log_average_employee_income"),
                                    X.avg.time = list(c(2004:2009)),
                                    index = c("municipality","year"), demean = T, estimator = "meanfirst")
  
  
  storegaps_manufact_2010_centered_mun[,i] <- 
    i <- i + 1
  manufact_placebo_2010_centered_mun$att
} # close loop over control units


beep()
results_mun_manufact_2010_centered<- as.data.frame(do.call(cbind, results_mun_manufact_2010_centered))

colnames(results_mun_manufact_2010_centered)<- unique(mid_size_m$municipality)
storegaps_manufact_2010_mun_centered<- results_mun_manufact_2010_
storegaps1_manufact_2010_mun_centered<- storegaps_manufact_2010_mun_centered[,-88]
storegaps_manufact_2010_mun_centered <- cbind(SCHIO,storegaps1_manufact_2010_mun_centered)


##### RMSPE

rmse <- function(y){sqrt(mean(y^2))}
preloss_manufact_10_centered<- apply(storegaps_manufact_2010_mun_centered[1:7,],2,rmse)
postloss_manufact_10_centered <- apply(storegaps_manufact_2010_mun_centered[8:11,],2,rmse)

check_covariates_manufact_10_centered <- as.data.frame(postloss_manufact_10_centered/preloss_manufact_10_centered)
check_covariates_manufact_10_centered$rank <- rank(-check_covariates_manufact_10_centered$`postloss_manufact_10_centered/preloss_manufact_10_centered`)
#mun_names <- names(postloss_non_tradable_mun)
#check_covariates_manufact_10_centered<- cbind(mun_names, check_covariates_manufact_10)

### I prepare the data for the plot
schio_manufact_10_MSPE<- cbind.data.frame(municipalities_names, preloss_manufact_10, postloss_manufact_10)
schio_manufact_10_MSPE<- schio_non_tradable_MSPE %>% mutate(RMSPE  = postloss_manufact_10/preloss_manufact_10)
schio_manufact_10_MSPE$rank<-rank(-schio_manufact_10_MSPE$RMSPE)

write.csv(schio_manufact_10_MSPE,file = here("results","output","out_placebo_manufact_2010.csv"))
# Leave-on-out analysis ---------------------------------------------------




### Municipality

top_10_manufact_mun_list <- row.names(top_10_manufact_mun)


# This loops through each dataset and saves the att

d<-14
storegapsL_manufacturing <- 
  matrix(NA,
         length(1:d),
         10)
controls<-rownames(top_10_manufact_mun)
colnames(storegapsL_manufacturing) <- controls

weightL_manufacturing <- 
  matrix(NA,
         length(1:104),
         10)
colnames(weightL_manufacturing) <- controls

#Create 8 datasets, removing one-at-a-time each of the municipalities that contribute to the counterfactual
listdb <- list()
for (s in 1:10){
  db_ <- subset(mid_size_northern_municipalities, mid_size_northern_municipalities$municipality!= controls[s])
  listdb[[s]] <- db_
}
i<-1
# loop over leave one outs to save gaps


for(k in 1:10 ){
  out_manufacturing_l1o.kbal <- tjbal(data = listdb[[k]], Y = "log_manufacturing", D = "treat_2012", 
                                      Y.match.time = c(2004:2012),
                                      X = c( "average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011", "emploment_rate_2011"),
                                      X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011), c(2011)),
                                      index = c("municipality","year"), demean = T, estimator = "meanfirst")
  
  
  storegapsL_manufacturing[,i] <-  
    out_manufacturing_l1o.kbal$att
  
  weightL_manufacturing[,i] <-  
    out_manufacturing_l1o.kbal$weights.co
  i <- i + 1
} # close loop over leave one outs

saveRDS(out_manufacturing_l1o.kbal,file = here("results","output", "out_manufacturing_l1o.rds"))
write.table(storegapsL_manufacturing, here("results","output","leave_one_out_mun_mat.txt"), sep = "\t", row.names = FALSE, col.names = FALSE)

#Figure 3: Leave-one-out distribution of the Synthetic 
Y1<-out_manufacturing_shift_2012_covariates.kbal$Y.bar[,1]
Y0<-out_manufacturing_shift_2012_covariates.kbal$Y.bar[,2]
Yleave<-as.matrix(cbind(Y1,Y1,Y1,Y1,Y1,Y1,Y1,Y1, Y1, Y1))
Yleave<- as.matrix(Yleave)
Yleave<-as.matrix(Yleave-storegapsL_manufacturing)

leave_one_out_manufacturing_plot<- plot(2004:2017, Y1,
                                        type="l",ylim=c(8.7,9.1), col="black",lty="solid",
                                        ylab ="Log Manufacturing employees",
                                        xlab ="Year",
                                        cex.lab = 1.6, cex.main = 1.6, cex.axis=1.6,
                                        main = NULL,
                                        xaxs = "i", yaxs = "i",
                                        lwd=2
)
abline(v=2012,lty="solid", col="grey", lwd=3)
for(i in 1:7){
  lines(2004:2017,Yleave[,i],col="orange",lty="solid")
}
lines(2004:2017,Y0,col="blue",lty="dashed", lwd=3)
lines(2004:2017,Y1,col="black",lty="solid",lwd=2)
legend(x="bottomleft",
       legend=c("Schio",
                "Synthetic Schio",
                "Synthetic Schio (leave-one-out)")
       ,lty=c("solid","dashed","solid"),
       col=c("black","blue","orange"),
       cex=1, bg="white",lwdc(3,4,3), bty = "n")


leave_one_out_manufacturing_est<- mean(storegapsL_manufacturing) 



# Leave-one-out LMA manufacturing -----------------------------------------



ttwa_names<-as.matrix(unique(lma_2012$ttwa_2011))
ttwa_names<- ttwa_names[-176]
ttwa_names<-as.matrix(ttwa_names)
weights_manufact_ttwa<-out_manufacturing_lma_shift_2012.kbal$weights.co
weights_manufact_ttwa<-as.matrix(weights_manufact_ttwa)
rownames(weights_manufact_ttwa)<-ttwa_names

weights_manufact_ttwa <- as.matrix(weights_manufact_ttwa[ order(row.names(weights_manufact_ttwa)), ])
SCHIO <- 0
weights_manufact_ttwa<-rbind(SCHIO,weights_manufact_ttwa);weights_manufact_ttwa
weights_manufact_ttwa<- as.data.frame(weights_manufact_ttwa)
top_10_manufact_ttwa <- slice_max(weights_manufact_ttwa,order_by = V1, n=10) %>% rename("TBM weights" = V1)

#Figure 3 & Table D.1: Leave-one-out distribution of the Synthetic Schio (for the 8 controls receiving weights)

# This loops through each dataset and saves the att

d<-14
storegapsL_manufacturing_ttwa <- 
  matrix(NA,
         length(1:d),
         10)
controls<-rownames(top_10_manufact_ttwa)
colnames(storegapsL_manufacturing_ttwa) <- controls

weightL_manufacturing_ttwa <- 
  matrix(NA,
         length(1:214),
         10)
colnames(weightL_manufacturing_ttwa) <- controls

#Create 8 datasets, removing one-at-a-time each of the municipalities that contribute to the counterfactual
listdb <- list()
for (s in 1:10){
  db_ <- subset(lma_2012, lma_2012$ttwa_2011!= controls[s])
  listdb[[s]] <- db_
}
i<-1
# loop over leave one outs to save gaps      
for(k in 1:10 ){
  out_manufacturing_ttwa_l1o.kbal <- tjbal(data = listdb[[k]], Y = "log_manufacturing", D = "treat_2012", 
                                           Y.match.time = c(2004:2012),
                                           X = c("pop_2011","popolazione_residente_totale_da_6_anni_e_pi_laurea_vecchio_e_nuovo_ordinamento_diplomi_universitari_diplomi_terziari_di_tipo_non_universitario_vecchio_e_nuovo_ordinamento_2011","edifici_e_complessi_di_edifici_utilizzati_ad_uso_produttivo_commerciale_direzionale_terziario_turistico_ricettivo_servizi_altro_2011"),  X.avg.time = list(c(2004:2011), c(2011),c(2011)),index = c("ttwa_2011","year"), demean = TRUE,estimator = "meanfirst")
  
  
  storegapsL_manufacturing_ttwa[,i] <-  
    out_manufacturing_ttwa_l1o.kbal$att
  
  weightL_manufacturing_ttwa[,i] <-  
    out_manufacturing_ttwa_l1o.kbal$weights.co
  i <- i + 1
} # close loop over leave one outs


saveRDS( out_manufacturing_ttwa_l1o.kbal ,file = here("results","output", "out_manufacturing_l1o_lma.rds"))



#Figure 3: Leave-one-out distribution of the Synthetic 
Y1<-out_manufacturing_lma_shift_2012.kbal$Y.bar[,1]
Y0<-out_manufacturing_lma_shift_2012.kbal$Y.bar[,2]
Yleave<-cbind(Y1,Y1,Y1,Y1,Y1,Y1,Y1,Y1, Y1, Y1)
Yleave<-as.matrix(Yleave-storegapsL_manufacturing_ttwa)

leave_one_out_manufacturing_ttwa_plot<- plot(2004:2017, Y1,
                                             type="l",ylim=c(9.5,10), col="black",lty="solid",
                                             ylab ="Manufactuing employees",
                                             xlab ="Year",
                                             cex.lab = 1.6, cex.main = 1.6, cex.axis=1.6,
                                             main = "Trends in number of manufacturing employees",
                                             xaxs = "i", yaxs = "i",
                                             lwd=2
)
abline(v=2012,lty="solid", col="grey", lwd=3)
for(i in 1:7){
  lines(2004:2017,Yleave[,i],col="orange",lty="solid")
}
lines(2004:2017,Y0,col="blue",lty="dashed", lwd=3)
lines(2004:2017,Y1,col="black",lty="solid",lwd=2)
legend(x="bottomleft",
       legend=c("Schio",
                "Synthetic Schio",
                "Synthetic Schio (leave-one-out)")
       ,lty=c("solid","dashed","solid"),
       col=c("black","blue","orange"),
       cex=1, bg="white",lwdc(3,4,3), bty = "n")


leave_one_out_manufacturing_ttwa_est<- mean(storegapsL_manufacturing_ttwa)  



# ## Displacement analysis  -----------------------------------------------


## 5.1 Municipalities in the rest of the LMA
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



### 5.11 Manufacturing



out_northern_manufacturing_spillover_2012.kbal <- tjbal(data = northern_municipalities_spillover, Y = "log_manufacturing", D = "treat_2012", Y.match.time = c(2004:2012),
                                                        X = c("pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                        X.avg.time = list(c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                        index = c("municipality","year"), demean = T, estimator = "meanfirst", vce="jack")



saveRDS(out_northern_manufacturing_spillover_2012.kbal ,file = here("Analysis", "results","output", "out_manufacturing_spillover.rds"))
### 5.12 Tradable

out_northern_tradable_spillover_2012.kbal <- tjbal(data = northern_municipalities_spillover, Y = "log_tradable", D = "treat_2012", Y.match.time = c(2004:2012),
                                                       X = c("pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                       X.avg.time = list(c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                       index = c("municipality","year"), demean = T, estimator = "meanfirst")


saveRDS(out_northern_tradable_spillover_2012.kbal ,file = here("Analysis", "results","output", "out_tradable_spillover.rds"))

beep()


### 5.13 Non-tradable

out_northern_non_tradable_spillover_2012.kbal <- tjbal(data = northern_municipalities_spillover, Y = "log_non_tradable", D = "treat_2012", Y.match.time = c(2004:2012),
                                                       X = c("pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                       X.avg.time = list(c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                       index = c("municipality","year"), demean = T, estimator = "meanfirst")


saveRDS(out_northern_non_tradable_spillover_2012.kbal ,file = paste0(results_dir,"output/", "out_non_tradable_spillover.rds"))

beep()


## 5.2 Bordering municipalities

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


### 5.21 Manufacturing


northern_municipalities_bordering<- northern_municipalities_bordering %>% 
  select(-year.y)
out_bordering_manufacturing_spillover_2012.kbal <- tjbal(data = northern_municipalities_bordering, Y = "log_manufacturing", D = "treat_neighbours", Y.match.time = c(2004:2012),
                                                         X = c("pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                         X.avg.time = list(c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                         index = c("municipality","year"), demean = T, estimator = "meanfirst", vce="jack")

beep()
saveRDS(out_bordering_manufacturing_spillover_2012.kbal ,file = here("Analysis","results","output", "out_manufacturing_bordering.rds"))

### 5.22 Tradable


out_bordering_tradable_spillover_2012.kbal <- tjbal(data = northern_municipalities_bordering, Y = "log_tradable", D = "treat_neighbours", Y.match.time = c(2004:2012),
                                                        X = c("log_manufacturing","pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                        X.avg.time = list(c(2004:2011),c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                        index = c("municipality","year"), demean = T, estimator = "meanfirst")

beep()

saveRDS(out_bordering_tradable_spillover_2012.kbal  ,file = here("Analysis","results","output", "out_tradable_bordering.rds"))



beep()


### 5.23 Non-tradable


out_bordering_non_tradable_spillover_2012.kbal <- tjbal(data = northern_municipalities_bordering, Y = "log_non_tradable", D = "treat_neighbours", Y.match.time = c(2004:2012),
                                                        X = c("log_manufacturing","pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                        X.avg.time = list(c(2004:2011),c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                        index = c("municipality","year"), demean = T, estimator = "meanfirst")

beep()

saveRDS(out_bordering_non_tradable_spillover_2012.kbal  ,file = here("Analysis","results","output", "out_non_tradable_bordering.rds"))





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

## 6. External validity 

# External validity -------------------------------------------------------


### Estimate the effect on Bergisch

siab_wide_2004_HQtreated<- siab_wide_2004_HQtreated %>% filter(year<2017)
manufacturing_tjbal_HQ <- tjbal(data = siab_wide_2004_HQtreated,  Y = "log_manufact_3", D = "treat_2012", Y.match.time = c(2004:2012),
                                index = c("district_region","year"), demean = T, estimator = "meanfirst")



fig_manufact_berg<- plot(manufacturing_tjbal_HQ, type = "counterfactual", xlab = "Year", 
                         ylab = "Manufacturing employees", main="Panel a: Trends in (log) Manufacturing, District",
                         legend.pos = "top",ylim = c(4.2, 4.6),
                         legend.labs = c("Bergischer", "Synthetic"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.05, 
                         cex.lab = 1 ,count = FALSE)
fig_manufact_berg<- fig_manufact_berg+ggtitle("Panel a: Trajectories in Manufacturing")+
  theme(axis.title=element_text(size=12),
        panel.background = element_blank())





### Placebo effects


options(knitr.duplicate.label = "allow")
y<-14
max<-4438
n<-(max/y)
id<-rep(c(1:n), each=14)
siab_wide_2004_HQtreated<- siab_wide_2004_HQtreated %>% arrange(district_region)
siab_wide_2004_HQtreated_id<- cbind(id, siab_wide_2004_HQtreated) 
t <-10
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:317)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
siab_wide_2004_HQtreated_id<- siab_wide_2004_HQtreated_id %>% arrange(id)
siab_wide_HQ_m<-cbind(siab_wide_2004_HQtreated_id, M) 

Bergischer<- manufacturing_tjbal_HQ$att

# loop across control units
storegaps_manufact_3_HQ<- 
  matrix(NA,
         length(1:y),
         length(unique(siab_wide_HQ_m$id))
  )
rownames(storegaps_manufact_3_HQ) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)

results_manufact_3_HQ<- foreach(k= 1:n ) %dopar% { 
  manufacturing_placebo_HQ<- tjbal(data = siab_wide_HQ_m, "log_manufact_3", D = paste("treat_", k, sep = "" ), 
                                   index = c("district_region","year"), demean = T, estimator = "meanfirst")
  
  
  storegaps_manufact_3_HQ[,i] <- 
    i <- i + 1
  manufacturing_placebo_HQ$att
} # close loop over control units



results_manufact_3_HQ<- as.data.frame(do.call(cbind, results_manufact_3_HQ))

colnames(results_manufact_3_HQ)<- unique(siab_wide_HQ_m$district_region)
storegaps_manufact_3_HQ<- results_manufact_3_HQ

storegaps1_manufact_3_HQ<- storegaps_manufact_3_HQ[,-236]
storegaps1_manufact_3_HQ<- storegaps1_manufact_3_HQ[1:13,]
storegaps_manufact_3_HQ <- cbind(Bergischer,storegaps1_manufact_3_HQ)


#####

rmse <- function(y){sqrt(mean(y^2))}



preloss_manufact_HQ<- apply(storegaps_manufact_3_HQ[1:9,],2,rmse)
postloss_manufact_HQ <- apply(storegaps_manufact_3_HQ[10:13,],2,rmse)
check_manufact_placebo_HQ<- as.data.frame(postloss_manufact_HQ/preloss_manufact_HQ)


check_manufact_placebo_HQ<- as.data.frame(postloss_manufact_HQ/preloss_manufact_HQ)
check_manufact_placebo_HQ<- check_manufact_placebo_HQ%>% 
  rename(rmspe_ratio = 'postloss_manufact_HQ/preloss_manufact_HQ')
check_manufact_placebo_HQ$rank <- rank(-check_manufact_placebo_HQ$rmspe_ratio)




### Prepare the data for the plot

### I prepare the data for the plot
district_names <- names(preloss_manufact_HQ)
bergischer_MSPE<- cbind.data.frame(district_names, preloss_manufact_HQ, postloss_manufact_HQ)
bergischer_MSPE<- bergischer_MSPE %>% mutate(RMSPE  = postloss_manufact_HQ/preloss_manufact_HQ)
bergischer_MSPE$rank<-rank(-bergischer_MSPE$RMSPE)


### Create plot for placebo


fig_placebo_mun_bergischer<- ggplot(data = bergischer_MSPE, aes(y =sample(seq_along(RMSPE)), x = (RMSPE/1000000000000))) +
  geom_point(data = subset(bergischer_MSPE, district_names == "Bergischer"), color = "red") +
  geom_point(data = subset(bergischer_MSPE, district_names != "Bergischer"))+
  geom_text(data = subset(bergischer_MSPE, district_names == "Bergischer"), aes(label = district_names))+
  theme(legend.position = "none",axis.text.y = element_blank(),
        plot.title=element_text(size=12,face="bold"),
        panel.background = element_blank(), 
        axis.line.x = element_line(size = 0.5, linetype = "solid"))+
  ylab(NULL)+
  xlab("RMSPE Ratio") +ggtitle("Panel b: RMSPE-ratio distribution")


## Putting the two plots together 

plot_grid(fig_manufact_berg, fig_placebo_mun_bergischer)



# synthetic control -------------------------------------------------------

id <- rep(1:251, each = 14)
mid_size_northern_municipalities<- mid_size_northern_municipalities %>% 
  arrange(municipality)
mid_size_northern_municipalities<- cbind(id, mid_size_northern_municipalities)

municipalities_list <- mid_size_northern_municipalities %>% select(municipality) %>% distinct() %>% pull(municipality)
id <- 1:251
municipalities_id <- cbind(municipalities_list, id)
municipalities_id<- as.data.frame(municipalities_id)
municipalities_id$id<-as.numeric(municipalities_id$id)
colnames(municipalities_id)<- c("municipality", "id")

mid_size_northern_municipalities<- mid_size_northern_municipalities %>% 
  select(-id) %>% left_join(municipalities_id)
mid_size_northern_municipalities<- as.data.frame(mid_size_northern_municipalities)

dataprep_out_manufacturing <- dataprep(
  foo = mid_size_northern_municipalities,
  predictors = c("average_employee_income"),
  predictors.op = "mean",
  time.predictors.prior = 2004:2012,
  special.predictors = list(
    list("pop_2011",2011 , "mean"),
    list("pop_2001_2011", 2011, "mean"),
    list("pop_density_2011", 2011, "mean"),
    list("young_pop_degree_share", 2011, "mean"),
    list("emploment_rate_2011", 2011, "mean"),
    list("high_skilled_share", 2011, "mean")), 
  dependent = "log_manufacturing",
  unit.variable = "id",
  unit.names.variable = "municipality",
  time.variable = "year",
  treatment.identifier = 213,
  controls.identifier = c(1:212, 214:251),
  time.optimize.ssr = 2004:2012,
  time.plot = 2004:2017
)


synth_out_manufacturing <- synth(data.prep.obj = dataprep_out_manufacturing, Sigf.ipop =3,optimxmethod = "All")
saveRDS(synth_out_manufacturing ,file = paste0(results_dir,"output/", "synth_out_manufacturing.rds"))
saveRDS(dataprep_out_manufacturing, file = paste0(results_dir, "output/", "prep_out_manufacturing.rds"))
synth.tables_manufacturing <- synth.tab(dataprep.res =dataprep_out_manufacturing, 
                                        synth.res = synth_out_manufacturing)#create table with all the results 

balance_table_manufacturing <- synth.tables_manufacturing$tab.pred 
placebo_manufacturing <- generate_placebos(dataprep_out_manufacturing, synth_out_manufacturing, Sigf.ipop = 2, strategy ="multiprocess")
mspe.plot(placebo_manufacturing , discard.extreme = TRUE, mspe.limit = 10, plot.hist = TRUE)+geom_histogram(bins=30)


# Calculating the weights -------------------------------------------------


### Manufacturing --------------------------------------------------------



manufacturing_results <- readRDS(here("Analysis", "results","output", "out_manufacturing_2012_mun.rds"))# 

municipalities<-as.matrix(unique(mid_size_northern_municipalities$municipality))
municipalities<- municipalities[-88]
municipalities<-as.matrix(municipalities)
weights<-manufacturing_results$weights.co
weights<-as.matrix(weights)
rownames(weights)<-municipalities


weights <- as.matrix(weights[ order(row.names(weights)), ])

weights_manufact<- as.data.frame(weights)
weights_manufact$municipality <- row.names(weights_manufact)
top_weights_manufact <-weights_manufact %>% rename(weights = V1) %>% 
  filter(weights>0.01)

# Set row names to NULL
row.names(top_weights_manufact) <- NULL

# ### Tradable ------------------------------------------------------------

tradable_results<- readRDS(here("Analysis","results","output", "out_tradable_mun.rds"))

#municipalities<- municipalities[-88]
weights_tradable<-tradable_results$weights.co
weights_tradable<-as.matrix(weights_tradable)
rownames(weights_tradable)<-municipalities

weights_tradable <- as.matrix(weights_tradable[ order(row.names(weights_tradable)), ])

weights_tradable<- as.data.frame(weights_tradable)
weights_tradable$municipality <- row.names(weights_tradable)
top_weights_tradable <-weights_tradable %>% rename(weights = V1) %>% 
  filter(weights>0.01)

# Set row names to NULL
row.names(top_weights_tradable) <- NULL


# ### Non-Tradable ------------------------------------------------------------

non_tradable_results<- readRDS(here("Analysis","results","output", "out_non_tradable_mun.rds"))

#municipalities<- municipalities[-88]
weights_non_tradable<-non_tradable_results$weights.co
weights_non_tradable<-as.matrix(weights_non_tradable)
rownames(weights_non_tradable)<-municipalities

weights_non_tradable <- as.matrix(weights_non_tradable[ order(row.names(weights_non_tradable)), ])

weights_non_tradable<- as.data.frame(weights_non_tradable)
weights_non_tradable$municipality <- row.names(weights_non_tradable)
top_weights_non_tradable <-weights_non_tradable %>% rename(weights = V1) %>% 
  filter(weights>0.01)

# Set row names to NULL
row.names(top_weights_non_tradable) <- NULL

# ### Orbis analysis ------------------------------------------------------

orbis_25<- readRDS(here("Analysis","results","output", "out_manufacturing_orbis_25.rds"))
municipalities_orbis<-as.matrix(unique(mid_size_northern_municipalities_orbis$municipality))
municipalities_orbis<- municipalities_orbis[-76]
weights_orbis_25<-orbis_25$weights.co
weights_orbis_25<-as.matrix(weights_orbis_25)
rownames(weights_orbis_25)<-municipalities_orbis

weights_orbis_25 <- as.matrix(weights_orbis_25[ order(row.names(weights_orbis_25)), ])

weights_orbis_25<- as.data.frame(weights_orbis_25)
weights_orbis_25$municipality <- row.names(weights_orbis_25)

top_weights_orbis_25 <-weights_orbis_25 %>% rename(weights = V1) %>% 
  filter(weights>0.01)

# Set row names to NULL
row.names(top_weights_orbis_25) <- NULL



#### List of municipalities ------------------------------------------------------------
weights_all <- rbind(top_weights_manufact,top_weights_tradable, top_weights_non_tradable, top_weights_orbis_25) %>% distinct()
weights_all <- weights_all %>% select(municipality) %>% distinct()
list_of_municipalities_weights<- as.vector(weights_all$municipality)
list_of_municipalities_weights<- c(list_of_municipalities_weights,"SCHIO")



# Analysis with smaller sample --------------------------------------------

# ### Manufacturing -------------------------------------------------------


mid_size_northern_municipalities_weights<- mid_size_northern_municipalities %>% filter(municipality %in% list_of_municipalities_weights)
mid_size_northern_municipalities_weights$log_average_employee_income<- log(mid_size_northern_municipalities_weights$average_employee_income)
out_manufacturing_shift_2012_covariates_selected.kbal <- tjbal(data = mid_size_northern_municipalities_weights, Y = "log_manufacturing", D = "treat_2012", Y.match.time = c(2004:2012),
                                                      X = c( "log_average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011", "emploment_rate_2011"),
                                                      X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                      index = c("municipality","year"), demean = T, estimator = "meanfirst")

saveRDS(out_manufacturing_shift_2012_covariates_selected.kbal,file = here("Analysis","results","output", "out_manufacturing_2012_mun_weights.rds"))# 
plot(out_manufacturing_shift_2012_covariates_selected.kbal, type = "counterfactual", xlab = "Year", 
     ylab = "Log Average turnover", main="Panel a: Trends in average turnover in Metal products (25)",
     legend.pos = "top",
     legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, 
     cex.lab = 1 ,count = FALSE)



## I have to include the balance table here
### Placebo test ------------------------------------------------------------
y<-14
max<-826
n<-(max/y)
t <-10
id<-rep(c(1:n), each=14)
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:59)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_m<-cbind(mid_size_northern_municipalities_weights, M,id) 

SCHIO<- out_manufacturing_shift_2012_covariates_selected.kbal$att

# loop across control units
storegaps_manufacturing_2012_covariates_selected<- 
  matrix(NA,
         length(1:y),
         length(unique(mid_size_m$id))
  )
rownames(storegaps_manufacturing_2012_covariates_selected) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)



mid_size_m<- mid_size_m %>% rename(treatment_2012 = treat_2010, treatment_2010= treat_2010)

results_manufact_mun_12_selected<- foreach(k= 1:n ) %dopar% {
  manufacturing_placebo_2012_covariates_selected <- tjbal(data = mid_size_m, "log_manufacturing", D =   paste("treat_", k, sep = "" ), Y.match.time = c(2004:2012),
                                                 X = c( "log_average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","young_pop_degree_share","pop_2001_2011", "emploment_rate_2011"),
                                                 X.avg.time = list(c(2004:2012), c(2011), c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                 index = c("municipality","year"), demean = T, estimator = "meanfirst")
  storegaps_manufacturing_2012_covariates_selected[,i] <- 
    i <- i + 1
  manufacturing_placebo_2012_covariates_selected$att
  
} # close loop over control units\
results_manufact_mun_12_selected<- as.data.frame(do.call(cbind, results_manufact_mun_12_selected))
beep()


colnames(results_manufact_mun_12_selected) <- unique(mid_size_m$municipality) 
storegaps1_manufacturing_2012_covariates_selected<- results_manufact_mun_12_selected[,-38]
storegaps_manufacturing_2012_covariates_selected <- cbind(SCHIO,storegaps1_manufacturing_2012_covariates_selected)


##### Now I create the RMSE function and calculate this for the pre and post treatment period

rmse <- function(y){sqrt(mean(y^2))}
preloss_manufacturing_selected <- apply(storegaps_manufacturing_2012_covariates_selected[1:9,],2,rmse)

postloss_manufacturing_selected <- apply(storegaps_manufacturing_2012_covariates_selected[10:14,],2,rmse)
check_2012_covariates_manufacturing_weights <- as.data.frame(postloss_manufacturing_selected/preloss_manufacturing_selected)
check_2012_covariates_manufacturing_weights$rank <- rank(-check_2012_covariates_manufacturing_weights$`postloss_manufacturing_selected/preloss_manufacturing_selected`)
municipalities_names <- names(postloss_manufacturing_selected)

check_2012_covariates_manufacturing_weights<- cbind(municipalities_names, check_2012_covariates_manufacturing_weights)

write.csv(check_2012_covariates_manufacturing_weights,file = here("Analysis", "results","output","out_placebo_manufact_weights.csv"))



# Non-tradable ------------------------------------------------------------


out_non_tradable_mun_2012_covariates_weights.kbal <- tjbal(data = mid_size_northern_municipalities_weights, Y = "log_non_tradable", D = "treat_2012", Y.match.time = c(2004:2012),
                                                   X = c("log_manufacturing", "log_average_employee_income", "pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011", "emploment_rate_2011"),
                                                   X.avg.time = list(c(2004:2012),c(2004:2012),c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                   index = c("municipality","year"), demean = T, estimator = "meanfirst")





saveRDS(out_non_tradable_mun_2012_covariates_weights.kbal,file = here("Analysis","results","output", "out_non_tradable_2012_weights.rds"))# 


## I have to include the balance table here
### Placebo test ------------------------------------------------------------
y<-14
max<-826
n<-(max/y)
t <-10
id<-rep(c(1:n), each=14)
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:59)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_m<-cbind(mid_size_northern_municipalities_weights, M,id)

SCHIO<- out_non_tradable_mun_2012_covariates_weights.kbal$att

# loop across control units
storegaps_non_tradable_2012_covariates_selected<- 
  matrix(NA,
         length(1:y),
         length(unique(mid_size_m$id))
  )
rownames(storegaps_non_tradable_2012_covariates_selected) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)



mid_size_m<- mid_size_m %>% rename(treatment_2012 = treat_2010, treatment_2010= treat_2010)

results_non_tradable_mun_12_selected<- foreach(k= 1:n ) %dopar% {
  non_tradable_placebo_2012_covariates_selected <- tjbal(data = mid_size_m, "log_non_tradable", D =   paste("treat_", k, sep = "" ), Y.match.time = c(2004:2012),
                                                         X = c("log_manufacturing", "log_average_employee_income", "pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011", "emploment_rate_2011"),
                                                         X.avg.time = list(c(2004:2012),c(2004:2012),c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                          index = c("municipality","year"), demean = T, estimator = "meanfirst")
  storegaps_non_tradable_2012_covariates_selected[,i] <- 
    i <- i + 1
  non_tradable_placebo_2012_covariates_selected$att
  
} # close loop over control units\
results_non_tradable_mun_12_selected<- as.data.frame(do.call(cbind, results_non_tradable_mun_12_selected))
beep()


colnames(results_non_tradable_mun_12_selected) <- unique(mid_size_m$municipality) 
storegaps1_non_tradable_2012_covariates_selected<- results_non_tradable_mun_12_selected[,-38]
storegaps_non_tradable_2012_covariates_selected <- cbind(SCHIO,storegaps1_non_tradable_2012_covariates_selected)


##### Now I create the RMSE function and calculate this for the pre and post treatment period

rmse <- function(y){sqrt(mean(y^2))}
preloss_non_tradable_selected <- apply(storegaps_non_tradable_2012_covariates_selected[1:9,],2,rmse)

postloss_non_tradable_selected <- apply(storegaps_non_tradable_2012_covariates_selected[10:14,],2,rmse)
check_2012_covariates_non_tradable_weights <- as.data.frame(postloss_non_tradable_selected/preloss_non_tradable_selected)
check_2012_covariates_non_tradable_weights$rank <- rank(-check_2012_covariates_non_tradable_weights$`postloss_non_tradable_selected/preloss_non_tradable_selected`)
municipalities_names <- names(postloss_non_tradable_selected)

check_2012_covariates_non_tradable_weights<- cbind(municipalities_names, check_2012_covariates_non_tradable_weights)

write.csv(check_2012_covariates_non_tradable_weights,file = here("Analysis", "results","output","out_placebo_non_tradable_weights.csv"))




# Tradable ----------------------------------------------------------------
# Please note this is the rest of the tradable sector and does not include manufacturing 
mid_size_northern_municipalities_weights<- mid_size_northern_municipalities_weights %>% 
  mutate(
log_tradable =  log(finance + business_activities + electricity_gas_water))


out_tradable_mun_2012_covariates_weights.kbal <- tjbal(data = mid_size_northern_municipalities_weights, Y = "log_tradable", D = "treat_2012", Y.match.time = c(2004:2012),
                                               X = c("log_manufacturing", "log_average_employee_income","log_non_tradable", "pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011", "emploment_rate_2011"),
                                               X.avg.time = list(c(2004:2012),c(2004:2012), c(2004:2012),c(2011), c(2011), c(2011), c(2011), c(2011)),
                                               index = c("municipality","year"), demean = T, estimator = "meanfirst")


saveRDS(out_tradable_mun_2012_covariates_weights.kbal,file = here("Analysis","results","output", "out_tradable_mun_2012_covariates_weights.rds"))# 

## I have to include the balance table here
### Placebo test ------------------------------------------------------------
y<-14
max<-826
n<-(max/y)
t <-10
id<-rep(c(1:n), each=14)
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:59)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
mid_size_m<-cbind(mid_size_northern_municipalities_weights, M,id)

SCHIO<- out_tradable_mun_2012_covariates_weights.kbal$att

# loop across control units
storegaps_tradable_2012_covariates_selected<- 
  matrix(NA,
         length(1:y),
         length(unique(mid_size_m$id))
  )
rownames(storegaps_tradable_2012_covariates_selected) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)



mid_size_m<- mid_size_m %>% rename(treatment_2012 = treat_2010, treatment_2010= treat_2010)

results_tradable_mun_12_selected<- foreach(k= 1:n ) %dopar% {
  tradable_placebo_2012_covariates_selected <- tjbal(data = mid_size_m, "tradable", D =   paste("treat_", k, sep = "" ), Y.match.time = c(2004:2012),
                                                     X = c("log_manufacturing", "log_average_employee_income","log_non_tradable", "pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011", "emploment_rate_2011"),
                                                     X.avg.time = list(c(2004:2012),c(2004:2012), c(2004:2012),c(2011), c(2011), c(2011), c(2011), c(2011)),
                                                         index = c("municipality","year"), demean = T, estimator = "meanfirst")
  storegaps_tradable_2012_covariates_selected[,i] <- 
    i <- i + 1
  tradable_placebo_2012_covariates_selected$att
  
} # close loop over control units\
results_tradable_mun_12_selected<- as.data.frame(do.call(cbind, results_tradable_mun_12_selected))
beep()


colnames(results_tradable_mun_12_selected) <- unique(mid_size_m$municipality) 
storegaps1_tradable_2012_covariates_selected<- results_tradable_mun_12_selected[,-38]
storegaps_tradable_2012_covariates_selected <- cbind(SCHIO,storegaps1_tradable_2012_covariates_selected)


##### Now I create the RMSE function and calculate this for the pre and post treatment period

rmse <- function(y){sqrt(mean(y^2))}
preloss_tradable_selected <- apply(storegaps_tradable_2012_covariates_selected[1:9,],2,rmse)

postloss_tradable_selected <- apply(storegaps_tradable_2012_covariates_selected[10:14,],2,rmse)
check_2012_covariates_tradable_weights <- as.data.frame(postloss_tradable_selected/preloss_tradable_selected)
check_2012_covariates_tradable_weights$rank <- rank(-check_2012_covariates_tradable_weights$`postloss_tradable_selected/preloss_tradable_selected`)
municipalities_names <- names(postloss_tradable_selected)

check_2012_covariates_tradable_weights<- cbind(municipalities_names, check_2012_covariates_tradable_weights)

write.csv(check_2012_covariates_tradable_weights,file = here("Analysis", "results","output","out_placebo_tradable_weights.csv"))



# Linked_industries -------------------------------------------------------

mid_size_northern_municipalities_orbis_weights<- mid_size_northern_municipalities_orbis %>% filter(municipality %in% list_of_municipalities_weights)

out_manufacturing_shift_2012_orbis_25_cov_weights.kbal <- tjbal(data = mid_size_northern_municipalities_orbis_weights, Y = "log_turnover_25", D = "treat_2012", Y.match.time = c(2004:2012),
                                                        X = c("average_employee_income","pop_2011", "pop_density_2011", "high_skilled_share","pop_2001_2011"),
                                                        X.avg.time = list(c(2004:2012),c(2011), c(2011), c(2011), c(2011)),
                                                        index = c("municipality","year"), demean = T, estimator = "meanfirst")

plot(out_manufacturing_shift_2012_orbis_25_cov_weights.kbal, type = "counterfactual", xlab = "Year", 
     ylab = "Log Average turnover", main="Panel a: Trends in average turnover in Metal products (25)",
     legend.pos = "top",
     legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, 
     cex.lab = 1 ,count = FALSE)



# In-time placebo 2008 ----------------------------------------------------

mid_size_northern_municipalities_weights<- mid_size_northern_municipalities_weights %>% 
  mutate(treat_2008 = case_when(municipality =="SCHIO" & year>2008 ~ 1, year<2009 & municipality =="SCHIO"~ 0,municipality!="SCHIO"~0))

mid_size_northern_municipalities_orbis_weights_08_12<- mid_size_northern_municipalities_weights %>% filter(year <2013)

out_manufacturing_shift_2008_covariates_selected_weights.kbal<-tjbal(data = mid_size_northern_municipalities_orbis_weights_08_12, Y = "log_manufacturing", D = "treat_2008", Y.match.time = c(2004:2008),
      X = c( "log_average_employee_income"),
      X.avg.time = list(c(2004:2008)),
      index = c("municipality","year"), demean = T, estimator = "meanfirst")



plot(out_manufacturing_shift_2008_covariates_selected_weights.kbal, type = "counterfactual", xlab = "Year", 
     ylab = "Log Average turnover", main="Panel a: Trends in average turnover in Metal products (25)",
     legend.pos = "top",
     legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, 
     cex.lab = 1 ,count = FALSE)



