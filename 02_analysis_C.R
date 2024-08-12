# Info --------------------------------------------------------------------
##
##Script name: 02_analysis_C
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
## Notes: This is the script for robustness tests
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
northern_municipalities_bordering <- read_csv(paste0(data_proc_dir, "municipalities/", "northern_muncipalities_bordering.csv"))
lma_2012<- read_csv(paste0(data_proc_dir, "SLL/", "ttwa_grouped_final.csv"))
top_10_manufact_mun_list<- read_csv(paste0(results_dir, "output/", "top_10_mun_weights.csv"))
top_10_manufact_ttwa<- read_csv(results_dir, "output/", "top_10_manufact_ttwa.csv"))


# 2. Analysis -------------------------------------------------------------

## 2.1 In-time-placebo 2008 -------------------------------------------------------------
### 2.11 Estimate the effect -------------------------------------------------------------

mid_size_northern_municipalities<- mid_size_northern_municipalities %>% mutate(treat_2008 = case_when(municipality =="SCHIO" & year>2008 ~ 1, year<2009 & municipality =="SCHIO"~ 0,
                                                                                                      municipality!="SCHIO"~0))# This creates a treatment variable
out_manufacturing_2008_covariates.kbal <- tjbal(data = mid_size_northern_municipalities, Y = "log_manufacturing", D = "treat_2008", Y.match.time = c(2004:2008),
                                                X = c( "log_average_employee_income"),
                                                X.avg.time = list(c(2004:2008)),
                                                index = c("municipality","year"), demean = T, estimator = "meanfirst")


### 2.12 Save the results -------------------------------------------------------------

saveRDS(out_manufacturing_2008_covariates.kbal,file = paste0(results_dir,"output/", "out_manufacturing_2008.rds"))

### 2.13 Placebo effect -------------------------------------------------------------
#### A. Setting up  -------------------------------------------------------------


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


#### B. Estimate the placebo effect  -------------------------------------------------------------

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

#### C. Spaghetti data  -------------------------------------------------------------

colnames(results_mun_manufact_2008)<- unique(mid_size_m$municipality)
storegaps_manufact_2008_mun<- results_mun_manufact_2008
storegaps1_manufact_2008<- storegaps_manufact_2008_mun[,-88]
storegaps_manufact_2008_mun <- cbind(SCHIO,storegaps1_manufact_2008)
year<- c(2004:2017)
spaghetti_data_2008<- as.data.frame(storegaps_manufact_2008_mun)
spaghetti_data_2008<- cbind(year, spaghetti_data_2008)
spaghetti_data_2008_long <- spaghetti_data_2008 %>% pivot_longer(cols = -1, names_to = "City", values_to = "Value") %>%
  rename(Year = year)
write.csv(spaghetti_data_2008_long,file = paste0(results_dir,"output","spaghetti_data_2008.csv"))

# Transform data to long format
spaghetti_data_long <- spaghetti_data_2008 %>% pivot_longer(cols = -1, names_to = "City", values_to = "Value") %>%
  rename(Year = year)
year<- c(2004:2017)
names(data_long)[1] <- "Year"

#### D. Spaghetti graph  -------------------------------------------------------------
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

#### E. RMSE  -------------------------------------------------------------

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
#### F. Save the placebo effects  -------------------------------------------------------------

write.csv(schio_manufact_08_MSPE,file = paste0(results_dir,"output","out_placebo_manufact_2008.csv"))







## 2.2 In-time-placebo 2010 -------------------------------------------------------------
### 2.21 Estimate the effect -------------------------------------------------------------

out_manufacturing_2010_covariates.kbal <- tjbal(data = mid_size_northern_municipalities, Y = "log_manufacturing", D = "treat_2010", Y.match.time = c(2004:2010),
                                                X = c( "log_average_employee_income"),
                                                X.avg.time = list(c(2004:2010)),
                                                index = c("municipality","year"), demean = T, estimator = "meanfirst")


plot(out_manufacturing_2010_covariates.kbal, type = "counterfactual", xlab = "Year",
     ylab = "Manufacturing employees", main="Panel a: Trends in (log) manufacturing, municipality",
     legend.pos = "top",
     legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, ylim = c(8.7,9.1),
     cex.lab = 1 ,count = FALSE)

### 2.22 Save the results -------------------------------------------------------------

saveRDS(out_manufacturing_2010_covariates.kbal,file = paste0(results_dir,"output", "out_manufacturing_2010.rds"))

### 2.23 Placebo effects-------------------------------------------------------------

#### A. Setting up  -------------------------------------------------------------

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
#### B. Estimate the placebo effect  -------------------------------------------------------------

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


#### C. Spaghetti  -------------------------------------------------------------

spaghetti_data_2010<- as.data.frame(storegaps_manufact_2010_mun
)

year<- c(2004:2017)
spaghetti_data_2010<- cbind(year, spaghetti_data_2010)

spaghetti_data_2010_long <- spaghetti_data_2010 %>%
  pivot_longer(cols = -1, names_to = "City", values_to = "Value") %>%
  rename(Year = year) # Ensure the year column is numeric
write.csv(spaghetti_data_2010_long, here("results", "output", "spaghetti_data_2010"))


#### D. RMSE function  -------------------------------------------------------------

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

#### E. Save the results  -------------------------------------------------------------

write.csv(schio_manufact_10_MSPE,file = paste0(results_dir,"output","out_placebo_manufact_2010.csv"))



## 2.3 Leave-on-out analysis municipality -------------------------------------------------------------

### 2.31 Setting up -------------------------------------------------------------



### Municipality

top_10_manufact_mun_list <- row.names(top_10_manufact_mun)# where does this come from?


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
### 2.32 Estimate the effect -------------------------------------------------------------


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
### 2.33 Save the results -------------------------------------------------------------

saveRDS(out_manufacturing_l1o.kbal,file = paste0(results_dir,"output/", "out_manufacturing_l1o.rds"))
write.table(storegapsL_manufacturing, paste0(results_dir,"output/","leave_one_out_mun_mat.txt"), sep = "\t", row.names = FALSE, col.names = FALSE)

### 2.34 Leave-one-out-graph -------------------------------------------------------------

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



##2.4 Leave-one-out analysis LMA manufacturing -----------------------------------------

###2.41 Setting up -----------------------------------------


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


###2.42 Estimate the effect -----------------------------------------

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

###2.43 Save the results -----------------------------------------

saveRDS( out_manufacturing_ttwa_l1o.kbal ,file = here("results","output", "out_manufacturing_l1o_lma.rds"))


###2.44 Leave-one-out graph -----------------------------------------

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


