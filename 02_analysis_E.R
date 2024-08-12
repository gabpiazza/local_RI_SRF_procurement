# Info --------------------------------------------------------------------
##
##Script name: 02_analysis_E
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
## Notes: This is the script for effects on Germany
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
# 3. Saving the data ------------------------------------------------------
siab_wide_2004_HQtreated<- readRDS(paste0(data_proc_dir, "German_data/", "siab_wide_2004_HQtreated.rds"))
siab_wide_2004_twotreated<- readRDS(paste0(data_proc_dir, "German_data/", "siab_wide_2004_twotreated.rds"))
siab_wide_2004_dortmund<-readRDSrds(paste0(data_proc_dir, "German_data/", "siab_wide_2004_dortmund.rds"))





# 2. Analysis  ------------------------------------------------------------

##2.1 Bergisch  ------------------------------------------------------------
###2.11 Estimate the effect  ------------------------------------------------------------

siab_wide_2004_HQtreated<- siab_wide_2004_HQtreated %>% filter(year<2017)#keep the analysis to 2017
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




###2.12 Placebo effects  ------------------------------------------------------------

####2.121 Setting up ------------------------------------------------------------


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


####2.122 RMSE function ------------------------------------------------------------





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

###2.13 Save the results  ------------------------------------------------------------
saveRDS(manufacturing_tjbal_HQ, paste0(results_dir, "output/", "manufacturing_tjbal_HQ.rds"))
saveRDS(bergischer_MSPE, paste0(results_dir, "output/", "bergischer_MSPE.rds"))



##2.2 Dortmund ----------------------------------------------------------
###2.21 Estimate the effect ----------------------------------------------------------


manufacturing_tjbal_dortmund <- tjbal(data = siab_wide_2004_dortmund,  Y = "log_manufact_3", D = "treat_2012", Y.match.time = c(2004:2012),
                                      index = c("district_region","year"), demean = T, estimator = "meanfirst")

###2.22 Placebo effects  ------------------------------------------------------------

####2.221 Setting up ------------------------------------------------------------
y<-14
max<-4452
n<-(max/y)
id<-rep(c(1:n), each=14)
siab_wide_2004_dortmund_id<- cbind(id, siab_wide_2004_check) 
t <-10
M <-matrix(0, nrow = max, ncol = n)
prefix <- "treat_"
suffix <- c(1:318)
my.names<-paste(prefix, suffix, sep = "")
colnames(M)<-my.names
for(col in 1:n){
  idRow = (t + y*(col-1)):(y*col)
  M[idRow, col] <- 1
}
siab_wide_2004_dortmund_id<- siab_wide_2004_dortmund_id %>% arrange(id)
siab_wide_m<-cbind(siab_wide_2004_dortmund_id, M) 

dortmund<- manufacturing_tjbal_out$att

# loop across control units
storegaps_manufact_3_dortmund<- 
  matrix(NA,
         length(1:y),
         length(unique(siab_wide_m$id))
  )
rownames(storegaps_manufacturing_3) <- 1:14

i <- 1
numCores <- detectCores()
registerDoParallel(numCores)

results_manufact_3_dortmund<- foreach(k= 1:n ) %dopar% { 
  manufacturing_placebo_dortmund<- tjbal(data = siab_wide_m, "log_manufact_3", D = paste("treat_", k, sep = "" ), 
                                         index = c("district_region","year"), demean = T, estimator = "meanfirst")
  
  
  storegaps_manufact_3_dortmund[,i] <- 
    i <- i + 1
  manufacturing_placebo_dortmund$att
} # close loop over control units

####2.222 RMSE function ------------------------------------------------------------


results_manufact_3_dortmund<- as.data.frame(do.call(cbind, results_manufact_3_dortmund))

colnames(results_manufact_3_dortmund)<- unique(siab_wide_m$district_region)
storegaps_manufact_3_dortmund<- results_manufact_3_dortmund

storegaps1_manufact_3_dor<- storegaps_manufact_3_dortmund[,-95]
storegaps_manufact_3_dortmund <- cbind(dortmund,storegaps1_manufact_3)

preloss_manufact_dortmund<- apply(storegaps_manufact_3_dortmund[1:9,],2,rmse)
postloss_manufact_dortmund <- apply(storegaps_manufact_3_dortmund[10:14,],2,rmse)

check_manufact_placebo_dortmund<- as.data.frame(postloss_manufact_dortmund/preloss_manufact_dortmund)
check_manufact_placebo_dortmund<- check_manufact_placebo_dortmund %>% 
  rename(rmspe_ratio = postloss_manufact_dortmund/preloss_manufact_dortmund)
check_manufact_placebo_dortmund$rank <- rank(-check_manufact_placebo_dortmund$rmspe_ratio)


###2.23 Save the results  ------------------------------------------------------------
saveRDS(manufacturing_tjbal_dortmund, paste0(results_dir, "output/", "manufacturing_tjbal_dortmund.rds"))
