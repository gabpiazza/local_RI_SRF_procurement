# Info --------------------------------------------------------------------
##
##Script name: 03_tables
##
##Purpose of script: Preparing the data for the paper "Where the God Particles touches the ground - The local economic impact of RI procurement " 
##
##Author: Gabriele Piazza
##
##Date Created: 2023-02-20
##
##Copyright (c) Gabriele Piazza, 2023
##Email: g.piazza@lse.ac.uk 
##

##
## Notes:
##   
##

# Load libraries ----------------------------------------------------------
library(here)
library(xtable)
library(stringi)
here()
# Load data ---------------------------------------------------------------

manufacturing_mun_2012<- readRDS(here("results", "output", "out_manufacturing_2012_mun.rds"))
synth_out_man<- readRDS(here("Analysis", "results","output", "synth_out_manufacturing.rds"))
prep_out_synth<- readRDS(here("Analysis","results", "output", "synth_out_manufacturing.rds"))
manufact_spillover<- readRDS(here("Analysis","results", "output", "out_manufacturing_spillover.rds"))
non_tradable_spillover<- readRDS(here("Analysis","results", "output", "out_non_tradable_spillover.rds"))
manufact_bordering <- readRDS(here("Analysis","results", "output", "out_manufacturing_bordering.rds"))
non_tradable_bordering<- readRDS(here("Analysis","results", "output", "out_non_tradable_bordering.rds"))
mid_size_northern_municipalities<- read_csv(here("data_proc", "mid_size_northern_municipalities.csv")) %>% select(-'...1') # for main analysis
# Table 1 -----------------------------------------------------------------
# This is saved in the folder as txt

# Table 2 -----------------------------------------------------------------
weights<-manufacturing_mun_2012$weights.co
weights<-as.matrix(weights)
municipalities<-as.matrix(unique(mid_size_northern_municipalities$municipality))
municipalities<- municipalities[-88]
rownames(weights)<-municipalities

weights <- as.matrix(weights[ order(row.names(weights)), ])
SCHIO <- 0
weights_manufact<-rbind(SCHIO,weights);weights
weights_manufact<- as.data.frame(weights)
top_10_manufact_mun <- slice_max(weights_manufact,order_by = V1, n=10) %>% rename("TBM weights" = V1)
row_10<- rownames(top_10_manufact_mun)
top_10_mun_weights <- cbind(row_10, top_10_manufact_mun)
top_10_mun_weights<- top_10_mun_weights %>% rename(Municipality = row_10)
top_10_mun_weights$Municipality<- str_to_title(top_10_mun_weights$Municipality)



table_2 <- xtable(top_10_mun_weights, caption = "Donor weights - Top 10")
table_2 <- print(table_2, include.rownames = FALSE)

# Save LaTeX table code to a file
write.table(table_2, here("Analysis","results", "tables", "table_2.tex"), sep = "", row.names = FALSE, col.names = FALSE, quote = FALSE)

table_2<- 


# Table 3-----------------------------------------------------------------
synth.tables_manufacturing <- synth.tab(dataprep.res =prep_out_synth, 
                                        synth.res = synth_out_man)#create table with all the results 

balance_table_manufacturing <- synth.tables_manufacturing$tab.pred 


balance_table_manufacturing <- synth.tables_manufacturing$tab.pred 




# Table 4 -----------------------------------------------------------------
spillover_manufacturing <- print(manufact_spillover)
spillover_non_tradable<- print(non_tradable_spillover)
bordering_manufacturing<- print(manufact_bordering)
bordering_non_tradable <- print(non_tradable_bordering)

row.names(spillover_non_tradable)<- "Log Non-tradable Employees"
row.names(spillover_manufacturing)<- "Log Manufacturing Employees"
row.names(bordering_manufacturing)<- "Log Manufacturing Employees"
row.names(bordering_non_tradable)<- "Log Non-tradable Employees"

spillover_results <- rbind.data.frame(spillover_manufacturing, spillover_non_tradable,
                                      bordering_manufacturing, bordering_non_tradable)

#spillover_results <- capture.output(spillover_results)
#writeLines(spillover_results, here("results","tables","table_4.txt"))


table_4 <- xtable(spillover_results, caption = "Displacement effects on neighbouring municipalities")
table_4 <- print(table_4, include.rownames = FALSE)

# Save LaTeX table code to a file
write.table(table_4, here("results", "tables", "table_4.tex"), sep = "", row.names = FALSE, col.names = FALSE, quote = FALSE)
