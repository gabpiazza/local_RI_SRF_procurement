# Run script for example project

# PACKAGES ------------------------------------------------------------------
library(here)

# PRELIMINARIES -------------------------------------------------------------
# Control which scripts run
run_01_dataprep <- 1
run_02_reg      <- 1
run_03_table    <- 1
run_04_graph    <- 1

# RUN SCRIPTS ---------------------------------------------------------------

# Read and clean example data
if (run_01_ex_dataprep) source(here("scripts", "01_dataprep.R"), encoding = "UTF-8")
# INPUTS
#  here("data", "example.csv") # raw data from XYZ source
# OUTPUTS
#  here("proc", "example.rds") # cleaned 

# Regress Y on X in example data
if (run_02_ex_reg) source(here("scripts", "02_reg.R"), encoding = "UTF-8")
# INPUTS
#  here("proc", "example.rds") # 01_ex_dataprep.R
# OUTPUTS 
#  here("proc", "ex_fixest.rds") # fixest object from feols regression

# Create table of regression results
if (run_03_ex_table) source(here("scripts", "03_table.R"), encoding = "UTF-8")
# INPUTS 
#  here("proc", "ex_fixest.rds") # 02_ex_reg.R
# OUTPUTS
#  here("results", "tables", "ex_fixest_table.tex") # tex of table for paper

# Create scatterplot of Y and X with local polynomial fit
if (run_04_ex_graph) source(here("scripts", "04_graph.R"), encoding = "UTF-8")
# INPUTS
#  here("proc", "example.rds") # 01_ex_dataprep.R
# OUTPUTS
#  here("results", "figures", "ex_scatter.eps") # figure