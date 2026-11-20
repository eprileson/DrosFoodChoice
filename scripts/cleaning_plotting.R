#########################################################################################
############# Candidate Gene / Food Choice / Cleaning and Plotting Script ############################
################### Testing between timepoints w/ 2025 Allele frequencies of candidate genes ##########
########################################################################################################################

### In R ###
##packages and libraries
packages <- c("lubridate", "pwr", "pwrss", "reshape2", "devtools", "stats", "TMB", 
              "MARSS", "datasets", "magrittr", "tidyr", "dplyr", "forecast", 
              "ggplot2", "viridis", "MASS", "AICcmodavg", "glmmTMB", "lme4", "nlme",
              "ggeffects", "emmeans", "DHARMa", "car", "boot", "geepack", "cowplot", "forcats", "visreg",
              "outliers", "EnvStats", "data.table", "matrixStats","Rcpp", "stringi", "foreach",
              "ggpubr", "stats", "ggfortify", "zoo", "rstatix", 'slider', "RColorBrewer")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
#load packages:
invisible(lapply(packages, library, character.only = TRUE))

##setwd 
setwd('/data/lab/rudman/Eric/Food_choice')

###
### Read in PHYPER datasets
###


###
### Read in Wilcoxon Rank Sum tests
###











