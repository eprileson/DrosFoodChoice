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
setwd('/data/lab/rudman/Eric/Food_choice/')

###
### Read in PHYPER datasets
###


###
### Read in Wilcoxon Rank Sum tests
###
p.vals_total <- read.table("results/rudflies_2025.glm.tpt.wilcox.p.txt", header = TRUE)
head(p.vals_total)
str(p.vals_total)

#remove founder comparisons:
p.vals_total <- p.vals_total[,-c(1:4)]
head(p.vals_total)

#pivot to long: timepoint contrast, pvalue
p.vals_total.long <- p.vals_total %>%
  pivot_longer(cols = c("T1.T2.pval", "T1.T3.pval", "T1.T4.pval", "T2.T3.pval", "T2.T4.pval",
  "T3.T4.pval"), names_to = c("Timepoint_contrast"),
               values_to = "P_value") %>%
  as.data.frame()

head(p.vals_total.long)

#change t-point contrast to discrete var
p.vals_total.long$Timepoint_contrast <- as.factor(p.vals_total.long$Timepoint_contrast)
levels(p.vals_total.long$Timepoint_contrast)

#change pvalue to -log10;
p.vals_total.long$P_value <- (-1*log10(p.vals_total.long$P_value))
head(p.vals_total.long$P_value)

#basic plot:
ggplot(data = p.vals_total.long, aes(x = Timepoint_contrast, y = P_value, color = Timepoint_contrast))+
  geom_violin()+
  #scale_y_log10()+
  scale_color_viridis_d()+
  guides(color = "none")+
  labs(x = "Timepoint contrast", y = "P_value (- log 10)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))


## run statistical tests:









