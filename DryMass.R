#############################################################
########### Drosophila Food Choice and Ovw 2025-26 experiment #######
#####################  Dry Mass ####################

##packages and libraries
packages <- c("lubridate", "pwr", "pwrss", "reshape2", "devtools", "stats", "TMB", 
              "MARSS", "datasets", "magrittr", "tidyr", "dplyr", "forecast", 
              "ggplot2", "viridis", "MASS", "AICcmodavg", "glmmTMB", "lme4", "nlme",
              "ggeffects", "emmeans", "DHARMa", "car", "boot", "geepack", "cowplot", 
              "forcats", "visreg", "lubridate", "knitr", "tibble", "survival", "ggsurvfit", "gtsummary",
              "broom.helpers")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#load packages:
invisible(lapply(packages, library, character.only = TRUE))


##setwd
setwd('C:/Users/prile/Documents/WSU_PhD/RudmanLab/Projects/FoodChoice/FoodChoice')

##1. read in data, take a look:
dryMass <- read.csv("dryMass.csv", header = TRUE)
head(dryMass)
str(dryMass)

#change chr to factor
dryMass[,c(1:4)] <- lapply(dryMass[,c(1:4)], as.factor)

#adjust factor labels:
levels(dryMass$Choice) <- list(Choice = "C", `No Choice` = "NC")
levels(dryMass$Food) <- list(`Plant Food` = "PF", `Yeast Food` = "YF")

#create mass in mg
dryMass$mass.mg <- dryMass$Corrected_drymass_mg*1000
head(dryMass) #check


#create bio rep:
dryMass.b <- dryMass %>%
  group_by(Cage, Choice, Food) %>%
  summarise(avg_dmass = mean(mass.mg)) %>%
  as.data.frame()

#manual se calcs
se_dryMass <- dryMass %>%
  group_by(Choice, Food) %>%
  summarise(mean = mean(mass.mg), sd = sd(mass.mg),
            se = sd/sqrt(8)) %>%
  as.data.frame()

#raw cage data pts:
dryMass.r <- dryMass %>%
  group_by(Cage, Choice, Food) %>%
  summarise(mean = mean(mass.mg)) %>%
  as.data.frame()

#3
## data exploration:
ggplot(data = dryMass.b, aes(X = Choice, y = avg_dmass, color = Food))+
  stat_summary(fun = "mean", geom = "point")+
  stat_summary(fun.data = "mean_se", geom = "errorbar")+
  
  geom_point(stat = "summary", fun = "mean", size = 6, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+ 
  geom_point(data = dryMass.r, aes(x = Choice, y = mean, color = Food), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))
  geom_errorbar(stat = "summary", fun.data = "mean_se", linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Choice treatment", y = "Dry mass (mg)", color = "Food treatment")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))














