#############################################################
########### Drosophila Food Choice and Ovw 2025-26 experiment #######
#####################  Field Survival ####################

##packages and libraries
packages <- c("lubridate", "pwr", "pwrss", "reshape2", "devtools", "stats", "TMB", 
              "MARSS", "datasets", "magrittr", "tidyr", "dplyr", "forecast", 
              "ggplot2", "viridis", "MASS", "AICcmodavg", "glmmTMB", "lme4", "nlme",
              "ggeffects", "emmeans", "DHARMa", "car", "boot", "geepack", "cowplot", 
              "forcats", "visreg", "lubridate")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#load packages:
invisible(lapply(packages, library, character.only = TRUE))


##setwd
setwd('C:/Users/prile/Documents/WSU_PhD/RudmanLab/Projects/FoodChoice/FoodChoice')

##1. read in data, take a look:
counts <- read.csv("Field_surv.csv", header = TRUE)
head(counts)
str(counts)

##2. Data wrangling

#change fixed vars to factors:
counts[,c(2:5)] <- lapply(counts[,c(2:5)], FUN = as.factor)

#change date to date:
counts$Date <- ymd(counts$Date)
class(counts$Date)


##Group by only the last 3 weeks:
counts_last3 <- counts %>%
  subset(Week != 2) %>%
  subset(Week != 3) %>%
  subset(Week != 4) %>%
  subset(Week != 5) %>%
  group_by(Week, Ovw_Cage, Choice, Food) %>%
  mutate(avg_alive = mean(Active_alive))

##3. data viz:
ggplot(data = counts_last3, aes(x = Week, y = avg_alive, color = Food, shape = Choice))+
  geom_point(stat = "summary", fun = "mean", size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Week", y = "Average count", color = "Food treatment", shape = "Choice")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))









