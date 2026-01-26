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

#remove week 4 (bad counts):
counts <- counts %>%
  subset(Week != 4)


##Group by only the first 5 or last 3 weeks (not including wk 4)
counts_first5<- counts %>%
  subset(Week != 6) %>%
  subset(Week != 7) %>%
  subset(Week != 8) %>%
  subset(Week != 9) %>%
  group_by(Week, Ovw_Cage, Choice, Food) %>%
  mutate(avg_alive = mean(Active_alive))

counts_last3 <- counts %>%
  subset(Week != 2) %>%
  subset(Week != 3) %>%
  subset(Week != 5) %>%
  group_by(Week, Ovw_Cage, Choice, Food) %>%
  mutate(avg_alive = mean(Active_alive))

##3. data viz:
#raw data alive
ggplot(data = counts, aes(x = Week, y = Active_alive.corrected, color = Food, shape = Choice))+
  geom_point(stat = "summary", fun = "mean", size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Week", y = "Average count", color = "Food treatment", shape = "Choice")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

#raw data dead weeks 2 - 5
ggplot(data = counts_first5, aes(x = Week, y = Inactive_Dead, color = Food, shape = Choice))+
  geom_point(stat = "summary", fun = "mean", size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Week", y = "Average count", color = "Food treatment", shape = "Choice")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))


#plot just week 6 - 9 alive
ggplot(data = counts_last3, aes(x = Week, y = avg_alive, color = Food, shape = Choice))+
  geom_point(stat = "summary", fun = "mean", size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Week", y = "Average count", color = "Food treatment", shape = "Choice")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))



##4. Modeling



##5. Model diagnostics



##6. Summary stats and Hypothesis testing



##7. Data visualization












