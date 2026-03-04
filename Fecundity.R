#############################################################
########### Drosophila Food Choice and Ovw 2025-26 experiment #######
#####################  Fecundity ####################

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
fec_choice <- read.csv("Fecundity.csv", header = TRUE)
head(fec_choice)
str(fec_choice)

#change first four vars from character to factors
fec_choice[,c(1:4)] <- lapply(fec_choice[,c(1:4)], as.factor)


#2
## data wrangling
#adjust labels for factors
levels(fec_choice$Choice) <- list(Choice = "C", `No Choice` = "NC")
levels(fec_choice$Food) <- list(`Plant Food` = "PF", `Yeast Food` = "YF")

#make bio rep average for fecundity:
fec_choice <- fec_choice %>%
  group_by(Cage, Choice, Food) %>%
  summarise(eggperfemday = mean(EggsPerFemale)) %>% #made the bio rep
  as.data.frame()


#make SE manually:
se_fec <- fec_choice %>%
  group_by(Choice, Food) %>%
  summarise(mean = mean(eggperfemday),
            sd = sd(eggperfemday),
            se = sd/sqrt(8))
  
#make raw data cage pts:
fec_choice.r <- fec_choice %>%
  group_by(Cage, Choice, Food) %>%
  summarise(mean = mean(eggperfemday)) %>%
  as.data.frame()


#3
## data exploration
ggplot(data = fec_choice, aes(x = Choice, y = eggperfemday, color = Food)) +
  geom_point(stat = "summary", fun = "mean", size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_point(data = fec_choice.r, aes(x = Choice, y = mean, color = Food), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(stat = "summary", fun.data = "mean_se", linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Choice treatment", y = "Eggs/Female/Day", color = "Food treatment")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))



#4
## modeling
#look at response var distribution
hist(fec_choice$eggperfemday, breaks = 15) #seems to be normal, run sims to check

#simulate data from normal distribution with mean and variance taken from response var:
#checks what a normal dist given the data should look like (repeat 10x for accuracy)
Y <- rnorm(31, mean = mean(fec_choice$eggperfemday, na.rm = TRUE), sd = sd(fec_choice$eggperfemday, na.rm = TRUE))
#then plot simulated data:
hist(Y, nclass = 15, main = "Simulated Data", xlab = "Fecundity")

##checks out as normal dist; can use lme

#first model:
fec_mod1 <- lmer(eggperfemday ~ Choice*Food + (1 | Cage), data = fec_choice)



#5
## Model diagnostics




#6
## summary stats and hypothesis testing
summary(fec_mod1)

Anova(fec_mod1, type = "III")

em_fec1 <- emmeans(fec_mod1, pairwise ~ Choice | Food, adust = "fdr")
em_fec1


#7
##Visualization








