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
dryMass[,c(1:5)] <- lapply(dryMass[,c(1:5)], as.factor)

#remove extra cols:
dryMass <- dryMass[,-c(9:11)]

#adjust factor labels:
levels(dryMass$Choice) <- list(Choice = "C", `No Choice` = "NC")
levels(dryMass$Food) <- list(`Plant Food` = "PF", `Yeast Food` = "YF")

#create mass in mg
dryMass$mass.mg <- dryMass$Corrected_drymass_mg*1000
head(dryMass) #check


#create bio rep:
dryMass.b <- dryMass %>%
  group_by(Timepoint, Cage, Choice, Food) %>%
  summarise(avg_dmass = mean(mass.mg)) %>%
  as.data.frame()

#manual se calcs
se_dryMass <- dryMass %>%
  group_by(Timepoint, Choice, Food) %>%
  summarise(mean = mean(mass.mg), sd = sd(mass.mg),
            se = sd/sqrt(8)) %>%
  as.data.frame()

#raw cage data pts:
dryMass.r <- dryMass %>%
  group_by(Timepoint, Cage, Choice, Food) %>%
  summarise(mean = mean(mass.mg)) %>%
  as.data.frame()

#3
## data exploration:
#within overwintering
ggplot(data = dryMass.b, aes(x = Choice, y = avg_dmass, color = Food))+
  geom_point(stat = "summary", fun = "mean", size = 6, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+ 
  geom_point(data = dryMass.r, aes(x = Choice, y = mean, color = Food), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(stat = "summary", fun.data = "mean_se",linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Choice treatment", y = "Dry mass (mg)", color = "Food treatment")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))


#between fall and winter:
ggplot(data = dryMass.b, aes(x = Timepoint, y = avg_dmass, group = 1))+
  geom_point(stat = "summary", fun = "mean", size = 6, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+ 
  geom_line(stat = "summary", fun = "mean", linewidth = 1.5, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+ 
  geom_point(data = dryMass.r, aes(x = Timepoint, y = mean), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(stat = "summary", fun.data = "mean_se",linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  guides(color = "none")+
  scale_color_viridis_d()+
  labs(x = "Timepoint", y = "Dry mass (mg)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))



#4
## modeling
hist(dryMass.b$avg_dmass, breaks = 15)

#simulate data from normal distribution with mean and variance taken from response var:
#checks what a normal dist given the data should look like (repeat 10x for accuracy)
Y <- rnorm(31, mean = mean(dryMass.b$avg_dmass, na.rm = TRUE), sd = sd(dryMass.b$avg_dmass, na.rm = TRUE))
#then plot simulated data:
hist(Y, nclass = 15, main = "Simulated Data", xlab = "Fecundity")

##checks out as normal dist; can use lme

#first model:
dry_mod1 <- lmer(avg_dmass ~ Choice*Food + (1 | Cage), data = dryMass.b)




#5
##Model diagnostics




#6
## summary stats and hypothesis testing
summary(dry_mod1)

Anova(dry_mod1, type = "III")

#to get contrasts, just so raw data vals for estimates
dry_em1 <- emmeans(dry_mod1, pairwise ~ Choice | Food, adjust = "fdr")
dry_em1 <- emmeans(dry_mod1, pairwise ~ Food | Choice, adjust = "fdr")

dry_em1










