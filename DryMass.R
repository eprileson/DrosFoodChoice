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

#Install and load lme4u package for lme4 model diagnostics
devtools::install_github("hollyyfc/lme4u")
library(lme4u)

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
  subset(Timepoint != "Fall") %>%
  group_by(Cage, Choice, Food) %>%
  summarise(avg_dmass = mean(mass.mg)) %>%
  as.data.frame()

#rep with time
dryMass.t <- dryMass %>%
  group_by(Timepoint, Cage, Choice, Food) %>%
  summarise(avg_dmass = mean(mass.mg)) %>%
  as.data.frame()


#manual se calcs
se_dryMass <- dryMass.b %>%
  group_by(Choice, Food) %>%
  summarise(mean = mean(avg_dmass), sd = sd(avg_dmass),
            se = sd/sqrt(8)) %>%
  as.data.frame()

#raw cage data pts:
dryMass.r <- dryMass.b %>%
  group_by(Cage, Choice, Food) %>%
  summarise(mean = mean(avg_dmass)) %>%
  as.data.frame()

dryMass.t.r <- dryMass %>%
  group_by(Timepoint, Cage) %>%
  summarise(mean = mean(mass.mg)) %>%
  as.data.frame()


#3
## data exploration:
#within overwintering
dryMass_ovw <-
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

ggsave("dryMass_ovw.jpg", plot = dryMass_ovw, device = "jpeg", width = 7.7, 
       height = 5.14, units = "in", dpi = 500, bg = "white")


#between fall and winter:
dry_time <-
ggplot(data = dryMass.t, aes(x = Timepoint, y = avg_dmass, group = 1, color = Timepoint))+
  geom_point(stat = "summary", fun = "mean", size = 6, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+ 
  geom_line(stat = "summary", fun = "mean", linewidth = 1.5, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+ 
  geom_point(data = dryMass.t.r, aes(x = Timepoint, y = mean, color = Timepoint), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(stat = "summary", fun.data = "mean_se",linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  guides(color = "none")+
  scale_color_manual(values = c("#440154", "#440154"))+
  labs(x = "Timepoint", y = "Dry mass (mg)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

ggsave("dry_mass.time.jpg", plot = dry_time, device = "jpeg", width = 7.7, 
       height = 5.14, units = "in", dpi = 500, bg = "white")


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

dry_modtime <- lmer(avg_dmass ~ Timepoint + (1 | Cage), data = dryMass.t)

#5
##Model diagnostics

#diagnostics with lme4U: (diagnostic tools for lme4 mods):
#explain model:
explain_lmer(dry_mod1, details = "general") #brief description of the model

#qqplot to check residual normality
res_norm(dry_mod1)

#residual homoscedasticity
res_fit(dry_mod1)

#checking for heteroscedastity by group:
res_box(dry_mod1, group_var = "Cage")

#diagnostifs with DhARMA:
sim_resid <- simulateResiduals(dry_mod1, plot = F)
plot(sim_resid)



#6
## summary stats and hypothesis testing
summary(dry_mod1) #estimate choice = 0.174, SE = 0.0658; estimate food = 0.149, SE = 0.06581
summary(dry_modtime) #estimate time = -0.279, SE = 0.0643

Anova(dry_mod1, type = "III") #chisq choice = 6.99, P = 0.00820; chisq food = 5.14, P = 0.0234; chis1 int = 1.70, P = 0.192
Anova(dry_modtime, type = "III") #chisq = 18.8, P < 0.0001

drop1(dry_mod1)

#to get contrasts, just so raw data vals for estimates
dry_em1 <- emmeans(dry_mod1, pairwise ~ Choice | Food, adjust = "fdr")
dry_em1 <- emmeans(dry_mod1, pairwise ~ Food | Choice, adjust = "fdr")

dry_em1

dry_em.time <- emmeans(dry_modtime, pairwise ~ Timepoint)

#make dfs for plots:
dry_em.dm <- dry_em1$emmeans %>%
  confint() %>%
  as.data.frame()

dry_em.time.dm <- dry_em.time$emmeans %>%
  confint() %>%
  as.data.frame()


##Visualization
#food choice
ggplot(data = dry_em.dm, aes(x = Choice, y = emmean, color = Food, group = 1))+
  geom_point(size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.2, seed = 0)) +
  geom_point(data = dryMass.r, aes(x = Choice, y = mean, color = Food), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.2, seed = 0))+
  scale_color_viridis_d()+
  labs(x = "Choice treatment", y = "Dry mass (mg)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

#time plot
ggplot(data = dry_em.time.dm, aes(x = Time, y = emmean, color = Time, group = 1))+
  geom_point(size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_line(linewidth = 1.5, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_point(data = dryMass.t.r, aes(x = Time, y = mean, color = Time), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_manual(values = c("#440154", "#440154"))+
  guides(color = "none")+
  labs(x = "Timepoint", y = "Dry mass (mg)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))







