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


#Install and load lme4u package for lme4 model diagnostics
devtools::install_github("hollyyfc/lme4u")
library(lme4u)


##setwd
setwd('C:/Users/prile/Documents/WSU_PhD/RudmanLab/Projects/FoodChoice/FoodChoice')

##1. read in data, take a look:
fec_choice <- read.csv("Fecundity.csv", header = TRUE)
head(fec_choice)
str(fec_choice)

#change first four vars from character to factors
fec_choice[,c(1:5)] <- lapply(fec_choice[,c(1:5)], as.factor)


#2
## data wrangling
#adjust labels for factors
levels(fec_choice$Choice) <- list(Choice = "C", `No Choice` = "NC")
levels(fec_choice$Food) <- list(`Plant Food` = "PF", `Yeast Food` = "YF")

#make bio rep average for fecundity:
fec_choice.c <- fec_choice %>%
  subset(Time != "Fall") %>%
  group_by(Cage, Choice, Food) %>%
  summarise(eggperfemday = mean(EggsPerFemale)) %>% #made the bio rep
  as.data.frame()


#make bio rep average for time comparison
fec_choice.t <- fec_choice %>%
  group_by(Time, Cage, Choice, Food) %>%
  summarise(eggperfemday = mean(EggsPerFemale)) %>% #made the bio rep
  as.data.frame()


#make SE manually:
se_fec <- fec_choice.c %>%
  group_by(Choice, Food) %>%
  summarise(mean = mean(eggperfemday),
            sd = sd(eggperfemday),
            se = sd/sqrt(8))
  
#make raw data cage pts:
fec_choice.r <- fec_choice.c %>%
  group_by(Cage, Choice, Food) %>%
  summarise(mean = mean(eggperfemday)) %>%
  as.data.frame()

#comparison of just time
fec_choice.t.r <- fec_choice.t %>%
  group_by(Time, Cage) %>%
  summarise(mean = mean(eggperfemday)) %>%
  as.data.frame()


#3
## data exploration
#by choice and food within overwinter
fec_ovw <-
ggplot(data = fec_choice.c, aes(x = Choice, y = eggperfemday, color = Food)) +
  geom_point(stat = "summary", fun = "mean", size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_point(data = fec_choice.r, aes(x = Choice, y = mean, color = Food), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(stat = "summary", fun.data = "mean_se", linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Choice treatment", y = "Eggs/Female/Day", color = "Food treatment")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

ggsave("fecundity_ovw.jpg", plot = fec_ovw, device = "jpeg", width = 7.7, 
       height = 5.14, units = "in", dpi = 500, bg = "white")


#across time, fall and post-winter
fec_time <- 
ggplot(data = fec_choice.t, aes(x = Time, y = eggperfemday, group = 1, color = Time)) +
  geom_point(stat = "summary", fun = "mean", size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_line(stat = "summary", fun = "mean", linewidth = 1.5, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_point(data = fec_choice.t.r, aes(x = Time, y = mean, color = Time), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(stat = "summary", fun.data = "mean_se", linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_manual(values = c("#440154", "#440154"))+
  guides(color = "none")+
  labs(x = "Timepoint", y = "Eggs/Female/Day")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

ggsave("fecundity_time.jpg", plot = fec_time, device = "jpeg", width = 7.7, 
       height = 5.14, units = "in", dpi = 500, bg = "white")


#4
## modeling
#look at response var distribution
hist(fec_choice$eggperfemday, breaks = 15) #seems to be normal, run sims to check
hist(fec_choice.t$eggperfemday, breaks = 15)

#simulate data from normal distribution with mean and variance taken from response var:
#checks what a normal dist given the data should look like (repeat 10x for accuracy)
Y <- rnorm(31, mean = mean(fec_choice$eggperfemday, na.rm = TRUE), sd = sd(fec_choice$eggperfemday, na.rm = TRUE))
#then plot simulated data:
hist(Y, nclass = 15, main = "Simulated Data", xlab = "Fecundity")

##checks out as normal dist; can use lme

#first model:
fec_mod1 <- lmer(eggperfemday ~ Choice*Food + (1 | Cage), data = fec_choice.c)

fec_time.mod1 <- lmer(eggperfemday ~ Time + (1 | Cage), data = fec_choice.t)


#5
## Model diagnostics
#diagnostics with lme4U: (diagnostic tools for lme4 mods):
#explain model:
explain_lmer(fec_mod1, details = "general") #brief description of the model

#qqplot to check residual normality
res_norm(fec_mod1)

#residual homoscedasticity
res_fit(fec_mod1)

#checking for heteroscedastity by group:
res_box(fec_mod1, group_var = "Cage")

#diagnostifs with DhARMA:
sim_resid <- simulateResiduals(fec_mod1, plot = F)
plot(sim_resid)



#6
## summary stats and hypothesis testing
summary(fec_mod1) #est food = 5.48, SE = 2.29; choice = 5.83, SE = 2.29; int est = -2.69, SE = 3.30 

summary(fec_time.mod1)

Anova(fec_mod1, type = "III") #choice = 6.50; P = 0.0108; food = 5.73; P = 0.0167; interaction = P =0.415
Anova(fec_time.mod1, type = "III") #chisq = 4.90, P = 0.0268

em_fec1 <- emmeans(fec_mod1, pairwise ~ Choice | Food, adust = "fdr")
em_fec1
em_fect <- emmeans(fec_time.mod1, pairwise ~ Time, adust = "fdr")


#create df from emmeans:
em_fec.df <- em_fec1$emmeans %>%
  confint() %>%
  as.data.frame()

em_fec.t.df <- em_fect$emmeans %>%
  confint() %>%
  as.data.frame()

#7
##Visualization
#food choice
ggplot(data = em_fec.df, aes(x = Choice, y = emmean, color = Food, group = 1))+
  geom_point(size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.2, seed = 0)) +
  geom_point(data = fec_choice.r, aes(x = Choice, y = mean, color = Food), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.2, seed = 0))+
  scale_color_viridis_d()+
  labs(x = "Choice treatment", y = "Eggs/Female/Day")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

#time plot
ggplot(data = em_fec.t.df, aes(x = Time, y = emmean, color = Time, group = 1))+
  geom_point(size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_line(linewidth = 1.5, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_point(data = fec_choice.t.r, aes(x = Time, y = mean, color = Time), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_manual(values = c("#440154", "#440154"))+
  guides(color = "none")+
  labs(x = "Timepoint", y = "Eggs/Female/Day")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

  







