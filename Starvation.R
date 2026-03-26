#############################################################
########### Drosophila Food Choice and Ovw 2025-26 experiment #######
#####################  Starvation ####################

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
starv <- read.csv("Starvation.csv", header = TRUE)
head(starv)
str(starv)

#2.
## data wrangling: 
#change first four vars from character to factors
starv[,c(1:5)] <- lapply(starv[,c(1:5)], as.factor)

#adjust labels for factors
levels(starv$Choice) <- list(Choice = "C", `No Choice` = "NC")
levels(starv$Food) <- list(`Plant Food` = "PF", `Yeast Food` = "YF")

#change col names to hour lengths:
colnames(starv) <- c("Time", "Cage", "Choice", "Food", "Rep", "Init", "12", "24",    
                     "36", "48", "60", "72" , "84", "96")
#check
head(starv)

#pivot from wide to long for time
starv <- starv %>%
  pivot_longer(cols = c("12", "24", "36", "48", "60", "72" , "84", "96"),
               values_to = "Count", names_to = "Hour") %>%
  as.data.frame(starv)

#change hr to num
starv$Hour <- as.numeric(starv$Hour)

#group by cage to get bio replicates at the minimum or earliest time of death
#first filter out extra 0s from individuals that died early to get accurate means
starv.filtered <- starv %>%
  filter(Count == 10) %>% #filters to when all inds / rep died --> removes 4 reps
  group_by(Cage, Choice, Food, Rep) %>% #group to to individual observation / vial
  filter(Hour == min(Hour)) %>% #calc the ind fly rep death hour
  group_by(Cage, Choice, Food) %>% #group by bio replicate
  mutate(Time_deathAvg = mean(Hour)) %>% #get avg fly death time / bio rep
  as.data.frame(starv)


#group by bio rep:
starv.filtered.fc <- starv.filtered %>%
  subset(Time != "Fall") %>%
  group_by(Cage, Choice, Food) %>%
  summarise(avg_hour = mean(Time_deathAvg)) %>%
  as.data.frame()

#group for time comp
starv.filtered.t <- starv.filtered %>%
  group_by(Time, Cage, Choice, Food) %>%
  summarise(avg_hour = mean(Time_deathAvg)) %>%
  as.data.frame() 

#manual SE calcs:
se_calc.stv <- starv.filtered.fc %>%
  group_by(Choice, Food) %>%
  summarise(mean = mean(avg_hour),
            sd = sd(avg_hour),
            se = sd/sqrt(8))

#raw data pts:
starv.fc.r <- starv.filtered.fc %>%
  group_by(Cage, Choice, Food) %>%
  summarise(mean = mean(avg_hour)) %>%
  as.data.frame()

#raw data for timepoint
starv.t.r <- starv.filtered %>%
  group_by(Time, Cage) %>%
  summarise(mean = mean(Time_deathAvg)) %>%
  as.data.frame()

  
### 3 data exploration
#across food and choice treatments within winter
stv_ovw <-
ggplot(data = starv.filtered.fc, aes(x = Choice, y = avg_hour, color = Food))+
  geom_point(stat = "summary", fun = "mean", size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_point(data = starv.r, aes(x = Choice, y = mean, color = Food), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(stat = "summary", fun.data = "mean_se", linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Choice", y = "Average time of death (hrs)", color = "Food treatment")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

ggsave("starvation_ovw.jpg", plot = stv_ovw, device = "jpeg", width = 7.7, 
       height = 5.14, units = "in", dpi = 500, bg = "white")


#across timepoint - fall to winter
stv_time <-
ggplot(data = starv.filtered.t, aes(x = Time, y = avg_hour, group = 1, color = Time))+
  geom_point(stat = "summary", fun = "mean", size = 6,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_line(stat = "summary", fun = "mean", linewidth = 1.5, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0)) +
  geom_point(data = starv.t.r, aes(x = Time, y = mean), inherit.aes = FALSE, size = 3, alpha = 0.25, stroke = 1, position = position_jitterdodge(dodge.width = 0.2, jitter.width = 0.5))+
  geom_errorbar(stat = "summary", fun.data = "mean_se", linewidth = 1.5, width = 0.1,position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  guides(color = "none") +
  scale_color_manual(values = c("#440154", "#440154"))+
  labs(x = "Timepoint", y = "Average time of death (hrs)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

ggsave("starvation_time.jpg", plot = stv_time, device = "jpeg", width = 7.7, 
       height = 5.14, units = "in", dpi = 500, bg = "white")


#4 
## Modeling

##
#look at response var distribution
hist(starv.filtered.fc$avg_hour, breaks = 15) #seems to be normal, run sims to check
hist(starv.filtered.t$avg_hour, breaks = 15) #seems to be normal, run sims to check

#simulate data from normal distribution with mean and variance taken from response var:
#checks what a normal dist given the data should look like (repeat 10x for accuracy)
Y <- rnorm(31, mean = mean(starv.filtered$avg_hour, na.rm = TRUE), sd = sd(starv.filtered$avg_hour, na.rm = TRUE))
#then plot simulated data:
hist(Y, nclass = 15, main = "Simulated Data", xlab = "Starvation tolerance")

##checks out as normal dist; can use lme
#first model: w/ cage as random int, interaction of food*choice
stv_mod1 <- lmer(avg_hour ~ Choice*Food + (1 | Cage), data = starv.filtered.fc) 

#time model:
stv_time.mod1 <- lmer(avg_hour ~ Time + (1 | Cage), data = starv.filtered.t)


#5
##Model diagnostics:
#explain model:
explain_lmer(stv_mod1, details = "general") #brief description of the model

#qqplot to check residual normality
res_norm(stv_mod1)

#residual homoscedasticity
res_fit(stv_mod1)

#checking for heteroscedastity by group:
res_box(fec_mod1, group_var = "Cage")

#diagnostifs with DhARMA:
sim_resid <- simulateResiduals(fec_mod1, plot = F)
plot(sim_resid)



#6
#summary stats and hypothesis testing
summary(stv_mod1)
summary(stv_time.mod1) #estimate = 1.66, SE = 3.47

Anova(stv_mod1, type = "III") #chisq choice = 4.07, P = 0.0437; food: 1.62, P = 0.203; int = 0.009, p = 0.925
Anova(stv_time.mod1, type = "III") #chisq = 0.230, P = 0.632

em_stv1 <- emmeans(stv_mod1, pairwise ~ Choice | Food, adjust = "fdr")
em_stv1


















