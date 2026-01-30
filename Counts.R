#############################################################
########### Drosophila Food Choice and Ovw 2025-26 experiment #######
#####################  Field Survival ####################

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
counts <- read.csv("Field_surv.csv", header = TRUE)
head(counts)
str(counts)

##2. Data wrangling

#change fixed vars to factors:
counts[,c(4:7)] <- lapply(counts[,c(4:7)], FUN = as.factor)

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
  subset(Week != 4) %>%
  subset(Week != 3) %>%
  subset(Week != 5) %>%
  group_by(Week, Ovw_Cage, Choice, Food) %>%
  mutate(avg_alive = mean(Active_alive)) %>%
  mutate(prop.surv = Active_alive/1500)

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

#plotting proportion survival weeks 6 - 9
ggplot(data = counts_last3, aes(x = Choice, y = prop.surv, color = Food))+
  geom_point(size = 6, stat = "summary", fun = "mean",position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  geom_errorbar(linewidth = 1.5, width = 0.05, stat = "summary", fun.data = "mean_se",position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Choice", y = "Proportion survival", color = "Food treatment")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))



##4. Modeling
hist(sqrt(counts_last3$prop.surv)) #sqrt trans okay

mod_prop <- glmmTMB(prop.surv ~ Choice*Food + (1|Cage), family = gaussian(link = "identity"),
                    data = counts_last3)


##5. Model diagnostics
test_sim <- simulateResiduals(mod_prop)
plot(test_sim)

##6. Summary stats and Hypothesis testing
summary(mod_prop) #effect of choice

Anova(mod_prop, type = "III") #choice chi-sq = 7.48, P = 0.00624

#no interaction, but interested in spec values: 
em_choice <- emmeans(mod_prop, pairwise ~ Choice | Food, regrid = "response", adjust = "fdr")
em_choice #reiterates above: effect of choice, not food

##7. Data visualization

em_food.grid <- em_choice$emmeans %>%
  confint() %>%
  as.data.frame

#Plot
ggplot(data = em_food.grid, aes(x = Choice, y = emmean, color = Food))+
  geom_point(size = 6, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), linewidth = 1.5, width = 0.1, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0))+
  scale_color_viridis_d()+
  labs(x = "Choice", y = "Proportion survival", color = "Food treatment")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

  



##################################################################
#################################################################
#################################################################
######## SURVIVAL ANALYSES : Cox Prop hazards model and Kaplan-Meier curves
#################################
#########################################################################

#create new long format data frame with 'event' for each fly at each time point
# 0 for dead, 1 for alive
#code first

#install.packages("splitstackshape")
library(splitstackshape) #to add ind rows

#read in data w. corrected dead counts from survival tab
counts_surv <- read.csv("Field_surv.cox.csv", header = TRUE)
head(counts_surv)

#change cols to factors
counts_surv[,c(3:7)] <- lapply(counts_surv[,c(3:7)], FUN = as.factor)

#remove unreliable data from week 4
counts_surv <- counts_surv %>%
  subset(Week != "4")

#change date to date:
counts_surv$Date <- ymd(counts_surv$Date)

##now create the event column for each ind fly
#first, pivot to create a status column with respective alive / dead count
#second, group and create row for each fly
counts_surv <- counts_surv %>%
  pivot_longer(cols = c("Corrected_Dead", "Active_alive"), #this section pivots the alive and dead col to be a single status column and number count col
               names_to = "Status",
               values_to = "Count") 
counts_surv <- counts_surv %>%
  group_by(Week, Ovw_Cage, Choice, Food) %>%
  expandRows("Count") %>% #this expands the dataset to make a row for each fly from the number in the Count column
  mutate(Event = ifelse(Status == "Corrected_Dead", 1, 0)) #this creates the status column of alive / dead (1/0)

#now create time object as number of days
counts_surv$Time <- 7 + (as.numeric(counts_surv$Week)*7)


##survival analysis

#first basic plot w/ survival object:
survfit2(Surv(Time, Event) ~ Choice + Food, data = counts_surv) %>%
  ggsurvfit(linewidth = 1) +
  theme_classic()+
  labs(x = "Day", y = "Survival probability") +
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  add_confidence_interval()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )


#quick estimate of x-day survival:
surv_mod <- survfit(Surv(Time, Event) ~ Choice + Food, data = counts_surv)
summary(surv_mod, times = 63)
#over whole period:
#choice: survival = 0.0214 (2.14%), SE = 0.000534
#no choice: survival = 0.00666 (0.6%), SE = 0.000270


#make table:
survfit(Surv(Time, Event) ~ Choice, data = counts_surv) %>%
  tbl_survfit(
    times = 40,
    label_header = "**63 day survival (95% CI)**"
  )

## comparing survival between groups:
#use log-rank test (equally weights observations over time)
#testing just choice here:
survdiff(Surv(Time, Event) ~ Choice, data = counts_surv)
#Chisq = 3208, 1df, P < 0.0001

## Cox regression model, can test both food and choice
#can fit multi-regression models; assumes that hazards (risk of death) is proportional at each point in time
cox_mod1 <- coxph(Surv(Time, Event) ~ Choice, data = counts_surv)
#intersting, across whole period, food is also signif?
cox_mod1 %>% tbl_regression(exp = TRUE)

#test assumptions of proportional hazards:
cz <- cox.zph(cox_mod1)
cz #significant for choice, food, and interaction as hazards not being proportional
plot(cz) #diagnostic plots verify that

##Dealing with non-proportionality:
#split the time of the dataset in half:
counts_surv.split <- survSplit(Surv(Time, Event) ~ Choice + Food + Ovw_Cage, data = counts_surv,
                               cut = c(49), episode = "tgroup", id = "id")
  
#new model :
cox_mod2 <- coxph(Surv(tstart, Time, Event) ~ Choice, data = counts_surv.split)
cox_mod2

#try new fit to test proportionality:
cox.zph(cox_mod2)

#that did not work


##could now try a time-dependent coefficient
cox_mod3 <- coxph(formula = Surv(Time, Event) ~ Choice, data = counts_surv, 
                  tt = function(x, t, ...) x*log(t + 20))

#try new fit to test proportionality:
cox.zph(cox_mod3)






