#############################################################
########### Drosophila Food Choice and Ovw 2025-26 experiment #######
#####################  Field Survival ####################

##packages and libraries
packages <- c("lubridate", "pwr", "pwrss", "reshape2", "devtools", "stats", "TMB", 
              "MARSS", "datasets", "magrittr", "tidyr", "dplyr", "forecast", 
              "ggplot2", "viridis", "MASS", "AICcmodavg", "glmmTMB", "lme4", "nlme",
              "ggeffects", "emmeans", "DHARMa", "car", "boot", "geepack", "cowplot", 
              "forcats", "visreg", "lubridate", "knitr", "tibble", "survival", "ggsurvfit")
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

#create new long format data frame with 'event' for each fly at each time point
# 0 for dead, 1 for alive
#code first

install.packages("splitstackshape")
library(splitstackshape) #to add ind rows

counts_surv <- read.csv("Field_surv.cox.csv", header = TRUE)
head(counts_surv)

counts_surv[,c(4:7)] <- lapply(counts_surv[,c(4:7)], FUN = as.factor)

#change date to date:
counts_surv$Date <- ymd(counts_surv$Date)

counts_surv <- counts_surv %>%
  pivot_longer(cols = c("Corrected_Dead", "Active_alive"), #this section pivots the alive and dead col to be a single status column and number count col
               names_to = "Status",
               values_to = "Count") 
counts_surv <- counts_surv %>%
  group_by(Week, Ovw_Cage, Choice, Food) %>%
  expandRows("Count") %>%
  mutate(Event = ifelse(Status == "Corrected_Dead", 1, 0))


counts_surv <- counts_surv %>%
  slice(seq_len(n()), counts_surv$Count) %>%
  mutate(Event = ifelse(Status == "Inactive_Dead", 1, 0)) %>%
  select(-Count)
  
  
counts_surv$Event <- 
  uncount(!is.na(Count)) %>%
  mutate(Event = ifelse(Status == "Inactive_Dead", 1, 0)) 
  










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

  












