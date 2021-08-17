#LOAD DATA=====
setwd("~/00DeakinUni/R/Fraser") #set working directory to data file
library(tidyverse)
library(MASS)
library(pscl)
library(here)

Euc_data <- read.csv("Euc_data.csv")#Load DATA

#Sum grass cover (New Variable = "gc" for grass cover) and filter out NA-s:
euc <-  Euc_data %>%
  mutate(gc = ExoticAnnualGrass_cover+ExoticPerennialGrass_cover+NativePerennialGrass_cover) %>%  #gc = grass cover, #Join covers of all (3) types of grasses:
  filter(is.na(gc)==F) #filter out NA-s 


#Run and Compare the Best Models for each of 3 seasons (nb1,nb2,nb3 and zi1, zi2, zi3)======

#Compare NB and zeroinfl model for Winter 2006:
summary(nb1 <- glm.nb(euc_sdlgs0_50cm ~ gc, data = subset(euc, Season=="Winter 2006")))#https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
summary(zi1 <- zeroinfl(euc_sdlgs0_50cm ~ gc,data = subset(euc, Season=="Winter 2006")))#https://stats.idre.ucla.edu/r/dae/zip/#:~:text=Remote%20Consulting-,Zero%2DInflated%20Poisson%20Regression%20%7C%20R%20Data%20Analysis%20Examples,zeros%20can%20be%20modeled%20independently.
AIC(nb1,zi1)#NB is a better model.

#Compare NB and zeroinfl model, for "Spring 2006":
summary(nb2 <- glm.nb(euc_sdlgs0_50cm ~ gc, data = subset(euc, Season=="Spring 2006")))#https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
summary(zi2 <- zeroinfl(euc_sdlgs0_50cm ~ gc,data = subset(euc, Season=="Spring 2006")))#https://stats.idre.ucla.edu/r/dae/zip/#:~:text=Remote%20Consulting-,Zero%2DInflated%20Poisson%20Regression%20%7C%20R%20Data%20Analysis%20Examples,zeros%20can%20be%20modeled%20independently.
AIC(nb1,zi2)#NB is a better model.

#Compare NB and zeroinfl model for "Autumn 2007":
summary(nb3 <- glm.nb(euc_sdlgs0_50cm ~ gc, data = subset(euc, Season=="Autumn 2007")))#https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
summary(zi3 <- zeroinfl(euc_sdlgs0_50cm ~ gc,data = subset(euc, Season=="Autumn 2007")))#https://stats.idre.ucla.edu/r/dae/zip/#:~:text=Remote%20Consulting-,Zero%2DInflated%20Poisson%20Regression%20%7C%20R%20Data%20Analysis%20Examples,zeros%20can%20be%20modeled%20independently.
AIC(nb3,zi3)#NB is a better model.

#Plot nb1======
#Crate NewData = recruitment estimates from nb1 (winter with 95% CI):
NewData1 <- data.frame(gc = rep(0:100, 3)) #for grass cover from 0 to 100%
NewData1 <- cbind(NewData1, predict(nb1, NewData1, type = "link", se.fit=TRUE))

NewData1 <- within(NewData1, {
  Euc_density <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

write.csv(NewData1, file = "1PawelWaryszak-prediction.csv", row.names = F)#First set of predictions for 0-100 % Grass Cover

#Plot NB model estimates with  NewData:
ggplot(NewData1, aes(gc,Euc_density)) + 
  geom_ribbon (aes(ymin = LL, ymax = UL), alpha = .25) +
  geom_line()+ labs(x = "Grass Cover (%)", y = "Predicted Euc Density") +
  ggtitle("Winter 2006")

# For Many Analysts predictions (for Tim)======
#Plot NB model (nb3) with new data from Many Analysts:
new_euc_data <- readr::read_csv(here::here("data/euc_specification_data_wide.csv")) %>% 
  dplyr::mutate(SurveyID = as.factor(SurveyID),
                Date = as.Date(Date, "%d/%m/%Y"),
                `Quadrat no` = as.factor(`Quadrat no`)) %>%
  mutate(gc = ExoticAnnualGrass_cover+ExoticPerennialGrass_cover+NativePerennialGrass_cover) %>%  #gc = grass cover, #Join covers of all (3) types of grasses:
  filter(is.na(gc)==F) #filter out NA-s 


dplyr::glimpse(new_euc_data)

# Are all column names in euc_data in new_euc data?
all(names(euc_data) %in% names(new_euc_data)) #[1] TRUE

predict(nb3, newdata = new_euc_data) #This predicts negative values that need to be exponentiated:

NewDataTim <- cbind(new_euc_data, predict(nb1, new_euc_data, type = "link", se.fit=TRUE))

new_euc_data_predicted <- within(NewDataTim, {
  Fit_Euc_density <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

#Export for Tim these variables = SurveyID, fit, lwr, upr, std.err
new_euc_data_predicted_export<-new_euc_data_predicted[ ,c("SurveyID", "Fit_Euc_density", "LL","UL", "se.fit")]
new_euc_data_predicted_export
write.csv(new_euc_data_predicted_export, file = "Estimates4Time.csv", row.names = F)