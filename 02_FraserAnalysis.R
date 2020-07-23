#LOAD DATA=====
setwd("~/00DeakinUni/R/Fraser") #set working directory to data file
library(tidyverse)
library(MASS)
library(pscl)

Euc_data <- read.csv("~/00DeakinUni/R/Fraser/Euc_data.csv")#Load DATA

#Sum grass covers and filter out NA-s:
euc <-  Euc_data %>%
  mutate(gc = ExoticAnnualGrass_cover+ExoticPerennialGrass_cover+NativePerennialGrass_cover) %>%  #gc = grass cover, #Join covers of all (3) types of grasses:
  filter(is.na(gc)==F) #filter out NA-s 


#Run 3 Models (nb1,nb2,nb3)======
#Compare NB and zeroinfl model, Winter 2006:
summary(nb1 <- glm.nb(euc_sdlgs0_50cm ~ gc, data = subset(euc, Season=="Winter 2006")))#https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
summary(zi1 <- zeroinfl(euc_sdlgs0_50cm ~ gc,data = subset(euc, Season=="Winter 2006")))#https://stats.idre.ucla.edu/r/dae/zip/#:~:text=Remote%20Consulting-,Zero%2DInflated%20Poisson%20Regression%20%7C%20R%20Data%20Analysis%20Examples,zeros%20can%20be%20modeled%20independently.
AIC(nb1,zi1)#NB is a better model.

#Run Model for "Spring 2006":
summary(nb2 <- glm.nb(euc_sdlgs0_50cm ~ gc, data = subset(euc, Season=="Spring 2006")))#https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
summary(zi2 <- zeroinfl(euc_sdlgs0_50cm ~ gc,data = subset(euc, Season=="Spring 2006")))#https://stats.idre.ucla.edu/r/dae/zip/#:~:text=Remote%20Consulting-,Zero%2DInflated%20Poisson%20Regression%20%7C%20R%20Data%20Analysis%20Examples,zeros%20can%20be%20modeled%20independently.
AIC(nb1,zi2)#NB is a better model.

#Run Model for "Autumn 2007":
summary(nb3 <- glm.nb(euc_sdlgs0_50cm ~ gc, data = subset(euc, Season=="Autumn 2007")))#https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
summary(zi3 <- zeroinfl(euc_sdlgs0_50cm ~ gc,data = subset(euc, Season=="Autumn 2007")))#https://stats.idre.ucla.edu/r/dae/zip/#:~:text=Remote%20Consulting-,Zero%2DInflated%20Poisson%20Regression%20%7C%20R%20Data%20Analysis%20Examples,zeros%20can%20be%20modeled%20independently.
AIC(nb3,zi3)#NB is a better model.

#Plot nb1======
#Crate NewData = recruitment estimates from nb1 (winter with 95% CI):
NewData <- data.frame(gc = rep(0:100, 3))
NewData <- cbind(NewData, predict(nb1, NewData, type = "link", se.fit=TRUE))

NewData <- within(NewData, {
  Euc_density <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

#Plot NewData:
ggplot(NewData, aes(gc,Euc_density)) + 
  geom_ribbon (aes(ymin = LL, ymax = UL), alpha = .25) +
  geom_line()+ labs(x = "Grass Cover (%)", y = "Predicted Euc Density") +
  ggtitle("Winter 2006")




