#LOAD DATA=====
setwd("~/00DeakinUni/R/Fraser") #set working directory to data file
library(tidyverse)
Euc_data <- read.csv("~/00DeakinUni/R/Fraser/Euc_data.csv")#Load DATA
unique(Euc_data$Season)# Autumn 2007 Spring 2006 Winter 2006, three seasons

#number of replicates among properties (sites):=====
properties <- Euc_data %>%
  group_by(Season,Property) %>%
  summarise(total = length(Property))%>%
  spread(Season,total) %>%
  arrange(desc(`Winter 2006`))

properties

#Check number of zeros obs:==========
hist(Euc_data$euc_sdlgs0_50cm)#Frequent zeros, overdispersion very likely
sum(Euc_data$euc_sdlgs0_50cm %in% 0 ) / nrow (Euc_data) *100 #88% are zeros.

zeros <- Euc_data %>%
  group_by(Season) %>%
  summarise(zeros = sum (euc_sdlgs0_50cm==0),
            total = length(euc_sdlgs0_50cm),
            percent_zeros = zeros/total*100)

zeros
####Season      zeros total percent_zeros
#1 Autumn 2007   105   117          89.7
#2 Spring 2006   103   117          88.0
#3 Winter 2006   102   117          87.2


#Sum grass covers and produce their ranks:=====
euc <-  Euc_data %>%
  mutate(gc = ExoticAnnualGrass_cover+ExoticPerennialGrass_cover+NativePerennialGrass_cover) %>%  #gc = grass cover, joining three types of grasses together
  filter(is.na(gc)==F) %>% #filter out NA-s 
  mutate(gc_rank = ntile(gc,10)) #rank grass covers

summary(euc$gc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   10.40   26.70   30.54   47.60   99.00       1 

#Proof of overdispersion = SD greater than mean:
with(euc, tapply(euc_sdlgs0_50cm, gc_rank, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))


#Plot gc_rank =======
#10 even ranks of grass cover (0-100%) against euc recruitment densites across three seasons:
ggplot(euc, aes(Season,euc_sdlgs0_50cm)) +
  geom_boxplot(outlier.shape = NA)+geom_jitter(aes(color=Season), alpha=.7)+
  facet_grid(.~gc_rank)+
  theme(axis.text.x=element_text(vjust=0.5,size=8, angle=90),
        axis.title.y=element_text(size=12),
        axis.title.x=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

