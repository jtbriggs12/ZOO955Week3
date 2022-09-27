#Title: Stat 955 Homework 3
#Authors: Jess Briggs & Amanda Kerkhove
#Date Created:2022-09-27

#Background
#Black sea bass are a protogynous hermaphrodite marine fish species – they change sex from female to male, although there are also primary males which first mature as males.  They do not change from male to female and they change sex primarily after the end of the spawning season (May to July).  The file BSB_tagging_data.xlsx contains the results of a tagging study conducted on this species.  Individual black sea bass (BSB) were captured, tagged with a unique identifier, and released on the “Date_at_capture.”  Some of these individuals were recaptured on the “Date_at_recapture.”  Sex and length were recorded at both time points (though this file only includes the “Length_at_capture.”  Sex change isn’t instantaneous, so some individuals recaptured while undergoing sex change are recorded as intersex.  These individuals can also be thought of as having changed sex.


#Question 1.1: Plot a probability density function (probability density vs. proportions from 0 to 1) for the proportion of female black sea bass that changed sex out of all those which were recaptured after the end of the spawning season (i.e., after July). 
library(lubridate)
library(dplyr)
data<-read.csv("BSB_tagging_data.csv")%>%
  mutate(Date_at_recapture = mdy(Date_at_recapture))%>%
  filter(month(Date_at_recapture) != 5)%>%
  filter(month(Date_at_recapture) !=6)%>%
  filter(month(Date_at_recapture) != 4)%>%
  mutate(Sex_at_recapture = as.factor(Sex_at_recapture))

recapFemale<-data%>%
  group_by(Sex_at_recapture)%>%
  count()
density<-dbeta(data, shape1 = 42, shape2 = 68)


bData<-data%>%
  select(Sex_at_recapture)
bData<-ifelse(bData$Sex_at_recapture == "F", 1, 0)

x_beta <- seq(0, 1, by = 0.02)    
dbeta<-dbeta(x_beta, 42, 68)
plot(x_beta, dbeta)




