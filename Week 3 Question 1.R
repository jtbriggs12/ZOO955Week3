#Title: Stat 955 Homework 3
#Authors: Jess Briggs & Amanda Kerkhove
#Date Created:2022-09-27

#Background
#Black sea bass are a protogynous hermaphrodite marine fish species – they change sex from female to male, although there are also primary males which first mature as males.  They do not change from male to female and they change sex primarily after the end of the spawning season (May to July).  The file BSB_tagging_data.xlsx contains the results of a tagging study conducted on this species.  Individual black sea bass (BSB) were captured, tagged with a unique identifier, and released on the “Date_at_capture.”  Some of these individuals were recaptured on the “Date_at_recapture.”  Sex and length were recorded at both time points (though this file only includes the “Length_at_capture.”  Sex change isn’t instantaneous, so some individuals recaptured while undergoing sex change are recorded as intersex.  These individuals can also be thought of as having changed sex.


#Question 1.1: Plot a probability density function (probability density vs. proportions from 0 to 1) for the proportion of female black sea bass that changed sex out of all those which were recaptured after the end of the spawning season (i.e., after July). 
library(lubridate)
library(dplyr)
library(ggplot2)

data<-read.csv("BSB_tagging_data.csv")%>%
  mutate(Date_at_recapture = mdy(Date_at_recapture))%>%
  filter(month(Date_at_recapture) > 7) %>%
  mutate(Sex_at_recapture = as.factor(Sex_at_recapture))

x_beta <- seq(0, 1, by = 0.02)    
dbeta<-dbeta(x_beta, 4, 59)
plot(x_beta, dbeta)


#Q1.2
p <- c(0.025, 0.975) #vector of probabilities
qbeta(p, 4, 59)

#Q1.3a Does the length of a female effect the probability of sex change given recapture after end of spawning season?
mf <- data %>% mutate(Sex_at_recapture = replace(Sex_at_recapture, Sex_at_recapture == 'Intersex', 'M'))
mf$Sex_at_capture <- ifelse(mf$Sex_at_capture == 'F', 1, 0)
mf$Sex_at_recapture <- ifelse(mf$Sex_at_recapture == 'F', 1, 0)
mf$Sex_at_capture <- as.numeric(mf$Sex_at_capture)
mf$Sex_at_recapture <- as.numeric(mf$Sex_at_recapture)
#Changed intersex at capture to M bc transition is in progress

#Add a success column:
mf <- mf %>% mutate(success = Sex_at_capture - Sex_at_recapture)

model <- glm(success ~ Length_at_capture, data = mf, family = binomial)
summary(model)
plot(model)
#No, length does not impact transition likelihood - p-value = 0.1122

#Q1.3b By how much is the log odds of sex change predicted to change for every millimeter increase in length?
#For every mm of length, there is a -0.014 percent log odds change of probability of sex change

#Q1.4 Plot the relationship between the probability of sex change for these individuals and length.  Overlay the model estimated relationship on the data.  Label axes appropriately and provide a figure caption

mf$predict <- predict.glm(model, type='response')

ggplot() +
  geom_point(data = mf, aes(x = Length_at_capture, y = success)) +
  geom_line(data = mf, aes(x = Length_at_capture, y = predict), color = 'blue') +
  ylab('Success where 1 = Sex Change') + xlab('Length at Initial Capture (mm)')

#Figure Caption: Model estimates shows that as tail length increases, probability of transition decreases.

              