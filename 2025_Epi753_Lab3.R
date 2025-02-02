#######################################################
#######  EPI                753########################
#######################################################

################################################
#################### LAB 3 #####################
################################################

#########
# Translated from the Stata version by 
# Mateo Bandala-Jacques and edited by Derek Ng
# Last updated: 01/06/2024




#Install the requires packages if needed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("haven")) install.packages("haven")  #This one is to import the Stata data
if (!require("janitor")) install.packages("janitor") #This one will help us with tables
if (!require("epitools")) install.packages("epitools") #This one is the R equivalent to Stata epitab, which is for contingency tables
if (!require("sandwich")) install.packages("sandwich") # This one is for robust standard errors
if (!require("biostat3")) install.packages("biostat3") # This one is for linear combinations
if (!require("car")) install.packages("car") #Just trust me



#LOAD THE REQUIRED PACKAGES

library(tidyverse)
library(haven)
library(janitor)
library(epitools)
library(sandwich)
library(biostat3)
library(car)

# options(scipen=7) ## This removes scientific notation which may be a bit easier to interpret at first, but be careful for small values

# Set your working directory to where the data is, OR
# Place this .R script in the same folder where the data is

getwd()

#If you are on a Mac, the file pathway looks something like this (I'm using iCloud):
setwd("/Users/antoniobandala/Library/Mobile Documents/com~apple~CloudDocs/Hopkins/ScM Year 2/Term 3/Epi 753/Lab3") #Change this!
# If you are on Windows, the file pathway looks something like this: (Uncomment if needed):
#setwd("C:\\Users\\dng\\OneDrive - Johns Hopkins\\EPI 753 - 2025\\Labs\\Conversion to R\\Lab3_2023\\2023\\")



#Load the data
lab3 <- read_dta("2025_Epi753_Lab3.dta")
names(lab3) #Name of the variables in the data



######################################
########AT HOME QUESTIONS 2-4#########
######################################

#Table of injection drug use and depressive symptoms


#These are just the Ns, since that is all we need to complete the table
lab3 %>%
  tabyl(pi_regimen, tx_failure) %>%
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>%
  adorn_title()

#But we can also add the percentages if we want.
lab3 %>%
  tabyl(pi_regimen, tx_failure) %>%
   adorn_percentages("row") %>%
   adorn_pct_formatting(digits=1) %>%
   adorn_ns()%>%
   adorn_title()


#Alternate code to calculate simple ROR and RR using epitab command (from the epitools package)

#Risk odds ratio
epitab(as.factor(lab3$pi_regimen), as.factor(lab3$tx_failure),
       method="oddsratio")

#Risk ratio
epitab(as.factor(lab3$pi_regimen), as.factor(lab3$tx_failure),
       method="riskratio")

#The epitools package does not contain a function to calculate risk difference.
#But knowing what it is we can calculate it ourselves

model_x <- glm(tx_failure ~ pi_regimen, data=lab3, family=binomial(link="identity"))
#By applying link=identity to a binomial family, we should know what we are trying to achieve
# In this case, the (Intercept) coefficient is simply the risk of treatment failure when pi_regimen=0,
# and the coefficient for pi_regimen is the risk difference for treatment failure when comparing pi_regimen=1 vs. pi_regimen=0
summary(model_x)
confint(model_x)
#Had we used link="log" (i.e, the EXPONENTIATED Beta0 would have been "risk" in group 0, 
#                         and the EXPONENTIATED Bet1 would have been RR of death in group 1 vs. group 0)




######################################
########AT HOME QUESTION 5############
######################################

#Stratified analysis: First must dichotomize CD4 cell count
lab3$lowCD4 <- ifelse(lab3$cd4<200,1,0)
table(lab3$lowCD4, useNA="ifany")

#Contingency table for those without low CD4
lab3 %>%
  filter(lowCD4==0)%>%
  tabyl(tx_failure, pi_regimen) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=1) %>%
  adorn_ns()%>%
  adorn_title()


#Contingency table for those with low CD4
lab3 %>%
  filter(lowCD4==1)%>%
  tabyl(tx_failure, pi_regimen) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=1) %>%
  adorn_ns()%>%
  adorn_title()


#Base R can calculate MH *ODDS* Ratio
mantelhaen.test(lab3$pi_regimen, lab3$tx_failure, lab3$lowCD4)$estimate

#Although we don't have a package to calculate MH RR,
#Knowing how MH works, we can calculate our own MH risk ratio

#I am creating a new temporary object called MH
#This is because I will be modifying some variables

mh <- lab3
mh$pi_regimen <- factor(mh$pi_regimen, levels=c(1,0)) #These two lines are only to get the variables in the correct order for our 2x2 table
mh$tx_failure <- factor(mh$tx_failure, levels=c(1,0))

table_highcd4 <- mh %>%
    filter(lowCD4==0)%>%
     tabyl(pi_regimen, tx_failure)%>%
    adorn_totals(where="col") %>%
  adorn_totals(where="row")


table_lowcd4 <- mh %>%
  filter(lowCD4==1)%>%
  tabyl(pi_regimen, tx_failure)%>%
  adorn_totals(where="col")%>%
  adorn_totals(where="row")

table_all <- rbind(table_highcd4, table_lowcd4)
table_all


numerator <- (((table_all[1,2] * table_all[2,4]) / (table_all[3,4]+table_all[6,4]))    + 
              ((table_all[4,2] * table_all[5,4]) / (table_all[3,4]+table_all[6,4])))


denominator <- (((table_all[2,2] * table_all[1,4]) / (table_all[3,4]+table_all[6,4]))    + 
                ((table_all[5,2] * table_all[4,4]) / (table_all[3,4]+table_all[6,4])))

#And this is our adjusted MH RR for treatment failure given a PI regimen
# This is identical to the STATA output for this question
round((numerator/denominator),2)

rm(mh)

######################################
######## AT HOME QUESTIONS 6-7 #######
######## IN CLASS QUESTION 5 #########
######################################

########## Linear model 1 (uses robust standard error)############

model1 <- lm(as.numeric(tx_failure) ~ pi_regimen, data=lab3)
summary(model1)


# Compute robust standard errors using the sandwich package
robust_se1 <- sqrt(diag(vcovHC(model1, type = "HC0")))
robust_se1

# ######### Linear model 2 (uses robust standard error) #############
model2 <- lm(as.numeric(tx_failure) ~ pi_regimen + lowCD4, data=lab3)
summary(model2)

robust_se2 <- sqrt(diag(vcovHC(model2, type = "HC0")))
robust_se2



############ Log-Binomial Model 3 #################
model3 <- glm(tx_failure ~ pi_regimen, data=lab3, family=binomial(link="log"))
summary(model3)

exp(coef(model3))
exp(confint(model3))

########### Log-Binomial Model 4 ################
model4 <- glm(tx_failure ~ pi_regimen + lowCD4, data=lab3, family=binomial(link="log"))
summary(model4)

exp(coef(model4))
exp(confint(model4))


lab3$cd4s = lab3$cd4 * lab3$cd4
lab3$cd4c = lab3$cd4 * lab3$cd4* lab3$cd4

########### Log-Binomial Model 5##############

model5a <- glm(tx_failure ~ pi_regimen + cd4, data=lab3, family=binomial(link="log"))
summary(model5a)

exp(coef(model5a))
exp(confint(model5a))



model5b <- glm(tx_failure ~ pi_regimen + cd4, data=lab3, family = poisson)
summary(model5b)
round((exp(coefficients(model5b))),2) 


robust_se5b <- sqrt(diag(vcovHC(model5b, type = "HC0")))
conf_intervals <- exp(cbind(coef(model5b) - 1.96 * robust_se5b, coef(model5b) + 1.96 * robust_se5b))


results <- data.frame(
  "Coefficients" = round((exp(coef(model5b))),4),
  "Robust SE" = round((robust_se5b),5),
  "Lower CI" = round((conf_intervals[, 1]),4),
  "Upper CI" = round((conf_intervals[, 2]),4)
)

print(results)



######### Log-Binomial Model 6 ##############

#It will fail to converge. 
#model6 <- glm(tx_failure ~ pi_regimen + cd4 +cd4s + cd4c, data=lab3, family=binomial(link="log"))

#So, let's try it as a Poisson regression with robust standard error
model6 <- glm(tx_failure ~ pi_regimen + cd4 + cd4s + cd4c, data=lab3, family=poisson)
summary(model6) #As we just saw, to extract the robust standard errors we need some additional steps

#Let's extract the coefficients first
round((exp(coefficients(model6))),3)

#And now the standard errors and confidence intervals
robust_se6 <- sqrt(diag(vcovHC(model6, type = "HC0")))
conf_intervals6 <- exp(cbind(coef(model6) - 1.96 * robust_se6, coef(model6) + 1.96 * robust_se6))


results <- data.frame(
  "Coefficients" = round((exp(coef(model6))),4),
  "Robust SE" = round((robust_se6),5),
  "Lower CI" = round((conf_intervals6[, 1]),4),
  "Upper CI" = round((conf_intervals6[, 2]),4)
)

print(results) 

######### Logistic Model 7 ##############

model7 <- glm(tx_failure ~ pi_regimen, data=lab3, family=binomial(link="logit"))
summary(model7)

round((exp(coefficients(model7))),3)
round((exp(confint(model7))),3)

######### Logistic Model 8 ##############
model8 <- glm(tx_failure ~ pi_regimen + lowCD4, data=lab3, family=binomial(link="logit"))
summary(model8)

round((exp(coefficients(model8))),3)
round((exp(confint(model8))),3)

######### Logistic Model 9 ##############
model9 <- glm(tx_failure ~ pi_regimen + cd4, data=lab3, family=binomial(link="logit"))
summary(model9)

round((exp(coefficients(model9))),3)
round((exp(confint(model9))),3)

######### Logistic Model 10 ##############
model10 <- glm(tx_failure ~ pi_regimen + cd4 + cd4s + cd4c, data=lab3, family=binomial(link="logit"))
summary(model10)

round((exp(coefficients(model10))),3)
round((exp(confint(model10))),3)

######################################
######## AT HOME QUESTION 8 ##########
######################################

#Table of initial ART regimen (exposure)

lab3$ge50yrs <- ifelse(lab3$age>=50,1,0)

#Table for those younger than 50
lab3 %>%
  filter(ge50yrs==0)%>%
  tabyl(tx_failure, pi_regimen) %>%
  adorn_totals(where="row")%>%
  adorn_totals(where="col")%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=1) %>%
  adorn_ns()%>%
  adorn_title()


#Table for those 50 or older %>%
lab3 %>%
  filter(ge50yrs==1)%>%
  tabyl(tx_failure, pi_regimen) %>%
  adorn_totals(where="row")%>%
  adorn_totals(where="col")%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=1) %>%
  adorn_ns()%>%
  adorn_title()

## Code to calculate RD, RR, ROR within strata of age

#Under 50 years old
lab3_under50 <- lab3 %>% filter(ge50yrs==0)

#ROR
epitab(as.factor(lab3_under50$pi_regimen), as.factor(lab3_under50$tx_failure),
       method="oddsratio")
#RR
epitab(as.factor(lab3_under50$pi_regimen), as.factor(lab3_under50$tx_failure),
       method="riskratio")

#RD
rd_under50 <- glm(tx_failure ~ pi_regimen, data=lab3_under50, family=binomial(link="identity"))
coef(rd_under50)
confint(rd_under50)

rm(lab3_under50)

#50 or older
lab3_over50 <- lab3 %>% filter(ge50yrs==1)

#OR
epitab(as.factor(lab3_over50$pi_regimen), as.factor(lab3_over50$tx_failure),
       method="oddsratio")
#RR
epitab(as.factor(lab3_over50$pi_regimen), as.factor(lab3_over50$tx_failure),
       method="riskratio")

#RD
rd_over50 <- glm(tx_failure ~ pi_regimen, data=lab3_over50, family=binomial(link="identity"))
coef(rd_over50)
confint(rd_over50)

rm(lab3_over50)


######################################
###### AT HOME QUESTIONS 10-11 #######
########IN CLASS QUESTION 7 ##########
######################################

#Create product variable

lab3$pi_ge50 <- lab3$pi_regimen * lab3$ge50yrs


######### Linear Model 11 ##############
# Specify main effects and product term in model

model11 <- lm(tx_failure ~ pi_regimen + ge50yrs + pi_ge50, data=lab3)
#Let's extract the coefficients, but remember to use a robust standard error!
summary(model11)

#Robust standard errors:
sqrt(diag(vcovHC(model11, type = "HC0"))) 

#Linear combination pi_regimen + pi_ge50
lincom(model11, "pi_regimen+pi_ge50",eform=FALSE)



######### Log-Binomial Model 12 ##############

model12 <- glm(tx_failure ~ pi_regimen + ge50yrs + pi_ge50, data=lab3, family=binomial(link="log"))
summary(model12)

round((exp(coef(model12))),3)
round((exp(confint(model12))),3)

lincom(model12, "pi_regimen+pi_ge50",eform=TRUE)



######### Logistic Model 13 ##############
model13 <- glm(tx_failure ~ pi_regimen + ge50yrs + pi_ge50, data=lab3, family=binomial(link="logit"))
summary(model13)

round((exp(coef(model13))),3)
round((exp(confint(model13))),3)

lincom(model13, "pi_regimen+pi_ge50",eform=TRUE)

######################################
###### IN CLASS QUESTION 6 ###########
######################################

#On your own! :)


######################################
###### IN CLASS QUESTION 9 ###########
######################################



######################################
######## EXAMPLE CODE FOR ############
###### IN CLASS QUESTION 6 ###########
######################################

lab3$loghivrna <- log(lab3$hivrna)
lab3$loghivrnas <- lab3$loghivrna^2
lab3$loghivrnac <- lab3$loghivrna^3

lab3$black <- ifelse(lab3$raceeth==2,1,0)
lab3$hispanic <- ifelse(lab3$raceeth==3,1,0)
lab3$otherrace <- ifelse(lab3$raceeth==4,1,0)

lab3$idu <- ifelse(lab3$hivrisk==1,1,0)
lab3$hetero <- ifelse(lab3$hivrisk==3,1,0)
lab3$otherrisk <- ifelse(lab3$hivrisk==4,1,0)


# As a linear regression

q6_linear <- lm(tx_failure ~ pi_regimen + female + loghivrna + loghivrnas + loghivrnac+
                 cd4 + cd4s +cd4c + black + hispanic + otherrace + idu + hetero + otherrisk,
               data=lab3)

summary(q6_linear) # Extract the coefficients 
sqrt(diag(vcovHC(q6_linear, type = "HC0"))) # The robust standard errors to link with coefficients

#As log-binomial (approached with Poisson)

q6_poisson <- glm(tx_failure ~ pi_regimen + female + loghivrna + loghivrnas + loghivrnac+
                   cd4 + cd4s +cd4c + black + hispanic + otherrace + idu + hetero + otherrisk,
                 data=lab3, family=poisson)


robust_se <- sqrt(diag(vcovHC(q6_poisson, type = "HC0")))
conf_intervals <- exp(cbind(coef(q6_poisson) - 1.96 * robust_se, coef(q6_poisson) + 1.96 * robust_se))


results <- data.frame(
  "Coefficients" = round((exp(coef(q6_poisson))),4),
  "Robust SE" = round((robust_se),5),
  "Lower CI" = round((conf_intervals[, 1]),4),
  "Upper CI" = round((conf_intervals[, 2]),4)
)

print(results)


#As logistic

q6_logistic <- glm(tx_failure ~ pi_regimen + female + loghivrna + loghivrnas + loghivrnac+
                     cd4 + cd4s +cd4c + black + hispanic + otherrace + idu + hetero + otherrisk,
                   data=lab3, family=binomial(link="logit"))

exponentiated_coefs<-round((exp(coef(q6_logistic))),3)
cis<-round((exp(confint(q6_logistic))),3)

cbind(exponentiated_coefs, cis)
