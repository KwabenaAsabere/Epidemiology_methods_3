#######################################################
#######  EPI                753########################
#######################################################

################################################
#################### LAB 2 #####################
################################################

#########
# Translated from the Stata version by 
# Mateo Bandala-Jacques and edited by Derek Ng
# Last updated: 01/06/2024




#Install the requires packages if needed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("haven")) install.packages("haven")  #This one is to import the Stata data
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("janitor")) install.packages("janitor") #This one will help us with tables
if (!require("logbin")) install.packages("logbin") # This one helps with log-binomial models
if (!require("sandwich")) install.packages("sandwich") # This one is to calculate robust standard errors in R


#LOAD THE REQUIRED PACKAGES

library(tidyverse)
library(haven)
library(ggplot2)
library(janitor)
library(logbin)
library(sandwich)

# Set your working directory to where the data is, OR
# Place this .R script in the same folder where the data is

getwd()

#If you are on a Mac, the file pathway looks something like this (I'm using iCloud):
setwd("/Users/antoniobandala/Library/Mobile Documents/com~apple~CloudDocs/Hopkins/ScM Year 2/Term 3/Epi 753/Lab2") #Change this!
# If you are on Windows, the file pathway looks something like this: (Uncomment if needed):
#setwd("C:\\Users\\dng\\OneDrive - Johns Hopkins\\EPI 753 - 2025\\Labs\\Conversion to R\\Lab2_2023\\2023\\")



#Load the data
lab2 <- read_dta("2025_Epi753_Lab2.dta")
names(lab2) #Name of the variables in the data



######################################
########## AT HOME QUESTION 1#########
######################################

#Looking for missing values of the risk factor variables:
#We can do this individually:
summary(lab2$age)
summary(lab2$sex)  
summary(lab2$black)
summary(lab2$idu)
summary(lab2$hcv)
summary(lab2$hbv)
summary(lab2$smoking)
summary(lab2$htn)
summary(lab2$diabetes)
summary(lab2$cd4)
summary(lab2$ud_vl200)
summary(lab2$art)

#The summary function provides frequency of missingness (see "smoking") and does not list "NA's" if there is complete data
#but missingness can also be listed with the following loop

variables <- c("age", "sex", "black", "idu", "hcv", "hbv", "smoking",
               "htn", "diabetes", "cd4", "ud_vl200", "art")

for (i in variables) {
  missing_count <- sum(is.na(lab2[[i]]))
  cat("Missing values in", i, ":", missing_count, "\n")
}


######################################
########## AT HOME QUESTION 2#########
######################################

# Looking at the distribution of CD4 at ESRD by death status

#Must use as.factor(death) because otherwise R thinks it's numeric and cannot stratify by a continuous variable
ggplot(data=lab2, mapping = aes(x=as.factor(death), y=cd4)) +
  geom_boxplot() +
  labs(x="Death Status", y="CD4 count")

summary(lab2$cd4[lab2$death==0]) #CD4 summary when death=0
summary(lab2$cd4[lab2$death==1]) #CD4 summary when death=1



######################################
########## AT HOME QUESTION 5#########
######################################

#Modifying the form of the age variable

lab2$age_cat <- NA


lab2$age_cat[lab2$age >= 0 & lab2$age < 30] <- 1
lab2$age_cat[lab2$age >= 30 & lab2$age < 40] <- 2
lab2$age_cat[lab2$age >= 40 & lab2$age < 50] <- 3
lab2$age_cat[lab2$age >= 50 & lab2$age < 60] <- 4
lab2$age_cat[lab2$age >= 60 & lab2$age < 70] <- 5
lab2$age_cat[lab2$age >= 70] <- 6


lab2$age_cat <- factor(lab2$age_cat, levels = 1:6, labels = c("<30 yrs", "30 to 39 yrs", "40 to 49 yrs",
                                                              "50 to 59 yrs", "60 to 69 yrs", ">=70 yrs"))

Freq <- table(lab2$age_cat, useNA="ifany")
Percentage <- round((prop.table(Freq)*100),2)
cbind(Freq,Percentage)



######################################
######### IN-CLASS QUESTION 4#########
######################################

#Number of participants who die after ESRD diagnosis

FreqQ4 <- table(lab2$death, useNA="ifany")
PercentageQ4 <- round((prop.table(FreqQ4)*100),2)
cbind(FreqQ4,PercentageQ4)


######################################
######### IN-CLASS QUESTION 5#########
######################################

#Modify the form of the risk factor variables

ggplot(lab2, aes(x = age)) +
  geom_histogram(binwidth = 3.5, boundary = 18, color = "black") +
  labs(x = "Age", y = "Frequency", title = "Histogram of Age at ESRD") +
  theme_minimal()


lab2$age10 <- lab2$age/10

ggplot(lab2, aes(x = cd4)) +
  geom_histogram(binwidth = 68.2, boundary = 0, color = "black") +
  labs(x = "CD4", y = "Frequency", title = "Histogram of CD4 count at ESRD") +
  theme_minimal()

lab2$cd4gte350 <- ifelse(lab2$cd4 >=350,1,0)



######################################
######### IN-CLASS QUESTION 6#########
######################################

# Statistics for a table investigating the correlation between the risk factors and death

#Age
summary(lab2$age[lab2$death==0])
sd(lab2$age[lab2$death==0])

summary(lab2$age[lab2$death==1])
sd(lab2$age[lab2$death==1])

t.test(lab2$age ~ lab2$death, var.equal=T)
#t.test(lab2$age ~ lab2$death, var.equal=T)$p.value


#Evidently, all of the following code can be looped. But let's take the time to look at each variable one at a time

#Sex  (New package alert!)

lab2 %>%
  tabyl(sex, death) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()

fisher.test(lab2$sex, lab2$death)
# Can obtain the pvalue only, if you wish, but it is good manners to check all components of the test
#fisher.test(lab2$sex, lab2$death)$p.value


#Black race
lab2 %>%
  tabyl(black, death) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()

fisher.test(lab2$black, lab2$death)

#Injection drug use
lab2 %>%
  tabyl(idu, death) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()

fisher.test(lab2$idu, lab2$death)

#Hepatitis C infection

lab2 %>%
  tabyl(hcv, death) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()

fisher.test(lab2$hcv, lab2$death)

#Hepatitis B Infection

lab2 %>%
  tabyl(hbv, death) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title() 

fisher.test(lab2$hbv, lab2$death)

#Smoking (Remember that this one had NAs!)

lab2 %>%
  tabyl(smoking, death, show_na=FALSE) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()

fisher.test(lab2$smoking, lab2$death)

#Treated Hypertension
lab2 %>%
  tabyl(htn, death) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()

fisher.test(lab2$htn, lab2$death)

#Diabetes

lab2 %>%
  tabyl(diabetes, death) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()

fisher.test(lab2$diabetes, lab2$death)

#CD4 count ≥350 cells/mm3
lab2 %>%
  tabyl(cd4gte350, death) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()

fisher.test(lab2$cd4gte350, lab2$death)

#Undetectable HIV RNA
lab2 %>%
  tabyl(ud_vl200, death) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()

fisher.test(lab2$ud_vl200, lab2$death)

# Antiretroviral therapy

lab2 %>%
  tabyl(art, death) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()

fisher.test(lab2$art, lab2$death)



######################################
######### IN-CLASS QUESTION 7#########
######################################

#Univariate log-binomial regression models
model_age10 <- glm(death ~ age10, data=lab2, family=binomial(link="log"))
summary(model_age10) #Remember, this is the UNEXPONENTIATED (i.e., log scale) summary of the model, and contains a lot of information!
#But for now, let's focus on the coefficients and the CIs:
round((exp(coefficients(model_age10))),2)         #Exponentiate the coefficients from the model, and round to 2 digits
round((exp(confint(model_age10, level=0.95))),2)  #Exponentiate the CIs from the model, and round to 2 digits

#You can repeat the previous code for each variable, or...
#We can loop :)

      

variables_for_loop <- c("sex", "black", "idu", "hcv", "hbv", "smoking",
               "htn", "diabetes", "cd4gte350", "ud_vl200", "art")

# Create empty lists to store the results
models <- list()
unexp_summary <- list()
exp_coef <- list()
exp_ci <- list()

# Loop through each variable
for (i in variables_for_loop) {
  # Fit the log-binomial regression model
  model <- glm(paste("death ~", as.factor(i)), data = lab2, family = binomial(link = "log"))
  
  # Store the model and unexponentiated summary
  models[[i]] <- model
  unexp_summary[[i]] <- summary(model)
  
  # Calculate and store the exponentiated coefficients
  exp_coef[[i]] <- round(exp(coef(model)), 2)
  
  # Calculate and store the exponentiated confidence intervals
  exp_ci[[i]] <- round(exp(confint(model)), 2)
}


# Print the exponentiated coefficients
for (i in variables_for_loop) {
  cat("Exponentiated coefficients for", i, ":\n")
  print(exp_coef[[i]])
  cat("\n")
}

# Print the exponentiated confidence intervals
for (i in variables_for_loop) {
  cat("Exponentiated confidence intervals for", i, ":\n")
  print(exp_ci[[i]])
  cat("\n")
}

#You may notice that the confidence intervals from the Stata and R versions do not match
# Stata and R calculate CIs in different ways.
# We can re-create the same ones from the Stata output with some more advanced code, 
# but that is beyond our current scope.

# In real life, we must live with the fact that if we repeat the same analysis in different software,
#they might give us slightly different results. It does not mean that one of them is "wrong". 
#Regardless, as an epidemiologist, we should not be making inferences/decisions based on numbers alone!
# So if one software says P=0.049, and the other one says #P=0.051, does this matter?
#For most cases, it does not. We must look at the bigger picture.


######################################
######### IN-CLASS QUESTION 8#########
######################################

#Model 1: Multivariable log-binomial regression model

# I'm using the logbin package here.
# Normally. we can use glm(family=binomial(link="log")), but it does not converge in R which demonstrates the 
# limitation of this method with many covariates, 
# and I want it to match to what the STATA version is doing
# (That is, I did not want to fit a Poisson yet)

model1 <- logbin(death ~ age10 + black + idu + hbv + smoking + htn + diabetes +
                             cd4gte350 + ud_vl200,  data=lab2, method="em")

round((exp(coefficients(model1))),2) 
round((exp(confint(model1))),2) 



lab2 %>%
  tabyl(smoking, death, show_na=FALSE) %>%
  adorn_totals()%>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title()



######################################
######### IN-CLASS QUESTION 9#########
######################################

#Unfortunately, since we used logbin() to fit the log-binomial models, most packages that perform stepwise regression
# are incompatible!
#So we will be doing this by hand
#If you find a way around this please let us know!

model1 <- logbin(death ~ age10 + black + idu + hbv + smoking + htn + diabetes +
                   cd4gte350 + ud_vl200,  data=lab2, method="em")
summary(model1)

#Based on P>0.1, remove age10, black, idu, hbv, smoking, diabetes

model2 <- logbin(death ~ cd4gte350 + htn + ud_vl200,  data=lab2, method="glm")
summary(model2)

round((exp(coefficients(model2))),2) 
round((exp(confint(model2))),2) 




######################################
######### IN-CLASS QUESTION 10#########
######################################

# Model 3: Logistic regression model

model3 <- glm(death ~ age10 + black + idu + hbv +smoking + htn + diabetes + cd4gte350 +ud_vl200, data=lab2, family=binomial(link="logit"))

summary(model3) #Unexponentiated sumary
round((exp(coefficients(model3))),2) 
round((exp(confint(model3))),2) 




######################################
######### IN-CLASS QUESTION 11#########
######################################


# Model 1 + sex: Multivariable log binomial regression model

model1_sex <- glm(death ~ age10 + sex + black + idu + hbv + smoking + htn + diabetes +
                     cd4gte350 + ud_vl200, data=lab2, family=binomial(link="log"))
#It does not converge!

#what if we try with the logbin command?
# I suggest you don't attempt it; it crashed my computer

#So let's try it as a Poisson model with robust standard errors


model1_sex <- glm(death ~ age10 + sex + black + idu + hbv + smoking + htn + diabetes + cd4gte350 + ud_vl200,
             data = lab2, family = poisson)

#there is a trick in R though!
# The robust standard errors must be obtained some other way.

#From the model, we can obtain the exponents:
summary(model1_sex)
round((exp(coefficients(model1_sex))),2) 
#However, these Confidence intervals are incorrect!
round((exp(confint(model1_sex))),2) 

#Let's extract them
# Compute robust standard errors using the sandwich package
robust_se <- sqrt(diag(vcovHC(model1_sex, type = "HC0")))

#Use the robust standard error to calculate the confidence intervals
conf_intervals <- exp(cbind(coef(model1_sex) - 1.96 * robust_se, coef(model1_sex) + 1.96 * robust_se))

#Store the results
#Note that I round them. You don't have to, but I always prefer to do so.
results <- data.frame(
  "Coefficients" = round((exp(coef(model1_sex))),2),
  "Robust SE" = round((robust_se),3),
  "Lower CI" = round((conf_intervals[, 1]),2),
  "Upper CI" = round((conf_intervals[, 2]),2)
)

print(results)




