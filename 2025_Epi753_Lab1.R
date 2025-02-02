#######################################################
#######  EPI                753########################
#######################################################


######### LAB 1 #######################################

#########
# Translated from the Stata version by 
# Mateo Bandala-Jacques and edited by Derek Ng
# Last updated: 01/04/2022

#Install the requires packages if needed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("haven")) install.packages("haven")  #The haven package provides ability to import STATA (and other) datasets

#LOAD THE REQUIRED PACKAGES

library(tidyverse)
library(haven)

# Set your working directory to where the data is, OR
# Place this .R script in the same folder where the data is

getwd()

#If you are on a Mac, the file pathway looks something like this (I'm using iCloud):
setwd("/Users/antoniobandala/Library/Mobile Documents/com~apple~CloudDocs/Hopkins/ScM Year 2/Term 3/Epi 753/Lab1") #Change this!
# If you are on Windows, the file pathway looks something like this: (Uncomment if needed):
#setwd("C:\\Users\\dng\\OneDrive - Johns Hopkins\\EPI 753 - 2025\\Labs\\Conversion to R\\Lab1_2023\\2023\\")



#Load the data
lab1 <- read_dta("2025_Epi753_Lab1.dta")
names(lab1) #Name of the variables in the data


#######################
# BACKGROUND##########
######################

#Average life expectancy
summary(lab1$LifeExpectancy)

#Average median income
summary(lab1$MedianIncome)

#Life expectancy and median income by Statistical Area

lab1 %>%
  select(CSA=CommunityStatisticalArea, LifeExpectancy, MedianIncome) %>%
  print(n=Inf)
#If you are confused at what the %>% means,
#basically, it means "and then..."
#So: Using the object 'lab1', select the following 3 variables, 
# display the name of CommunityStatisticalArea as CSA, and then 
# print all records.
# Sometimes the select function may not work, but try restarting R and it usually works. 

######################
## Question 1 ########
######################

#Graph of relationship of life expectancy and income


InClass_Q1_Lowess <-ggplot(data=lab1, aes(x=MedianIncome, y=LifeExpectancy)) +
  geom_point()+
  geom_smooth(method="loess", se=FALSE)+
  labs(x="Median Household Income", y="Life expectancy at birth")+
  ggtitle("Lowess smoother")

InClass_Q1_Lowess

ggsave(filename = "lowess_q1in_class_R.png", plot = InClass_Q1_Lowess, device = "png",
            width=4, height=4, unit="in")


############################
# Questions 1-2 (at home)###
############################

#Model 1: Income modeled as a binary variable

lab1$below35000 <- ifelse(lab1$MedianIncome<35000,1,0)
table(lab1$below35000, useNA='ifany')


#I am using as.factor(below35000) to be very specific that this is not a numeric variable
#In this particular case it doesn't matter because the interpretation is:
#for a one unit increase in the independent variable. Try coding it with and without the as.factor function to confirm.
#In more complex data, categorical variables can be coded as:
#1,2,3,4,5 or 0,1,2,3, and here it would be important to specify that they are not numeric! 
# See Question 5 below about ordinal interpretations
model1<-lm(LifeExpectancy ~ as.factor(below35000), data=lab1)
summary(model1)

###################################
##### Questions 3-4 (at-home)######
###################################

#Model 2: Income modeled as a disjoint category variable

MedIncQuant <- quantile(lab1$MedianIncome, probs=c(0.2, 0.4, 0.6, 0.8), type=6)
MedIncQuant
#The option type=6 in the quantiles function matches STATA but the default is fine too

lab1$income_ind2 <- ifelse(lab1$MedianIncome>MedIncQuant[1]  & lab1$MedianIncome<=MedIncQuant[2] ,1,0)
lab1$income_ind3 <- ifelse(lab1$MedianIncome>MedIncQuant[2]  & lab1$MedianIncome<=MedIncQuant[3] ,1,0)
lab1$income_ind4 <- ifelse(lab1$MedianIncome>MedIncQuant[3]  & lab1$MedianIncome<=MedIncQuant[4] ,1,0)
lab1$income_ind5 <- ifelse(lab1$MedianIncome>MedIncQuant[4]  ,1,0)

model2 <- lm(LifeExpectancy ~ income_ind2 + income_ind3 + income_ind4 +income_ind5, data=lab1)
summary(model2)


############################
# Question 5 (at-home)#####
############################

#Model 3: Income modeled as an ordinal variable with categorical scores of 0-4

lab1$income_qt1 <- ifelse(lab1$income_ind2==1,1, ifelse(
                          lab1$income_ind3==1,2, ifelse(
                          lab1$income_ind4==1,3,ifelse(
                          lab1$income_ind5==1,4,0))))
table(lab1$income_qt1)

model3 <- lm(LifeExpectancy ~ income_qt1, data=lab1)
summary(model3)
# Note: as.factor() should not be used. The coded structure treats the categorical variable as a continuous variable
# with the interpretation of a one unit increase in the independent variable


# Alternative model FYI consideration
# Model XX: Income modeled as an ordinal variable with categorical scores equal
# to midpoint median income level within each quintile of median income

# incvals <- c(14105+30853,30853+36569,36569+40618,40618+58244,58244+104482)/2
# incindex <- 1 + lab1$income_ind2 + 2*lab1$income_ind3 + 3*lab1$income_ind4 + 4*lab1$income_ind5
# lab1$income_qt2 <- incvals[incindex]


# 
# table(lab1$income_qt2)
# modelx <- lm(LifeExpectancy ~ income_qt2, data=lab1)
# summary(modelx)



############################
# Question 5 (in-class)######
#############################

#Visual model comparison

# aggregate function: Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
means_m1 <- aggregate(LifeExpectancy ~ below35000, data = lab1, FUN = mean)
means_m2 <- aggregate (LifeExpectancy~ income_ind2 + income_ind3 + income_ind4 +income_ind5, data=lab1, FUN=mean)


#Complex figure describing assumptions of constant levels within bins
InClass_Q5<-ggplot(data = lab1, aes(x = MedianIncome, y = LifeExpectancy)) +
  geom_point(size=0.5) +
  geom_smooth(method = "loess", se = FALSE, color="black") +
  #From model 1
  geom_segment(aes(x = 0, xend = 35000, y = means_m1$LifeExpectancy[2], yend = means_m1$LifeExpectancy[2]),
               linetype = "solid", color = "red") +
  geom_segment(aes(x = 35000, xend = max(lab1$MedianIncome), y = means_m1$LifeExpectancy[1], yend = means_m1$LifeExpectancy[1]),
               linetype = "solid", color = "red") +
  #From model2
  geom_segment(aes(x = 0, xend = 30853, y = means_m2$LifeExpectancy[1], yend = means_m2$LifeExpectancy[1]),
               linetype = "solid", color = "blue") +
  geom_segment(aes(x = 30853, xend = 36569, y = means_m2$LifeExpectancy[2], yend = means_m2$LifeExpectancy[2]),
               linetype = "solid", color = "blue") +
  geom_segment(aes(x = 36569, xend = 40618, y = means_m2$LifeExpectancy[3], yend = means_m2$LifeExpectancy[3]),
               linetype = "solid", color = "blue") +
  geom_segment(aes(x = 40618, xend = 58243, y = means_m2$LifeExpectancy[4], yend = means_m2$LifeExpectancy[4]),
               linetype = "solid", color = "blue") +
  geom_segment(aes(x = 58243, xend = max(lab1$MedianIncome), y = means_m2$LifeExpectancy[5], yend = means_m2$LifeExpectancy[5]),
               linetype = "solid", color = "blue") +
  #From model 3
  geom_segment(aes(x = 0, xend = 30853, y = 69.08, yend = 69.08),
               linetype = "solid", color = "darkgreen") +
  geom_segment(aes(x = 30853, xend = 36569, y = 71.27, yend = 71.27),
               linetype = "solid", color = "darkgreen") +
  geom_segment(aes(x = 36569, xend = 40618, y = 73.46, yend = 73.46),
               linetype = "solid", color = "darkgreen") +
  geom_segment(aes(x = 40618, xend = 58243, y = 75.66, yend = 75.66),
               linetype = "solid", color = "darkgreen") +
  geom_segment(aes(x = 58243, xend = max(lab1$MedianIncome), y = 77.85, yend = 77.85),
               linetype = "solid", color = "darkgreen") +
  #Labs
  labs(x = "Median Household Income", y = "Life expectancy at birth") +
  ggtitle("Relationship between life expectancy and income")+
  geom_text(x = 5000, y = 85, label = "Binary Model 1", color = "red", hjust=0)+
  geom_text(x = 5000, y = 83, label = "Disjoint Model 2", color = "blue", hjust=0)+
  geom_text(x = 5000, y = 81, label = "Ordinal Model 3", color = "darkgreen", hjust=0)

InClass_Q5

ggsave(filename = "q5in_class_R.png", plot = last_plot(), device = "png",
       width=6, height=4, unit="in")



################################
# Questions 6-7 (at-home) #######
#################################

#Model 4
# Income modeled as continuous variable (untransformed)
#Change format for estimates to remove scientific notation
options(scipen=10, digits= 4) 
model4 <- lm(LifeExpectancy ~ MedianIncome, data=lab1)
summary(model4)




#################################
# Questions 8-9 (at home) #######
#################################

#Model 5: Income modeled as continuous variable (d
lab1$income_centered_scaled <- (lab1$MedianIncome - mean(lab1$MedianIncome))/sd(lab1$MedianIncome)

model5 <- lm(LifeExpectancy ~ income_centered_scaled, data=lab1)
summary(model5)


##################################
# Question 8 (in-class) ##########
##################################

# Visual Model Comparison


InClass_Q8 <-ggplot(data = lab1, aes(x = MedianIncome, y = LifeExpectancy)) +
  geom_point(size=0.5) +
  geom_smooth(method = "loess", se = FALSE, color="black") +
  #From model 1
  geom_segment(aes(x = 0, xend = 35000, y = means_m1$LifeExpectancy[2], yend = means_m1$LifeExpectancy[2]),
               linetype = "solid", color = "red") +
  geom_segment(aes(x = 35000, xend = max(lab1$MedianIncome), y = means_m1$LifeExpectancy[1], yend = means_m1$LifeExpectancy[1]),
               linetype = "solid", color = "red") +
  #From model2
  geom_segment(aes(x = 0, xend = 30853, y = means_m2$LifeExpectancy[1], yend = means_m2$LifeExpectancy[1]),
               linetype = "solid", color = "blue") +
  geom_segment(aes(x = 30853, xend = 36569, y = means_m2$LifeExpectancy[2], yend = means_m2$LifeExpectancy[2]),
               linetype = "solid", color = "blue") +
  geom_segment(aes(x = 36569, xend = 40618, y = means_m2$LifeExpectancy[3], yend = means_m2$LifeExpectancy[3]),
               linetype = "solid", color = "blue") +
  geom_segment(aes(x = 40618, xend = 58243, y = means_m2$LifeExpectancy[4], yend = means_m2$LifeExpectancy[4]),
               linetype = "solid", color = "blue") +
  geom_segment(aes(x = 58243, xend = max(lab1$MedianIncome), y = means_m2$LifeExpectancy[5], yend = means_m2$LifeExpectancy[5]),
               linetype = "solid", color = "blue") +
  #From model 4
  geom_abline(slope = coef(model4)[2], intercept = coef(model4)[1], color = "darkgreen", linewidth=1)+

  #Labs
  labs(x = "Median Household Income", y = "Life expectancy at birth") +
  ggtitle("Relationship between life expectancy and income")+
  geom_text(x = 5000, y = 85, label = "Binary Model 1", color = "red", hjust=0)+
  geom_text(x = 5000, y = 83, label = "Disjoint Model 2", color = "blue", hjust=0)+
  geom_text(x = 5000, y = 81, label = "Linear Model 4", color = "darkgreen", hjust=0)

InClass_Q8

ggsave(filename = "q8in_class_R.png", plot = InClass_Q8, device = "png",
       width=6, height=4, unit="in")


