# PANEL REGRESSION ANALYSIS- LEAST SQUARES DUMMY VARIABLE
# Author: EE Ajaegbu
# Date: 13/01/2021
# email: ajaegbu35@gmail.com
# Whatsapp: +234-8131355153

# Goal: To study the effects of Years of full time Experience on Wages of Head
#       of houehold
# Data: The data for this reserach was drawn from years 1976-1982 of the 
#       non-Survey of EconomicOpportunity portion of the Panel Study of 
#       Income Dynamics (PSID).

# Load Libraries
library(plm)
library(tidyverse)
library(ggplot2)
library(AER)

# 1) ExpLORATORY DATA ANALYSIS
# a) Structure of the Daa
str(PSID)

# Convert female(Gender) varaible to a factor object: 1= female and 0 is Male
PSID$FEM = as.factor(PSID$FEM)
# Convert BLK(Race) varaible to a factor object: 1= Black and 0 is Non Black
PSID$BLK = as.factor(PSID$BLK)

# b) Descriptive summary
summary(PSID)

# C) Visulaisation
# HISTOGRAM OF WAGES
ggplot(PSID,aes(x=LWAGE))+geom_histogram(bins=50)+
  labs(x="Wages",y='Frequency/Count')+
  labs(title=" HISTOGRAM OF WAGES",
       caption = "@ EE ajaegbu",tag="Normally Distributed")

# BOXPLOT OF WAGES BASED ON GENDER
ggplot(PSID,aes(x=FEM,y=LWAGE,fill=FEM))+ 
  geom_boxplot(outlier.colour = "blue",outlier.shape = 3)+
  labs(x="Female:Gender",y='Wages')+
  labs(title=" BOXPLOT OF WAGES BASED ON GENDER",
       caption = "@ EE ajaegbu",tag="OLS RegressIon")

# BOXPLOT OF WAGES BASED ON RACE
ggplot(PSID,aes(x=BLK,y=LWAGE,fill=BLK))+ 
  geom_boxplot(outlier.colour = "blue",outlier.shape = 3)+
  labs(x="Black: Race",y='Wages')+
  labs(title=" BOXPLOT OF WAGES BASED ON RACE",
       caption = "@ EE ajaegbu",tag="OLS RegressIon")

# HISTOGRAM OF Experience
ggplot(PSID,aes(x=EXP))+geom_histogram(bins=50)+
  labs(x="EXP",y='Frequency/Count')+
  labs(title=" HISTOGRAM OF EXP",
       caption = "@ EE ajaegbu",tag="Distribution")

# BOXPLOT OF EXP BASED ON GENDER
ggplot(PSID,aes(x=FEM,y=EXP,fill=FEM))+ 
  geom_boxplot(outlier.colour = "blue",outlier.shape = 3)+
  labs(x="Female:Gender",y='EXP')+
  labs(title=" BOXPLOT OF EXP BASED ON GENDER",
       caption = "@ EE ajaegbu",tag="OLS RegressIon")

# BOXPLOT OF EXP BASED ON RACE
ggplot(PSID,aes(x=BLK,y=EXP,fill=BLK))+ 
  geom_boxplot(outlier.colour = "blue",outlier.shape = 3)+
  labs(x="Black: Race",y='EXP')+
  labs(title=" BOXPLOT OF EXP BASED ON RACE",
       caption = "@ EE ajaegbu",tag="OLS RegressIon")


## Pooled OLS REGRESSION 

# Polability test
pooltest(LWAGE~ ED,data=nPSID,model="pooling")

# Creating a Panel Data
nPSID= pdata.frame(PSID, index = c("ID","YEAR"))

# OLS Regression Model: Regress Wages on Years of Education 
model1= lm(LWAGE~ ED,data=PSID)
summary(model1)

# Coefficient of the regression Model
coef = model1$coefficients

ggplot(PSID,aes(x=ED,y=LWAGE))+geom_point(color="blue")+
  geom_abline(intercept = coef[1],slope =coef[2],col="red")+
  labs(x="Years of Education",y='Wages')+
  labs(title=" Wages VS Years of Education",caption = "OLS Estimator",tag="PSID")

# Model Diagnostic
par(mfrow=c(2,2))
plot(model1)


# LEAST SQUARE DUMMY VARIABLE MODEL
#- Unonserved individual heterogenity
# via lm() - The least square Dummy Variable Fixed Effects Model
model2 =  lm(LWAGE~ ED + ID  -1, data = PSID)
summary(model2)
plot(model2)

# The Robust Standard Error for Heteroscedasticity
coeftest(model2,vcov=vcovHC(model1, type="HC0"))



## Thank You.