## Load libraries
library(tidyverse)
library(modelr)
#library(lmerTest)
library(lme4)
library(emmeans) #effect sizes
library(lsmeans) #means, des info
library(stats)

# Call data3 includes everything - inc n/a for missing BLP DOM var
data3 <- read.csv("~/BMB_JHS/EXP_JHS01/data_050823.csv")
View(data3) 

#Convert variable data types
data3$VF_ENG <- as.numeric(data3$VF_ENG)
data3$VF_SPA <- as.numeric(data3$VF_SPA)
data3$DELE_ACC <- as.numeric(data3$DELE_ACC)
data3$BSQ_L1S <- as.numeric(data3$BSQ_L1S)
data3$BSQ_L2S <- as.numeric(data3$BSQ_L2S)
data3$BSQ_OS <- as.numeric(data3$BSQ_OS)
data3$BSQ_CS <- as.numeric(data3$BSQ_CS)
data3$BSQ_US <- as.numeric(data3$BSQ_US)
data3$DOM <- as.numeric(data3$DOM)
data3$HSID <- as.character(data3$HSID)
data3$HSID <- as.factor(data3$HSID)

#Standardize variables
#standardize, did data2 and overwrote to do data3
data3$VF_ENGz <- scale(data3$VF_ENG)
data3$VF_SPAz <- scale(data3$VF_SPA)
data3$DELE_ACCz <- scale(data3$DELE_ACC)
data3$BSQ_L1Sz <- scale(data3$BSQ_L1S)
data3$BSQ_L2Sz <- scale(data3$BSQ_L2S)
data3$BSQ_OSz <- scale(data3$BSQ_OS)
data3$BSQ_CSz <- scale(data3$BSQ_CS)
data3$BSQ_USz <- scale(data3$BSQ_US)
data3$DOMz <- scale(data3$DOM)

#Generate a dummy variable for missing values in DOM variable where "NA" = 1. Then append it to the data3 data frame.
DOM_missing <- ifelse(is.na(data3$DOM), 1, 0)
data3$DOM_missing <- DOM_missing
view(DOM_missing)

#Create a new variable where missing values (NA) are replaced with 0. Then append it to data set.
DOM_v2 <- replace(DOM_v2, is.na(DOM_v2), 333)
data3$DOM_v2 <- DOM_v2

#Run model using both DOM variables as confounds?
###summary(lm(ACC_ENG ~ BSI_composite + ENG_first + VF_ENG + DOM_missing + DOM_v2 + BSI_composite*ENG_first , data3))
summary(lm(ACC_ENG ~ BSI_composite + ENG_first + DOM, data3))
summary(lm(ACC_ENG ~ BSI_composite + ENG_first + DOM_v2 + DOM_missing, data3))
summary(lm(ACC_ENG ~ BSI_composite + ENG_first + DOM_v2*DOM_missing, data3))
