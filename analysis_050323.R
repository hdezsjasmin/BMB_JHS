library(tidyverse)
library(ggplot2)
library(dplyr)
library(modelr)
#library(lmerTest)
library(lme4)
library(emmeans) #effect sizes
library(lsmeans) #means, des info
library(stats)

#read data to var
data1 <- read.csv("~/BMB_JHS/data/MASTER data file_ALLPARTICIPANTS-noex3.csv")
#remove n/a cells
data1 <- na.omit(data1)
#set ID as a factor
#(if needed, or convert to char)
#data$X <- as.factor(data$X)

#convert num var to int
data1$VF_ENG <- as.numeric(data1$VF_ENG)
data1$VF_SPA <- as.numeric(data1$VF_SPA)
data1$DELE_ACC <- as.numeric(data1$DELE_ACC)
data1$BSQ_L1S <- as.numeric(data1$BSQ_L1S)
data1$BSQ_L2S <- as.numeric(data1$BSQ_L2S)
data1$BSQ_OS <- as.numeric(data1$BSQ_OS)
data1$BSQ_CS <- as.numeric(data1$BSQ_CS)
data1$BSQ_US <- as.numeric(data1$BSQ_US)
data1$X <- as.character(data1$X)
data1$X <- as.factor(data1$X)
#create a dummy var for ENG_first

#standardize
data1$VF_ENGz <- scale(data1$VF_ENG)


summary(lm(ACC_SPA ~ 1 + ENG_first*BSI.composite + VF_SPA, data1))