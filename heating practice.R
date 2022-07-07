#practice to develop code for leaf heat tolerance
#Joe Endris

#libraries
library(dplyr)
library(ggplot2)
library(tidyr)

#create dataframe
leaf_heating <- read.csv("leaf_heating.csv")

#check data for correctness
glimpse(leaf_heating)

#create unique ID for each species and individual
unique_ID <- mutate(leaf_heating, unique_ID = paste(id, Individual, sep = ""))

#force R to recognize Fv/Fm results of 0.000 as 0.001
replace(unique_ID$FvFm, unique_ID$FvFm < 0.001, 0.001)

#run function from Evan
logit <- qlogis
attach(unique_ID)
#get parameter estimates for logistic decay model
cof=coef(lm(logit(leaf_heating)~Temperature))
#Fit a non linear least squares model to the FvFm and Temperature data
HT.model <- nls(FvFm ~ theta1/(1 + exp(-(theta2 + theta3*Temperature))),  start=list(theta1 = .8, theta2 = cof[1], theta3 = cof[2]),
                trace=F, control=list(maxiter=5000, tol=0.1))#had to relax the tolerance to get model convergence, original tolerance is 1e-3

