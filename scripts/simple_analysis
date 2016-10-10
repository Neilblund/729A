#packages----
library(foreign) #foreign is used to load .dta files , read.dta loads a file
library(obsval) #obsval for observed values logit
library(vioplot) #vioplot for violin plots
library(lrtest) #lrtest for likelihood ratio test - you can also use base R for this (see below)



#using obsval for logistic regression and prediction----

mymodel<-obsval(binary_warsup~female, #the regression model y~x1+x2+x3
                  data=studentVote,         #the dataset
                  reg.model="logit",          #the link function
                  n.draws=1000,             #the number of random draws
                   "effect.var"="female",       #the variable of interest
                  effect.vals=c(0,1) ,       #the low and high values of the IV of interest
                   verbose=TRUE, 
                  baseline.category=oppose) #this is the baseline category of your dependent variable



#results----

#this gets the basic model output
summary(mymodel$model)

#average effect of moving from low to high
mymodel$effect.mean
#low ci
mymodel$effect.low.ci
#high ci
mymodel$effect.high.ci

#predicted probability
mymodel$means

#plots----

#violin plots
plot.new()
vioplot(mymodel$preds[,1], mymodel$preds[,2], names=c("male","female"), 
        col="red", horizontal=TRUE)
title("Predicted Probability")

#coefficient plots
vioplot(mymodel$sim.coef[,1], mymodel$sim.coef[,2], names=c(expression(beta[0]),expression(beta[age])))

#histogram of predicted effect sizes
hist(mymodel$preds[,1]-mymodel$preds[,2], main="Simulated Effects", xlab="effects")


#Model fit----
AIC(mymodel$model)
BIC(mymodel$model)

#comparison with null model
nullModel<-glm(studentVote$binary_warsup~1, family="binomial")

#likelihood ratio test (using base R)
lr.test = -2*(logLik(nullModel) - logLik(mymodel$model))
1-pchisq(as.numeric(lr.test), df=1)


#likelihood ratio test (using lmtest)
library(lmtest)
lrtest(mymodel$model, nullModel)
