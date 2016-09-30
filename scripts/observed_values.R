############
#this function makes it a little easier to get a hypothetical value after regression
############
#function makes a data frame from observed predictors, replaces one predictor with specified value, and then gets new predictions
predProb<-function(varname, value, mod){
  xes<-data.frame(model.matrix(mod))
  xobs<-replace(xes, varname,value)
  xprob<-predict(mod, type="response", newdata=xobs)
  return(mean(xprob))
}

#example----

#read in simulated data
simdat<-read.csv("https://raw.githubusercontent.com/Neilblund/729A/master/data/simdata.csv", stringsAsFactors=FALSE)

#estimate probit regression
my.model<-glm(y~x1+x2,family="binomial"(link="probit"), data=simdat)

#use function to get P(Y=1) at mean of x1
predProb("x1", mean(simdat$x1), my.model)

#get P(Y=1) at max of x1
predProb("x1", max(simdat$x1), my.model)


##get marginal effects
mean(dnorm(predict(my.model, type = "link")))*coef(my.model)
