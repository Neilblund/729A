#example plotting confidence intervals for a continuous IV using the simulated values approach

library(ggplot2) #for plotting results
library(mvtnorm) #used for drawing simulated coefficients
#utility function for replacing factor variables while keeping levels
replaceFactor<-function(data, varname, value){   
  if(is.factor(data[[varname]])){
    value<-factor(value, levels=levels(data[[varname]]))
    
  }
  data<-replace(data, varname, value)
}




#read in simulated data
simdat<-read.csv("https://raw.githubusercontent.com/Neilblund/729A/master/data/simdata.csv", stringsAsFactors=FALSE)


simulPred <- function(model, #the regression model
                  vname, #name of variable you want to change 
                  newvalues, #the new values
                  confint = .975, #confidence level (default = two-tailed .05)
                  seed = 123, #random number seed (for replication)
                  nreps = 1000){  #number of simulations
  require(mvtnorm)
  set.seed(seed)
  coefs <- rmvnorm(nreps, coef(model), vcov(model))  #simulate random coefficients
  
  pmat<-data.frame(matrix(ncol=5, nrow=length(newvalues))) 
  allpreds<-data.frame(matrix(ncol=nreps, nrow=length(newvalues))) 
  
  colnames(pmat)<-c("xvalue","se", "lb", "probability", "ub")
  
  for(i in 1:length(newvalues)){
    data<-model.matrix(model$formula,replaceFactor(model$model, vname,newvalues[i])) # replace with hypothetical value, and create model matrix
    sim<-rowMeans(model$family$linkinv(coefs %*% t(data))) #use simulated coefficients to get new predictions
    
    pmat[i,]<-c(newvalues[i],
                sd(sim), 
                mean(sim) - sd(sim)*qnorm(confint), #lower bound
                mean(sim), #estimate
                mean(sim) + sd(sim)*qnorm(confint)) #upper bound
    allpreds[i,]<-sim
  }
  return(list("summary"=pmat,
              "predictions"=allpreds))
}
#estimate probit regression
my.model<-glm(y~x1+x2,family="binomial"(link="probit"), data=simdat)

newvalues<-seq(min(simdat$x1), max(simdat$x1), by=.1) #values from min of x1 to max of x1

probmat<-simulPred(my.model, "x1", newvalues) #get simulated mean and confidence interval


plot<-ggplot(data=probmat$summary, aes(x=xvalue, y=probability)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=probmat$summary$lb, ymax=probmat$summary$ub), linetype=2, alpha=0.1)
plot

