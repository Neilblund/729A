#example plotting confidence intervals for a continuous IV using the simulated values approach

#read in simulated data
simdat<-read.csv("https://raw.githubusercontent.com/Neilblund/729A/master/data/simdata.csv", stringsAsFactors=FALSE)

#estimate probit regression
my.model<-glm(y~x1+x2,family="binomial"(link="probit"), data=simdat)



library(mvtnorm)
coefs <- rmvnorm(300, coef(my.model), vcov(my.model))

simul <- function(model, #the regression model
                  vname, #name of variable you want to change 
                  newvalues, #the new values
                  confint = .975, #confidence level default two-tailed .5
                  seed = 123, #random number seed (for replication)
                  nreps = 1000){  #number of simulations
  require(mvtnorm)
  set.seed(seed)
  coefs <- rmvnorm(nreps, coef(model), vcov(model))  #simulate random coefficients
  
  pmat<-data.frame(matrix(ncol=5, nrow=length(newvalues))) 
  colnames(pmat)<-c("xvalue","se", "lb", "probability", "ub")
  
  for(i in 1:length(newvalues)){
    data<-model.matrix(model$formula,replace(model$data, vname,newvalues[i])) # replace with hypothetical value
    if (model$family$link == "logit") {
      sim <- rowMeans(plogis(coefs %*% t(data))) #get predictions for logit model
    }
    if (model$family$link == "probit") {
      sim <- rowMeans(pnorm(coefs %*% t(data))) #get predictions for probit model
      
    }

    
    
    pmat[i,]<-c(newvalues[i],
                sd(sim), 
                mean(sim) - sd(sim)*qnorm(confint), #lower bound
                mean(sim), 
                mean(sim) + sd(sim)*qnorm(confint)) #upper bound
  }
    
  
  
  
  
  

  
  return(pmat)
  
}





vals<-seq(min(simdat$x1), max(simdat$x1), by=.1) #values from min of x1 to max of x1
probmat<-simul(my.model, "x1", vals) #get simulated mean and confidence interval





library(ggplot2)
plot<-ggplot(data=probmat, aes(x=xvalue, y=probability)) + geom_point() + geom_line() +
      geom_ribbon(aes(ymin=probmat$lb, ymax=probmat$ub), linetype=2, alpha=0.1)
plot
