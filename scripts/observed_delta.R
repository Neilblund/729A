library(ggplot2) #for plotting results
#utility function for replacing factor variables while keeping levels
replaceFactor<-function(data, varname, value){   
  if(is.factor(data[[varname]])){
    value<-factor(value, levels=levels(data[[varname]]))
    
  }
  data<-replace(data, varname, value)
}




#read in simulated data
simdat<-read.csv("https://raw.githubusercontent.com/Neilblund/729A/master/data/simdata.csv", stringsAsFactors=FALSE)



#getting prediction and confidence intervals using delta method

deltaPred<-function(model, vname, newvalues, confint=.975){
  pmat<-data.frame(matrix(ncol=5, nrow=length(newvalues))) 
  colnames(pmat)<-c("xvalue","se", "lb", "probability", "ub")
  #allpreds<-data.frame(matrix(ncol=nreps, nrow=length(newvalues))) 
  
  for(i in 1:length(newvalues)){
    data<-data.frame(model.matrix(model$formula,replaceFactor(model$model, vname,newvalues[i]))) # replace with hypothetical value, and create model matrix
    preds<-predict(model, newdata=data, type="link", se.fit=T)
    
    
    pmat[i,]<-c(
      newvalues[i],
      mean(model$family$linkinv(preds$se.fit)),
      mean(model$family$linkinv(preds$fit - preds$se.fit * abs(qnorm(confint)))), #lower bound
      mean(model$family$linkinv(preds$fit)), # mean
      mean(model$family$linkinv(preds$fit + preds$se.fit * abs(qnorm(confint)))) #upper bound
    )
    
    
  }
  return(pmat)
}

#estimate probit regression
my.model<-glm(y~x1+x2,family="binomial"(link="probit"), data=simdat)

newvalues<-seq(min(simdat$x1), max(simdat$x1), by=.1) #values from min of x1 to max of x1

probmat<-deltaPred(my.model, "x1", newvalues)


plot<-ggplot(data=probmat, aes(x=xvalue, y=probability)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin=probmat$lb, ymax=probmat$ub), linetype=2, alpha=0.1)
plot
dpreds<-deltaPred(my.model, "x1", newvalues)
