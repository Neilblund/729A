library(mvtnorm) #used for drawing simulated coefficients

replaceFactor<-function(data, varname, value){   #utility function for replacing factor variables while keeping levels
  
  if(is.factor(data[[varname]])){
    value<-factor(value, levels=levels(data[[varname]]))
    
  }
  data<-replace(data, varname, value)
}

replaceCase<-function(case, data){   # utility function for replacing values using a list
  for(i in 1:length(case)){
    data<-replaceFactor(data, names(case)[i], case[[i]])
    
  }
  return(data)
}


#read in simulated data
simdat<-read.csv("https://raw.githubusercontent.com/Neilblund/729A/master/data/simdata.csv", stringsAsFactors=FALSE)





simulFx <- function(model, #the regression model
                    case1, #a named list of values (baseline)
                    case2, #a named list of values (comparison)
                    seed = 123,#random number seed (for replication)
                    ci = .95, #confidence level (defaults to .95)
                    nreps = 1000,
                    full = FALSE #return all results or just the summary of results
){  #number of simulations
  require(mvtnorm)
  if(!is.null(seed)){
    set.seed(seed)
    
  }
  coefs <- rmvnorm(nreps, coef(model), vcov(model))  #simulate random coefficients
  
  
  
  data0<-model.matrix(eval(model$call$formula), replaceCase(case1, model$model)) #case1 (baseline)
  data1<-model.matrix(eval(model$call$formula), replaceCase(case2, model$model)) #case2 (comparison)
  
  x0<-rowMeans(model$family$linkinv(coefs %*% t(data0))) 
  x1<-rowMeans(model$family$linkinv(coefs %*% t(data1)))
  Xb<-x1 - x0
  est<-data.frame('x0' = mean(x0), "x1" = mean(x1), 
                  'lb' = quantile(Xb,  c((1 - ci)/2)), 
                  'est' = quantile(Xb, .5) ,
                  'ub'= quantile(Xb, 1-(1 - ci)/2), row.names=NULL)
                  if(full==TRUE){
                    ret<-data.frame("x0" =  x0,
                                    "x1" = x1,
                                    "fx" = Xb)
                    attr(ret, "summary")<-est
                  }
                  else{
                    ret<-est
                  }
                  
                  
                  return(ret)
}

my.model<-glm(y~x1+x2,family="binomial"(link="logit"), data=simdat)

#get estimated effect for an increase from x1 = 0 to x1 =1
preds<-simulFx(my.model, 
               case1=list("x1"=0), 
               case2=list("x1"=1))


preds
