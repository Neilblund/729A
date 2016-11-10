
library(nnet)

dataset<-read.csv("https://raw.githubusercontent.com/Neilblund/729A/master/data/austria_erp.csv")
dataset$votegen<-relevel(dataset$votegen, ref="extreme right")

#-----getting predictions

model<-multinom(votegen ~ u24 + ps+inter, data = dataset)
newdata<-data.frame(model.matrix(model))



prediction.function<-function(coefs, newdata){
  #add a row of a base coefficients
  model.coefs<-rbind("base"=0, coefs)
  #exponentiate
  model.coefs<-exp(model.coefs%*%t(newdata))
  #get the column means
  result<-apply(model.coefs, 1, function(x) mean(x/(colSums(model.coefs))))
  return(result)
}


prediction.function(coef(model), newdata)



#simulate coefficients---
#copied from obsval https://github.com/chrismeserole/obsval/blob/master/R/drawsimcoefs.R - shown here for clarity, simply use obsval::drawSimCoefs(model, "mlogit", 1000)
multiSim<-function(model, n.draws){
  n.coefs <- dim(coef(model))[2]
  n.coef.sets <- dim(coef(model))[1]
  sim.coefs <- array(NA, dim=(c(n.draws, n.coefs, n.coef.sets)), 
                     dimnames=list(NULL, colnames(coef(model)), 
                                                rownames(coef(model))))
  for(j in 1:n.coef.sets){
  
    dim1 <- (j - 1) * n.coefs + 1
    dim2 <- j * n.coefs

    sim.coefs[, , j] <- mvtnorm::rmvnorm(n.draws, 
                                         coef(model)[j, ], 
                                         vcov(model)[dim1:dim2, dim1:dim2]) 
  }
  return(sim.coefs)
}

resl<-multiSim(model, 1000)
predictions<-t(apply(resl, 1, function(x) prediction.function(t(x), newdata)))



####getting predictions and standard deviations----
colMeans(predictions)

apply(predictions, 2, sd)


####interaction effects----



simul.interaction <- function(model, bvar, cvar, ivar,
                              seed = 123,
                              nreps = 1000) {
  require(mvtnorm)
  set.seed(seed)
  
  newdata<-data.frame(model.matrix(model))
  
  vars<-c(cvar, bvar, ivar)
  

  
  data1 <- replace(newdata, (vars), list(1, 0, 1*0))
  data2 <- replace(newdata, (vars), list(0, 0, 0 * 0))
  data3 <- replace(newdata, (vars), list(1, 1, 1 * 1))
  data4 <- replace(newdata, (vars), list(0, 1, 0*1))
  
  
  
#####DIFFERENT!
  
  coefs<-multiSim(model, nreps)
  baseline<-prediction.function(coef(model), newdata)
  m1<-t(apply(resl, 1, function(x) prediction.function(t(x), data1)))
  m2<-t(apply(resl, 1, function(x) prediction.function(t(x), data2)))
  m3<-t(apply(resl, 1, function(x) prediction.function(t(x), data3)))
  m4<-t(apply(resl, 1, function(x) prediction.function(t(x), data4)))
  
  
  
#####
  
  sim<-(m2-m1)-(m4-m3)

  apply(sim, 2, mean)
  #standard errors
  sim.se<-apply(sim, 2, sd)
  sim.mean<-apply(sim, 2, mean)
  sim.z<-sim.mean/sim.se
  
  
  return(cbind("yhat"=baseline, "ie"=sim.mean, "ie.se"=sim.se, "ie.z"=sim.z))
  
}


result<-simul.interaction(model, bvar="u24", cvar="ps", ivar="inter")
result
