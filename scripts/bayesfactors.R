#bayes factor for hypothesis testing and model comparison, using the approach recommended by Wagenmakers 2007
#https://link.springer.com/article/10.3758/BF03194105

#generating some data----
set.seed(100)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
X <- cbind(1, x1, x2, x3)
b <- c(1, -.5, .5, 0) #the true effect of x3 is 0
p <- pnorm(X%*%b)
y <- rbinom(n, 1, p)

dat<-data.frame("y"=y, "x1"=x1, "x2"=x2,"x3"=x3)


model0<-glm(y~ x1 + x2, family="binomial", data=dat) #null hypothesis: x3 = 0
model1<-glm(y~x1+x2 + x3, family="binomial", data=dat) #alternative hypothesis x3!=0


bic0<-BIC(model0) 
bic1<-BIC(model1)

bic10<-bic1 - bic0
exp(bic10/2) # odds ratio of null hypothesis compared to alternative - evidence strongly favors null hypothesis

1/(1+exp(-.5 * bic10)) #probability of model0 (null) compared to probability of model1 (alternative)


#using a function to compare multiple models

bfactor<-function(mlist){
  bics<-numeric(length(mlist))
  
  for(i in 1:length(mlist)){
    bics[i]<-BIC(mlist[[i]])
    
  }
  prob<-round(exp(-.5*bics)/sum(exp(-.5*bics)), digits=5)
  odds<-round(prob/(1-prob), digits=5)
              
  ph<-data.frame("hypothesis"=paste(sapply(mlist, function(x) x$formula)),
                 "probability"=prob,
                 "bayes factor"=odds)
  return(ph)
  }


model0<-glm(y~1, family=binomial, data=dat) #intercept only model
model1<-glm(y~x3, family=binomial, data=dat) #worst model
model2<-glm(y~x1+x2, family=binomial, data=dat) #true model
model3<-glm(y~x1+x2+x3, family=binomial, data=dat) 

bfactor(list(model0, model1, model2, model3))

            

