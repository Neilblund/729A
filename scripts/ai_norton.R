#Ai and Norton method for interaction effects with binary by continuous interaction will update with options for continuous by continuous-----

dataset <-
  read.dta("http://www.ats.ucla.edu/stat/data/logitcatcon.dta")
model <- glm(y ~ f * s, family = "binomial", data = dataset)


  simul.interaction <- function(model, bvar, cvar, ivar,
                                seed = 123,
                                nreps = 1000) {
    require(mvtnorm)
    set.seed(seed)
    coefs <- rmvnorm(nreps, coef(model), vcov(model))
    
    newdata<-data.frame(model.matrix(model))
    
    vars<-c(cvar, bvar, ivar)
    data1 <- replace(newdata, (vars), list(newdata[,cvar]+1, 0, (newdata[,cvar]+1) * 0))
    data2 <- replace(newdata, (vars), list(newdata[,cvar], 0, newdata[,cvar] * 0))
    data3 <- replace(newdata, (vars), list(newdata[,cvar]+1, 1, (newdata[,cvar]+1) * 1))
    data4 <- replace(newdata, (vars), list(newdata[,cvar], 1, newdata[,cvar] * 1))
    
    
    
    if (model$family$link == "logit") {
      baseline<-colMeans(plogis(coefs%*%t(newdata)))
      
      m1<-plogis(coefs%*%t(data2))
      m2<-plogis(coefs%*%t(data1))
      
      m3<-plogis(coefs%*%t(data4))
      m4<-plogis(coefs%*%t(data3))
      
  
      
    }
    if (model$family$link == "probit") {
      baseline<-colMeans(plogis(coefs%*%t(newdata)))
      
      m1<-pnorm(coefs%*%t(data2))
      m2<-pnorm(coefs%*%t(data1))
      
      m3<-pnorm(coefs%*%t(data4))
      m4<-pnorm(coefs%*%t(data3))
      
    }
    #difference in differences - marginal effect of s for men (m1-m2) minus the marginal effect of s for women (m3-m4) 
    sim<-(m1-m2)-(m3-m4)
    
    #standard errors
    sim.se<-apply(sim, 2, sd)
    
    #average effects
    sim.mean<-apply(sim, 2, mean)
    
    #zscores
    sim.z<-sim.mean/sim.se
    
    
    return(cbind("yhat"=baseline, "ie"=sim.mean, "ie.se"=sim.se, "ie.z"=sim.z))
    
  }
  


#get simulated interaction effects----

result<-simul.interaction(model, bvar="f", cvar="s", ivar="f.s")

colMeans(result)


#plot interaction effect----

plot(result[,1], result[,2], ylab="interaction effect", xlab="Pr Y=1")
abline(h=0, col='red', lty=2)


#plot a z score----
plot(result[,1], result[,4], xlim=c(0, 1), ylim=c(-5, 10), ylab="z-score", xlab="Pr Y=1")
abline(h=0, col="red")
abline(h=1.96, col="red", lty=2)
abline(h=-1.96, col='red', lty=2)


#add a box plot and some guidelines to the plot (optional)
dat <- summary(result[,1])
y<-(-5)


lines(x = c(dat[1], dat[6]),
      y = c(y,y),
      lty = 1)


rect(
  xleft = dat[2],
  xright = dat[5],
  ybottom = y-(y*.05),
  ytop = y+(y*.05),
  col = "white"
)

points(x = c(dat[1], dat[6]),
       y = c(y,y),
       pch = 5)

lines(x = c(dat[3], dat[3]), y = c(y-(y*.05),y+(y*.05)))
points(x = c(dat[4]), y = y, pch=20)

