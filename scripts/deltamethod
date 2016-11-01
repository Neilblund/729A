#adapted from: http://www.ats.ucla.edu/stat/stata/faq/margins_mlogcatcon.htm



#setup----
library(foreign)
dataset<-read.dta("http://www.ats.ucla.edu/stat/data/logitcatcon.dta")
model<-glm(y~f*s, family="binomial", data=dataset)

newdata<-data.frame(model.matrix(model, data=dataset))
vars<-c("s", "f", "f.s")

#dataset with s = 40 and different values of male or female
maledata<-replace(newdata, (vars), list(40, 0, 40*0))
femaledata<-replace(newdata, (vars), list(40, 1, 40*1))


#for plotting effect of gender across multiple values of s
sequence<-seq(from=min(newdata$s), to=max(newdata$s), by=1)
male.list<-lapply(sequence, function(x) replace(newdata, vars, list(x, 0, x*0)))
female.list<-lapply(sequence, function(x) replace(newdata, vars, list(x, 1, x*1)))



#predictions----

#predictions (f=0) for men and for women (f=1)
malepred<-(predict(model, newdata=maledata, type="response", se.fit=TRUE))

femalepred<-(predict(model, newdata=femaledata, type="response", se.fit=TRUE))

#effect size and confidence interval

delta.effect<-mean(femalepred$fit)-mean(malepred$fit)
delta.ub<-delta.effect+mean(sqrt(malepred$se.fit^2+femalepred$se.fit^2))*qnorm(.975)
delta.lb<-delta.effect-mean(sqrt(malepred$se.fit^2+femalepred$se.fit^2))*qnorm(.975)

deltas<-print(c("delta.effect"=delta.effect, "delta.lb"=delta.lb, "delta.lb"=delta.ub))

# plot marginal effect of gender across multiple values of s


malepred.list<-t(sapply(male.list, function(x) {
  preds<-predict(model, newdata=x, type="response", se.fit=TRUE)
  return(c(
    "prob"=mean(preds$fit),
    "se"=mean(preds$se.fit)))})
)

femalepred.list<-t(sapply(female.list, function(x) {
  preds<-predict(model, newdata=x, type="response", se.fit=TRUE)
  return(c(
    "prob"=mean(preds$fit),
    "se"=mean(preds$se.fit))
  )
  
})
)

effects<-femalepred.list[,"prob"]-malepred.list[,"prob"]
diff.se<-sqrt(femalepred.list[,"se"]^2+malepred.list[,"se"]^2)
effects.ub<-effects+(diff.se*qnorm(.975))
effects.lb<-effects-(diff.se*qnorm(.975))
plot(sequence, effects.list[,"effect"], ylim=c(-1, 1), ylab="marginal effect of gender", xlab="S", type="l", lty=1)


polygon(y=c(effects.list[,"effect.ub"], rev(effects.list[,"effect.lb"])), x=c(sequence, rev(sequence)), col=adjustcolor( "grey", alpha.f = 0.2), border="black", lty=2)

grid()
abline(h=0, col='red')
title(main="Effect of gender at different values of S")

#add a rug plot showing the actual distribution of the modifying variable
rug(jitter(dataset$s))



#add a boxplot to show a five number summary of S
dat<-summary(dataset$s)
lines(x=c(dat[1], dat[6]), y=c(-.75, -.75), lty=1)
points(x=c(dat[1], dat[6]), y=c(-.75, -.75), pch=5)
rect(xleft=dat[2], xright=dat[5], ybottom=-.80, ytop=-.70, col="white")
lines(x=c(dat[3], dat[3]), y=c(-.80, -.70))
points(x=c(dat[4]), y=c(-.75))


