library(mvtnorm)
library(vioplot)
dataset<-read.csv("https://raw.githubusercontent.com/Neilblund/729A/master/data/voterid.csv")

####Using vanilla R----

#create a binary indicator for gop control of legislature
dataset$gop_control<-as.numeric(dataset$gopleg>50)

#use complete.cases to remove rows with missing values
dataset<-dataset[complete.cases(dataset),]

ml<-glm(photo~gop_control*fraud, family="binomial", data = dataset)


#Using simulated values for some point predictions 

set.seed(123)


sim.coefs <- rmvnorm(1000, coef(ml), vcov(ml))


#a function to get simulated confidence intervals for a case
getCI<-function(sim.coefs, data, confint=c(.025, .975)){
  
  pred<-rowMeans(plogis(sim.coefs%*%t(data)))
  cis<-quantile(pred, confint)
  predCIs<-c("pred"=mean(pred), "lb"=cis[1], "ub"=cis[2])
  
  result<-list("predictions"=pred, "predCIs"=predCIs)
  
  return(result)
}

#our model matrix
mod<-data.frame(model.matrix(ml), stringsAsFactors = FALSE)


#average marginal effect of fraud for GOP legs
GOPleg<-replace(mod, c("gop_control", "fraud","gop_control.fraud"), c(rep(1, nrow(mod)), mod$fraud, mod$fraud*1))
DEMleg<-replace(mod, c("gop_control", "fraud","gop_control.fraud"), c(rep(0, nrow(mod)), mod$fraud, mod$fraud*0))

GOPmfx<-getCI(sim.coefs, GOPleg)$predictions

DEMmfx<-getCI(sim.coefs, DEMleg)$predictions


#violin plot of effects
vioplot(GOPmfx, DEMmfx, names=c("GOP legislature","Dem legislature"))
title(main="Probability of voter ID leg")




#a series of predicted probabilities----
interpred1<-t(sapply(seq(0, 10, by=1), 
                     function(x) getCI(sim.coefs, replace(mod, c("gop_control", "fraud","gop_control.fraud"), c(1, x, x*1)))$predCIs
))
interpred1<-data.frame(interpred1)
interpred2<-t(sapply(seq(0, 10, by=1), 
                     function(x) getCI(sim.coefs, replace(mod, c("gop_control", "fraud","gop_control.fraud"), c(0, x, 0)))$predCIs
))
interpred2<-data.frame(interpred2)


#plotting predictions----
plot(seq(0, 10, by=1), interpred1[,1], ylim=c(0, 1), xlab="fraud cases", ylab="Pr of photo ID law")
points(seq(0, 10, by=1), interpred2[,1], pch=2)

polygon(y=c(interpred1[,"lb.2.5."], rev(interpred1[,"ub.97.5."])), x=c(0:10, 10:0), col=adjustcolor( "yellow", alpha.f = 0.2), border="black", lty=2)
polygon(y=c(interpred2[,"lb.2.5."], rev(interpred2[,"ub.97.5."])), x=c(0:10, 10:0),  col=adjustcolor( "blue", alpha.f = 0.2), border="black", lty=4)

legend("bottomright", legend=c("GOP controlled", "DEM controlled"), pch=15 , col=c("yellow","blue"))
title(main="Predictions")




#ggplot----
library(ggplot2)
preds<-rbind(interpred1, interpred2)

preds<-data.frame(cbind("fraud"=rep(seq(0, 10), 2), 
                        "party_control"=rep(c("GOP Control", "DEM control"), each=11), preds))

p1<-ggplot(data=preds, aes(x=fraud, y=pred, group=party_control))+
  geom_line(aes(x=fraud, y=preds$lb.2.5., group=party_control, color=party_control))+
  geom_line(aes(x=fraud, y=preds$ub.97.5., group=party_control, color=party_control))+
  
  geom_point(aes(color=party_control))+
  scale_color_brewer(palette="Paired")+
  theme_minimal()+theme(legend.position="top",
                        axis.title.x=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank())+
  scale_y_continuous(expand = c(0,0))


p2<-ggplot(data=dataset, aes(x=fraud))+geom_histogram()+theme_minimal()+xlim(0, 10)
gridExtra::grid.arrange(p1, p2, heights=c(3/4, 1/4))
