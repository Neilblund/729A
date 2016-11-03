#adapted from: http://www.ats.ucla.edu/stat/stata/faq/logitcatcon.htm - note, this is NOT the method suggested by Ai and Norton 



#setup----
library(foreign)
dataset <-
  read.dta("http://www.ats.ucla.edu/stat/data/logitcatcon.dta")
model <- glm(y ~ f * s, family = "binomial", data = dataset)

newdata <- data.frame(model.matrix(model, data = dataset))
vars <- c("s", "f", "f.s")

#dataset with s = 40 and different values of male or female
maledata <- replace(newdata, (vars), list(40, 0, 40 * 0))
femaledata <- replace(newdata, (vars), list(40, 1, 40 * 1))






#predictions----


#get results using simulation approach

simul <- function(model,
                  data,
                  seed = 123,
                  nreps = 1000) {
  require(mvtnorm)
  set.seed(seed)
  coefs <- rmvnorm(nreps, coef(model), vcov(model))
  
  if (model$family$link == "logit") {
    sim <- rowMeans(plogis(coefs %*% t(data)))
  }
  if (model$family$link == "probit") {
    sim <- rowMeans(pnorm(coefs %*% t(data)))
    
  }
  
  return(cbind("pred" = mean(sim), "se" = sd(sim)))
  
}

#function for getting confidence intervals for a prediction and its standard error

conf <- function(case1, case2, confidence = .975) {
  #take the difference
  effect <- case2[, 1] - case1[, 1]
  effect.se <- sqrt(case1[, 2] ^ 2 + case2[, 2] ^ 2)
  effect.se <- unname(effect.se)
  effect.lb <- effect - effect.se * qnorm(confidence)
  effect.ub <- effect + effect.se * qnorm(confidence)
  return(cbind(
    "effect" = effect,
    "effect.lb" = effect.lb,
    "effect.ub" = effect.ub
  ))
  
}




#getting a point prediction----
sim.male <- simul(model = model, data = maledata)
sim.female <- simul(model = model, data = femaledata)

sim.results <- print(conf(sim.male, sim.female))



# plot effect of gender across multiple values of s----

#prep data
sequence <- seq(from = min(newdata$s),
                to = max(newdata$s),
                by = 1)
male.list <-
  lapply(sequence, function(x)
    replace(newdata, vars, list(x, 0, x * 0)))
female.list <-
  lapply(sequence, function(x)
    replace(newdata, vars, list(x, 1, x * 1)))


malepred.list <- t(sapply(male.list, function(x)
  simul(model, x)))

femalepred.list <- t(sapply(female.list, function(x)
  simul(model, x)))

effects.list <- conf(malepred.list, femalepred.list)



plot(
  sequence,
  effects.list[, "effect"],
  ylim = c(-1, 1),
  ylab = "marginal effect of gender",
  xlab = "S",
  type = "l",
  lty = 1
)

polygon(
  y = c(effects.list[, "effect.ub"], rev(effects.list[, "effect.lb"])),
  x = c(sequence, rev(sequence)),
  col = adjustcolor("grey", alpha.f = 0.2),
  border = "black",
  lty = 2
)

grid()
abline(h = 0, col = 'red')
title(main = "Effect of gender at different values of S")

#add a rug plot showing the actual distribution of the modifying variable
rug(jitter(dataset$s))



#add a boxplot to show a five number summary of S
dat <- summary(dataset$s)
lines(x = c(dat[1], dat[6]),
      y = c(-.75,-.75),
      lty = 1)
points(x = c(dat[1], dat[6]),
       y = c(-.75,-.75),
       pch = 5)
rect(
  xleft = dat[2],
  xright = dat[5],
  ybottom = -.80,
  ytop = -.70,
  col = "white"
)
lines(x = c(dat[3], dat[3]), y = c(-.80,-.70))
points(x = c(dat[4]), y = c(-.75))


  #but this is a little misleading...----
  
  model <- glm(y ~ f + s, family = "binomial", data = dataset)
  newdata <- data.frame(model.matrix(model, data = dataset))
  vars<-c("s", "f")
  
  #for plotting effect of gender across multiple values of s
  sequence <- seq(from = min(newdata$s),
                  to = max(newdata$s),
                  by = 1)
  
  
  male.list <-
    lapply(sequence, function(x)
      replace(newdata, vars, list(x, 0)))
  female.list <-
    lapply(sequence, function(x)
      replace(newdata, vars, list(x, 1)))
  
  
  malepred.list <- t(sapply(male.list, function(x)
    simul(model, x)))
  
  femalepred.list <- t(sapply(female.list, function(x)
    simul(model, x)))
  
  effects.list <- conf(malepred.list, femalepred.list)
  polygon(
    y = c(effects.list[, "effect.ub"], rev(effects.list[, "effect.lb"])),
    x = c(sequence, rev(sequence)),
    col = adjustcolor("blue", alpha.f = 0.4)
  
  )
