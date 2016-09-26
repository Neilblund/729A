#generating some data----
 n <- 1000
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  X <- cbind(1, x1, x2)
  b <- c(1, -.5, .5)
  p <- pnorm(X%*%b)
  y <- rbinom(n, 1, p)
  model <- glm(y ~ x1 + x2, family = "binomial"(link="probit"))

#predictions----

#with x1 and x2 set at their mean
meanx1<-mean(x1)
meanx2<-mean(x2)
pnorm(model$coefficients[1]+
    meanx1*model$coefficients[2]+
    meanx2*model$coefficients[3])

#with x1 at its 25%th percentile and x2 at its mean

q.x1<-quantile(x1, .25)

pnorm(model$coefficients[1]+q.x1*model$coefficients[2]+meanx2*model$coefficients[3])

#alternatively using the predict command----

#create a hypothetical data set with just one row containing hypothetical values for x1 and x2. 
scenario<-data.frame("x1"=q.x1, "x2"=meanx2, row.names = "prediction")

#use the previous data as "newdata" in the predict command
predict(model, newdata = scenario, type="response")

#plotting across multiple values


scenario<-data.frame("x1"=x1, "x2"=mean(x2))
prediction<-predict(model, newdata = scenario, type="response")
plot(x1, prediction)
