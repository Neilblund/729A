zika<-read.csv("~zika.csv", stringsAsFactors = FALSE)

library(quanteda)
library(glmnet)
library(caret)
zika.dfm<-dfm(zika$texts, stem = TRUE, ignoredFeatures = stopwords("english"))
top.features<-topfeatures(zika.dfm, n=length(features(zika.dfm)), decreasing=TRUE)
occurence<-docfreq(zika.dfm, scheme="count", threshold=0)
occurence<-occurence/ndoc(zika.dfm)
rems<-names(occurence)[which(occurence<=.05)]
zika.dfm<-tfidf(zika.dfm, normalize=TRUE, scheme="inverse")
zika.dfm<-removeFeatures(zika.dfm, rems)

zika.dfm$lrcode<-zika$leftright




inTraining <- createDataPartition(zika.dfm$lrcode, p = .75, list = FALSE)
training <- zika.dfm[ inTraining,]
testing  <- zika.dfm[-inTraining,]
set.seed(3456)
trainX <-training[,-which(colnames(training)=="lrcode")]        # Pull out the variables for training
testX<-testing[,-which(colnames(training)=="lrcode")] 
Y<-make.names(training$lrcode)
testY<-make.names(testing$lrcode)

objControl <- trainControl(method='cv', number=3, returnResamp='none')
objModel <- train(x=trainX, 
                  y=Y, method='glmnet',  metric = "Kappa", trControl=objControl)



pred.objModel <- predict(objModel, testX)
confusionMatrix(pred.objModel, testY)
prop.table(table(pred.svm, testY),2)
