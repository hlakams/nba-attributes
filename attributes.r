# remove residual vars
rm(list=ls())
# necessary libraries
library(randomForest)
library(gbm)
library(e1071)
# random seed
set.seed(2021)

# loading in dataset
bbdata = read.csv("nbaallelo_mod_short.csv")
bbdata.names = names(bbdata)
bbdata = bbdata[-c(1:4,9:10)]

# train + test sets
bbdata.train = sample(1:nrow(bbdata), nrow(bbdata)/2)
bbdata.test=bbdata[-bbdata.train,"forecast"]

# random forest
rf.bbdata=randomForest(forecast~.,data=bbdata,subset=bbdata.train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.bbdata,newdata=bbdata[-bbdata.train,])
mean((yhat.rf-bbdata.test)^2)
importance(rf.bbdata)
varImpPlot(rf.bbdata)

# boosting
boost.bbdata=gbm(forecast~.,data=bbdata[bbdata.train,],distribution="gaussian",n.trees=6000,shrinkage=0.1,interaction.depth=4)
summary(boost.bbdata)
yhat.boost=predict(boost.bbdata,newdata=bbdata[-bbdata.train,],n.trees=6000)
mean((yhat.boost-bbdata.test)^2)

# re-load dataset for SVM (tri-variate)
bbdata2 = read.csv("nbaallelo_2007_playoffs.csv")
bbdata2.names = names(bbdata2)
bbdata2 = bbdata2[-c(1:7,10:15)]

# train + test sets
bbdata2.train = sample(1:nrow(bbdata2), nrow(bbdata2)/2)
bbdata2.test=bbdata2[-bbdata2.train,"forecast"]

# svm
bbdata2.svm=svm(as.factor(forecast) ~ ., data=bbdata2[bbdata2.train,], kernel="radial",  cost=1)
plot(bbdata2.svm, bbdata2[bbdata2.train,])
summary(bbdata2.svm)
