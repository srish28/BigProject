testSinglePatient$X <- NULL
library(doParallel)
library(foreach)
library(randomForest)
registerDoParallel(cores=4)
Patient.rf <- foreach(ntree = rep(45, 3), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(Patient[,4:ncol(Patient)], Patient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#random forest on cfs
cfsPatient.rf <- foreach(ntree = rep(38, 3), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(cfsPatient[,4:ncol(cfsPatient)], cfsPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#random forest on lasso
lassoPatient.rf <- foreach(ntree = rep(1, 2), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(lassoPatient, Patient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)


r <- Patient.rf

a <- importance(Patient.rf, scale = TRUE, type = 1)
a<-data.frame(a)
a$Vars<-row.names(a)
a <- a[order(a$X.IncMSE, decreasing = TRUE),][1:40,]

PatientRFdata <- Patient[,row.names(a)]



library(randomForest)
#random forest selection
#nzv correlated features removed dataset
#low
set.seed(4353)
#low15NzvPatient.rf <- randomForest(dmIndicator~., data = data.matrix(low15NzvPatient), ntree = 35, keep.forest = FALSE, importance = TRUE)
low15NzvPatient.rf <- foreach(ntree = rep(3,4), .combine = combine, .packages = 'randomForest') %dopar% randomForest(low15NzvPatient[,4:ncol(low15NzvPatient)], low15NzvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)
#high
#high95NzvPatient.rf <- randomForest(dmIndicator~., data = data.matrix(high95NzvPatient), ntree = 80, keep.forest = TRUE, importance = TRUE)
high95NzvPatient.rf <- foreach(ntree = rep(5, 4), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(high95NzvPatient[,4:ncol(high95NzvPatient)], high95NzvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#zv correlated features removed dataset
#low
set.seed(9856)
low15ZvPatient.rf <- foreach(ntree = rep(25,4), .combine = combine, .multicombine=TRUE, .packages = 'randomForest') %dopar% randomForest(low15ZvPatient[,4:ncol(low15ZvPatient)], low15ZvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)
#low15ZvPatient.rf <- randomForest(dmIndicator~., data = data.matrix(low15ZvPatient) , ntree = 350, keep.forest = FALSE, importance = TRUE)
#high
#high95ZvPatient.rf <- randomForest(dmIndicator~., data = data.matrix(high95NzvPatient), ntree = 500, keep.forest = FALSE, importance = TRUE)
high95ZvPatient.rf <- foreach(ntree = rep(31,4), .combine = combine, .multicombine=TRUE, .packages = 'randomForest') %dopar% randomForest(high95ZvPatient[,4:ncol(high95ZvPatient)], high95ZvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#FS with rfimportance
PatientRFdata.rf <- foreach(ntree = rep(3,4), .combine = combine, .multicombine=TRUE, .packages = 'randomForest') %dopar% randomForest(PatientRFdata, Patient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)
#random forest on above data

#model creation for classification
library(doParallel)
library(foreach)
library(randomForest)
registerDoParallel(cores=4)
CPatient.rf <- foreach(ntree = rep(5, 4), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(Patient[,4:ncol(Patient)], Patient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#random forest on cfs
CcfsPatient.rf <- foreach(ntree = rep(5, 4), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(cfsPatient[,4:ncol(cfsPatient)], cfsPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#random forest on lasso
ClassoPatient.rf <- foreach(ntree = rep(1, 3), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(lassoPatient, Patient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)


Clow15NzvPatient.rf <- foreach(ntree = rep(2,3), .combine = combine, .packages = 'randomForest') %dopar% randomForest(low15NzvPatient[,4:ncol(low15NzvPatient)], low15NzvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)
#high

Chigh95NzvPatient.rf <- foreach(ntree = rep(2, 4), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(high95NzvPatient[,4:ncol(high95NzvPatient)], high95NzvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#zv correlated feat#RFS for classification
library(doParallel)
library(foreach)
library(randomForest)
registerDoParallel(cores=4)
CPatient.rf <- foreach(ntree = rep(5, 4), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(Patient[,4:ncol(Patient)], Patient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#random forest on cfs
CcfsPatient.rf <- foreach(ntree = rep(5, 4), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(cfsPatient[,4:ncol(cfsPatient)], cfsPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#random forest on lasso
ClassoPatient.rf <- foreach(ntree = rep(1, 3), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(lassoPatient, Patient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)


Clow15NzvPatient.rf <- foreach(ntree = rep(2,3), .combine = combine, .packages = 'randomForest') %dopar% randomForest(low15NzvPatient[,4:ncol(low15NzvPatient)], low15NzvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)
#high

Chigh95NzvPatient.rf <- foreach(ntree = rep(2, 4), .combine = combine, .multicombine=TRUE, .packages= 'randomForest') %dopar% randomForest(high95NzvPatient[,4:ncol(high95NzvPatient)], high95NzvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#zv correlated features removed dataset
#low

Clow15ZvPatient.rf <- foreach(ntree = rep(6,3), .combine = combine, .multicombine=TRUE, .packages = 'randomForest') %dopar% randomForest(low15ZvPatient[,4:ncol(low15ZvPatient)], low15ZvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#high

Chigh95ZvPatient.rf <- foreach(ntree = rep(5,4), .combine = combine, .multicombine=TRUE, .packages = 'randomForest') %dopar% randomForest(high95ZvPatient[,4:ncol(high95ZvPatient)], high95ZvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)


Cr <- CPatient.rf

a <- importance(CPatient.rf, scale = TRUE, type = 1)
a<-data.frame(a)
a$Vars<-row.names(a)
a <- a[order(a$X.IncMSE, decreasing = TRUE),][1:40,]

CPatientRFdata <- Patient[,row.names(a)]
CPatientRFdata.rf <- foreach(ntree = rep(2,3), .combine = combine, .multicombine=TRUE, .packages = 'randomForest') %dopar% randomForest(PatientRFdata, Patient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#low

Clow15ZvPatient.rf <- foreach(ntree = rep(6,3), .combine = combine, .multicombine=TRUE, .packages = 'randomForest') %dopar% randomForest(low15ZvPatient[,4:ncol(low15ZvPatient)], low15ZvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)

#high

Chigh95ZvPatient.rf <- foreach(ntree = rep(5,4), .combine = combine, .multicombine=TRUE, .packages = 'randomForest') %dopar% randomForest(high95ZvPatient[,4:ncol(high95ZvPatient)], high95ZvPatient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)


Cr <- CPatient.rf

a <- importance(CPatient.rf, scale = TRUE, type = 1)
a<-data.frame(a)
a$Vars<-row.names(a)
a <- a[order(a$X.IncMSE, decreasing = TRUE),][1:40,]

CPatientRFdata <- Patient[,row.names(a)]
CPatientRFdata.rf <- foreach(ntree = rep(2,3), .combine = combine, .multicombine=TRUE, .packages = 'randomForest') %dopar% randomForest(PatientRFdata, Patient[,1], ntree = ntree, keep.forest = TRUE, importance = TRUE)
#random forest on above data


#Prediction
set.seed(4314)
low15NzvPatient.pred <- predict(Clow15NzvPatient.rf, testSinglePatient, type = 'response', predict.all = TRUE)
low15ZvPatient.pred <- predict(Clow15ZvPatient.rf, testSinglePatient, type = 'response', predict.all = TRUE)

high95NzvPatient.pred <- predict(Chigh95NzvPatient.rf, testSinglePatient, type = 'response', predict.all = TRUE)
high95ZvPatient.pred <- predict(Chigh95ZvPatient.rf, testSinglePatient, type = 'response', predict.all = TRUE)

cfsPatient.pred <- predict(CcfsPatient.rf, testSinglePatient, type = 'response', predict.all = TRUE)

lassoPatient.pred <- predict(ClassoPatient.rf, testSinglePatient, type = 'response', predict.all = TRUE)

Patient.pred <- predict(CPatient.rf, testSinglePatient, type = 'response', predict.all = TRUE)

PatientRFdata.pred <- predict(CPatientRFdata.rf, testSinglePatient, type = 'response', predict.all = TRUE)

a <- as.data.frame(PatientRFdata.pred$aggregate)

library(ROCR)
#ROC Curve
trainPatientRFdata.pred <- predict(CPatientRFdata.rf, SinglePatient[2:ncol(SinglePatient)], type = 'response', predict.all = TRUE)


cutoff <- 0.47
a <- as.data.frame(trainPatientRFdata.pred$aggregate)

a$`trainPatientRFdata.pred$aggregate`[a$`trainPatientRFdata.pred$aggregate`< cutoff] <- 0
a$`trainPatientRFdata.pred$aggregate`[a$`trainPatientRFdata.pred$aggregate`>= cutoff] <- 1
confusionMatrix( a$`trainPatientRFdata.pred$aggregate`, SinglePatient$dmIndicator)


p <- prediction(a, SinglePatient$dmIndicator)
perf <- performance(p, measure="tpr", x.measure="fpr")
plot(perf)



library(glmnet)
#LOGISTIC

#RIDGE

benchmark$DMIndicator[benchmark$DMIndicator < cutoff] <- 0
benchmark$DMIndicator[benchmark$DMIndicator >= cutoff] <- 1


cutoff <- 0.38
#RIDGE regression
#all 404 variables
ridgeFit <- glmnet(mPatient, mY, family = "gaussian", alpha = 0, lambda = 0.00001)
predictRidge <- predict(ridgeFit, newx = mPatient, type = "link")

rmse <- mean((benchmark[,2] - predictRidge)^2)
print(rmse)
predictRidge[predictRidge < cutoff] <- 0
predictRidge[predictRidge >= cutoff] <- 1
confusionMatrix(predictRidge, mY,  dnn = c("Prediction", "Reference"))

#PatientRF
x <- data.matrix(PatientRFdata)
ridgeFit <- glmnet(x, mY, family = "gaussian", alpha = 0, lambda = 0.001)
a <- testSinglePatient[, colnames(PatientRFdata)]
predictRidge <- predict(ridgeFit, newx = data.matrix(a) , type = "link")
predictRidge[predictRidge < cutoff] <- 0
predictRidge[predictRidge >= cutoff] <- 1

rmse <- mean((benchmark[,2] - predictRidge)^2)
print(rmse)
confusionMatrix(predictRidge, benchmark[,2],  dnn = c("Prediction", "Reference"))

#lasso
ridgeFit <- glmnet(data.matrix(lassoPatient), mY, family = "gaussian", alpha = 0, lambda = 0.001)
a <- testSinglePatient[, colnames(lassoPatient)]
predictRidge <- predict(ridgeFit, newx = data.matrix(a) , type = "link")
predictRidge[predictRidge < cutoff] <- 0
predictRidge[predictRidge >= cutoff] <- 1

confusionMatrix(predictRidge, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictRidge)^2)
print(rmse)

#CFS
ridgeFit <- glmnet(data.matrix(cfsPatient[2:ncol(cfsPatient)]), mY, family = "gaussian", alpha = 0, lambda = 0.001)
a <- testSinglePatient[, colnames(cfsPatient[2:ncol(cfsPatient)])]
predictRidge <- predict(ridgeFit, newx = data.matrix(a) , type = "link")
predictRidge[predictRidge < cutoff] <- 0
predictRidge[predictRidge >= cutoff] <- 1

confusionMatrix(predictRidge, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictRidge)^2)
print(rmse)

#correlations
#low15NzvPatient
ridgeFit <- glmnet(data.matrix(low15NzvPatient[2:ncol(low15NzvPatient)]), mY, family = "gaussian", alpha = 0, lambda = 0.001)
a <- testSinglePatient[, colnames(low15NzvPatient[2:ncol(low15NzvPatient)])]
predictRidge <- predict(ridgeFit, newx = data.matrix(a) , type = "link")
predictRidge[predictRidge < cutoff] <- 0
predictRidge[predictRidge >= cutoff] <- 1

confusionMatrix(predictRidge, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictRidge)^2)
print(rmse)

#high95NzvPatient
ridgeFit <- glmnet(data.matrix(high95NzvPatient[2:ncol(high95NzvPatient)]), mY, family = "gaussian", alpha = 0, lambda = 0.001)
a <- testSinglePatient[, colnames(high95NzvPatient[2:ncol(high95NzvPatient)])]
predictRidge <- predict(ridgeFit, newx = data.matrix(a) , type = "link")
predictRidge[predictRidge < cutoff] <- 0
predictRidge[predictRidge >= cutoff] <- 1

confusionMatrix(predictRidge, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictRidge)^2)
print(rmse)

#low15ZvPatient
ridgeFit <- glmnet(data.matrix(low15ZvPatient[2:ncol(low15ZvPatient)]), mY, family = "gaussian", alpha = 0, lambda = 0.001)
a <- testSinglePatient[, colnames(low15ZvPatient[2:ncol(low15ZvPatient)])]
predictRidge <- predict(ridgeFit, newx = data.matrix(a) , type = "link")
predictRidge[predictRidge < cutoff] <- 0
predictRidge[predictRidge >= cutoff] <- 1

confusionMatrix(predictRidge, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictRidge)^2)
print(rmse)

#high95ZvPatient
ridgeFit <- glmnet(data.matrix(high95ZvPatient[2:ncol(high95ZvPatient)]), mY, family = "gaussian", alpha = 0, lambda = 0.001)
a <- testSinglePatient[, colnames(high95ZvPatient[2:ncol(high95ZvPatient)])]
predictRidge <- predict(ridgeFit, newx = data.matrix(a) , type = "link")
predictRidge[predictRidge < cutoff] <- 0
predictRidge[predictRidge >= cutoff] <- 1

confusionMatrix(predictRidge, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictRidge)^2)
print(rmse)


cutoff <- 0.43
#LASSO regression
#all 404 variables
newx = data.matrix(testSinglePatient)
lassoFit <- glmnet(mPatient, mY, family = "gaussian", alpha = 1, lambda = 0.00001)
predictLasso <- predict(lassoFit, mPatient, type = "link")

rmse <- mean((benchmark[,2] - predictLasso)^2)
print(rmse)
predictLasso[predictLasso < cutoff] <- 0
predictLasso[predictLasso >= cutoff] <- 1
confusionMatrix(predictLasso, mY,  dnn = c("Prediction", "Reference"))

#PatientRFdata
x <- data.matrix(PatientRFdata)
lassoFit <- glmnet(x, mY, family = "gaussian", alpha = 1, lambda = 0.001)
a <- testSinglePatient[, colnames(PatientRFdata)]
predictLasso <- predict(lassoFit, newx = data.matrix(a) , type = "link")
predictLasso[predictLasso < cutoff] <- 0
predictLasso[predictLasso >= cutoff] <- 1

rmse <- mean((benchmark[,2] - predictLasso)^2)
print(rmse)
confusionMatrix(predictLasso, benchmark[,2],  dnn = c("Prediction", "Reference"))

#lasso
lassoFit <- glmnet(data.matrix(lassoPatient), mY, family = "gaussian", alpha = 1, lambda = 0.001)
a <- testSinglePatient[, colnames(lassoPatient)]
predictLasso <- predict(lassoFit, newx = data.matrix(a) , type = "link")
predictLasso[predictLasso < cutoff] <- 0
predictLasso[predictLasso >= cutoff] <- 1
confusionMatrix(predictLasso, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictLasso)^2)
print(rmse)

#CFS
lassoFit <- glmnet(data.matrix(cfsPatient[2:ncol(cfsPatient)]), mY, family = "gaussian", alpha = 1, lambda = 0.001)
a <- testSinglePatient[, colnames(cfsPatient[2:ncol(cfsPatient)])]
predictLasso <- predict(lassoFit, newx = data.matrix(a) , type = "link")
predictLasso[predictLasso < cutoff] <- 0
predictLasso[predictLasso >= cutoff] <- 1
confusionMatrix(predictLasso, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictLasso)^2)
print(rmse)

#low15NzvPatient
lassoFit <- glmnet(data.matrix(low15NzvPatient[2:ncol(low15NzvPatient)]), mY, family = "gaussian", alpha = 1, lambda = 0.001)
a <- testSinglePatient[, colnames(low15NzvPatient[2:ncol(low15NzvPatient)])]
predictLasso <- predict(lassoFit, newx = data.matrix(a) , type = "link")
predictLasso[predictLasso < cutoff] <- 0
predictLasso[predictLasso >= cutoff] <- 1
confusionMatrix(predictLasso, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictLasso)^2)
print(rmse)

#high95NzvPatient
lassoFit <- glmnet(data.matrix(high95NzvPatient[2:ncol(high95NzvPatient)]), mY, family = "gaussian", alpha = 1, lambda = 0.001)
a <- testSinglePatient[, colnames(high95NzvPatient[2:ncol(high95NzvPatient)])]
predictLasso <- predict(lassoFit, newx = data.matrix(a) , type = "link")
predictLasso[predictLasso < cutoff] <- 0
predictLasso[predictLasso >= cutoff] <- 1
confusionMatrix(predictLasso, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictLasso)^2)
print(rmse)

#low15ZvPatient
lassoFit <- glmnet(data.matrix(low15ZvPatient[2:ncol(low15ZvPatient)]), mY, family = "gaussian", alpha = 1, lambda = 0.001)
a <- testSinglePatient[, colnames(low15ZvPatient[2:ncol(low15ZvPatient)])]
predictLasso <- predict(lassoFit, newx = data.matrix(a) , type = "link")
predictLasso[predictLasso < cutoff] <- 0
predictLasso[predictLasso >= cutoff] <- 1
confusionMatrix(predictLasso, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictLasso)^2)
print(rmse)

#high95ZvPatient
lassoFit <- glmnet(data.matrix(high95ZvPatient[2:ncol(high95ZvPatient)]), mY, family = "gaussian", alpha = 1, lambda = 0.001)
a <- testSinglePatient[, colnames(high95ZvPatient[2:ncol(high95ZvPatient)])]
predictLasso <- predict(lassoFit, newx = data.matrix(a) , type = "link")
predictLasso[predictLasso < cutoff] <- 0
predictLasso[predictLasso >= cutoff] <- 1
confusionMatrix(predictLasso, benchmark[,2],  dnn = c("Prediction", "Reference"))
rmse <- mean((benchmark[,2] - predictLasso)^2)
print(rmse)


library(ROCR)
#ROC Curve
p <- prediction(predictLasso, benchmark[,2])
perf <- performance(p, measure="lift", x.measure="rpp")
plot(perf)


library(pROC)
r <- roc(as.factor(benchmark[,2]), as.numeric(predictRidge))
r <- smooth(r, n = 512)
plot(r, add = TRUE)


#LIFT

