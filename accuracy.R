
benchmark <- read.csv("randomForest-Benchmark.csv")
testSinglePatient <- read.csv("testSinglePatientTable.csv")
benchmark <- benchmark[order(benchmark$PatientGuid),] 
testSinglePatient <- testSinglePatient[order(testSinglePatient$PatientGuid),]



cutoff <- 0.52
benchmark$DMIndicator[benchmark$DMIndicator < cutoff] <- 0
benchmark$DMIndicator[benchmark$DMIndicator >= cutoff] <- 1

library(SDMTools)
library(pROC)
#plotting roc curve for low15nzv

low15NzvPatient.pred$aggregate[low15NzvPatient.pred$aggregate < cutoff] <- 0
low15NzvPatient.pred$aggregate[low15NzvPatient.pred$aggregate >= cutoff] <- 1

benchmark <- cbind(benchmark, low15NzvPatient.pred$aggregate)
rmse <- (mean((benchmark[,3] - benchmark[,2])**2))**0.5
confusionMatrix(benchmark[,3], benchmark[,2],  dnn = c("Prediction", "Reference"))

#plotting roc curve for high95nzv
high95NzvPatient.pred$aggregate[high95NzvPatient.pred$aggregate < cutoff] <- 0
high95NzvPatient.pred$aggregate[high95NzvPatient.pred$aggregate >= cutoff] <- 1

benchmark <- cbind(benchmark, high95NzvPatient.pred$aggregate)
rmse <- (mean((benchmark[,4] - benchmark[,2])**2))**0.5
confusionMatrix(benchmark[,4], benchmark[,2],  dnn = c("Prediction", "Reference"))

#plotting roc curve for low15Zv
low15ZvPatient.pred$aggregate[low15ZvPatient.pred$aggregate < cutoff] <- 0
low15ZvPatient.pred$aggregate[low15ZvPatient.pred$aggregate >= cutoff] <- 1

benchmark <- cbind(benchmark, low15ZvPatient.pred$aggregate)
confusionMatrix(benchmark[,5], benchmark[,2],  dnn = c("Prediction", "Reference"))

rmse <- (mean((benchmark[,5] - benchmark[,2])**2))**0.5


#plotting roc curve for high95Zv
high95ZvPatient.pred$aggregate[high95ZvPatient.pred$aggregate < cutoff] <- 0
high95ZvPatient.pred$aggregate[high95ZvPatient.pred$aggregate >= cutoff] <- 1

benchmark <- cbind(benchmark, high95ZvPatient.pred$aggregate)

rmse <- (mean((benchmark[,6] - benchmark[,2])**2))**0.5
confusionMatrix(benchmark[,6], benchmark[,2],  dnn = c("Prediction", "Reference"))

#plotting roc curve for cfs
cfsPatient.pred$aggregate[cfsPatient.pred$aggregate < cutoff] <- 0
cfsPatient.pred$aggregate[cfsPatient.pred$aggregate >= cutoff] <- 1

benchmark <- cbind(benchmark, cfsPatient.pred$aggregate)
rmse <- (mean((benchmark[,2] - benchmark[,7])**2))**0.5

confusionMatrix(benchmark[,7], benchmark[,2],  dnn = c("Prediction", "Reference"))

#plotting roc curve for lasso
lassoPatient.pred$aggregate[lassoPatient.pred$aggregate < cutoff] <- 0
lassoPatient.pred$aggregate[lassoPatient.pred$aggregate >= cutoff] <- 1

benchmark <- cbind(benchmark, lassoPatient.pred$aggregate)
rmse <- (mean((benchmark[,2] - benchmark[,8])**2))**0.5

confusionMatrix(benchmark[,8], benchmark[,2],  dnn = c("Prediction", "Reference"))
#Patient
Patient.pred$aggregate[Patient.pred$aggregate < cutoff] <- 0
Patient.pred$aggregate[Patient.pred$aggregate >= cutoff] <- 1

benchmark <- cbind(benchmark, Patient.pred$aggregate)
rmse <- (mean((benchmark[,9] - benchmark[,2])**2))**0.5
confusionMatrix(benchmark[,9], benchmark[,2],  dnn = c("Prediction", "Reference"))

#PatientRF
PatientRFdata.pred$aggregate[PatientRFdata.pred$aggregate < cutoff] <- 0
PatientRFdata.pred$aggregate[PatientRFdata.pred$aggregate >= cutoff] <- 1


benchmark <- cbind(benchmark, PatientRFdata.pred$aggregate)
rmse <- (mean((benchmark[,10] - benchmark[,2])**2))**0.5
confusionMatrix(benchmark[,10], benchmark[,2],  dnn = c("Prediction", "Reference"))
