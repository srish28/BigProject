Patient <- read.csv("SingleFeaturesTable.csv")
Patient$X <- NULL
Patient <- Patient[c(2,1,3,4:ncol(Patient))]

benchmark <- read.csv("randomForest-Benchmark.csv")
testSinglePatient <- read.csv("testSinglePatientTable.csv")
benchmark <- benchmark[order(benchmark$PatientGuid),] 
testSinglePatient <- testSinglePatient[order(testSinglePatient$PatientGuid),]

#####
###separating test and train
Patient <- Patient[Patient$dmIndicator!=-1,]

testPatient <- Patient[Patient$dmIndicator==-1,]
write.csv(trainPatient,"TrainSingleFeaturesTable.csv")
Patient <- read.csv("TrainSingleFeaturesTable.csv")
Patient$X <- NULL
library(caret)


mcorrPatient <- data.matrix(Patient)


#removing near zero and zero
nzv <- nearZeroVar(mcorrPatient)
#near zero and zero removed
nzvPatient <- Patient[, -nzv]

nzv <- nearZeroVar(mcorrPatient, saveMetrics = TRUE, names = TRUE, foreach = FALSE, allowParallel = TRUE)


zv <- nzv[nzv[, "zeroVar"] >0, ]
zvRow <- rownames(zv)
zvId <- match(zvRow, names(Patient))

#zero variance removed
zvPatient <- Patient[, -zvId]


#find correlation in nzv
train <- cor(data.matrix(nzvPatient))
nzvCorr <- c()
j <- 1
i <- 0

y <- c()
while(i <= 1.05){
  print(i)
  y[j] <- i
  corr <- findCorrelation(train, cutoff = i, verbose = FALSE, names = FALSE, exact = FALSE)
  
  i <- i + 0.05
  
  nzvCorr[j] <- ncol(nzvPatient) - length(corr)
  j <- j + 1
}



#find correlation in zv
train <- cor(data.matrix(zvPatient))
zvCorr <- c()
j <- 1
i <- 0

y <- c()
while(i <= 1.05){
  print(i)
  y[j] <- i
  corr <- findCorrelation(train, cutoff = i, verbose = FALSE, names = FALSE, exact = FALSE)
  
  i <- i + 0.05
  
  zvCorr[j] <-ncol(zvPatient) - length(corr)
  j <- j + 1
}


numberOfFeaturesRemaining<- zvCorr
thresholdValue <- y

plot(thresholdValue,numberOfFeaturesRemaining, type = 'o', col = 'red')

numberOfFeaturesRemaining <- nzvCorr
thresholdValue <- y

lines(thresholdValue,numberOfFeaturesRemaining, type = 'o', pch=22, lty=2, col = 'blue')
legend("topleft", c("zvPatient", "nzvPatient"), cex=0.8, col=c( "red", "blue"), lty=1:3, lwd=2, bty="n")

##############################


numberOfFeaturesRemaining<- zvCorr
thresholdValue <- y

plot(thresholdValue,numberOfFeaturesRemaining, type = 'o', col = 'red')
title(main="zv", col.main="red", font.main=4)

numberOfFeaturesRemaining <- nzvCorr
thresholdValue <- y

plot(thresholdValue,numberOfFeaturesRemaining, type = 'o', pch=22, lty=2, col = 'blue')
title(main="nzv", col.main="blue", font.main=4)
#################################

slope <- function(x1,y1,x2,y2) {
  s <- (y2 - y1)/(x2 - x1)
  return(s)
} 

i <- 1
max <- -10000
min <- 10000
sl = c()
while (i < length(nzvCorr)){
  x1 <- y[i]
  y1 <- nzvCorr[i]
  
  x2 <- y[i+1]
  y2 <- nzvCorr[i+1]
  s <- slope(x1,y1,x2,y2)
  sl[i] <- s
  if(s > max)
    max <- s
  
  
  if(s < min)
    min <- s
  
  i <- i + 1
}
nzvS <- sl

d <- c()
i <- 2
while(i <= length(sl)){
  d[i - 1] <- sl[i] - sl[i - 1]
  i <- i + 1
}

nzvD <- d

i <- 1
max <- -10000000
min <- 100000000
sl = c()
while (i < length(zvCorr)){
  x1 <- y[i]
  y1 <- zvCorr[i]
  
  x2 <- y[i+1]
  y2 <- zvCorr[i+1]
  s <- slope(x1,y1,x2,y2)
  sl[i] <- s
  if(s > max)
    max <- s
  
  
  
  if(s < min)
    min <- s
  
  i <- i + 1
}

zvS <- sl

d <- c()
i <- 2
while(i <= length(sl)){
  d[i - 1] <- sl[i] - sl[i - 1]
  i <- i + 1
}

zvD <- d


#nzv Set
train <- cor(data.matrix(nzvPatient))
#Low Correlation
nzvcorr15 <- findCorrelation(train, cutoff = 0.15, verbose = FALSE, names = FALSE, exact = FALSE)

low15NzvPatient <- nzvPatient[, -nzvcorr15]

#High Correlation
nzvcorr95 <- findCorrelation(train, cutoff = 0.95, verbose = FALSE, names = FALSE, exact = FALSE)
high95NzvPatient <- nzvPatient[, -nzvcorr95]

#zv
train <- cor(data.matrix(zvPatient))
#Low Correlation
zvcorr15 <- findCorrelation(train, cutoff = 0.15, verbose = FALSE, names = FALSE, exact = FALSE)
low15ZvPatient <- zvPatient[, -zvcorr15]

#High Correlation
zvcorr95 <- findCorrelation(train, cutoff = 0.95, verbose = FALSE, names = FALSE, exact = FALSE)
high95ZvPatient <- zvPatient[, -zvcorr95]





library(mlbench)
library(FSelector)
options(java.parameters = "-Xmx12g")
library(rJava)


mPatient <- data.matrix(Patient[,2:ncol(Patient)])
mY <- data.matrix(Patient[,1])


cfsResult <- cfs(dmIndicator~., Patient[1:404])

to.remove <-cfsResult
'%ni%' <- Negate('%in%')
cfsPatient <- subset(Patient, select = names(Patient) %ni% to.remove)

write.csv(cfsPatient,"cfsPatient.csv")

rm("%ni%", slope, zvS, zvD, zvRow, zvId, zvcorr90, zvcorr10, zvCorr, y, y1, y2, x1, x2, to.remove, thresholdValue, sl, s, nzvD, nzvS, nzvcorr90, nzvcorr10, nzvCorr, numberOfFeaturesRemoved, nzv, train, zv)

library(lars)

#from some website
lassoFit <- lars(mPatient, mY, type="lasso")
summary(lassoFit)
best_step <- lassoFit$df[which.min(lassoFit$RSS)]
predictions <- predict(lassoFit, mPatient, s=best_step, type="fit")$fit
plot(lassoFit)

rmse <- mean((mY - ifelse(predictions == 0.5, 1, round(predictions)) )^2)
print(rmse)



library(glmnet)
#tryin glmnet

fit <- glmnet(mPatient, mY, alpha = 1)
print(fit)
plot(fit, xvar = 'lambda', label = TRUE)
plot(fit, xvar = 'dev', label = TRUE)

coef.exact = coef(fit, s = 0.07, exact = TRUE)
coef.apprx = coef(fit, s = 0.07, exact = TRUE)
cbind2(coef.exact, coef.apprx)

g <- predict(fit, newx = mPatient, type = "nonzero")


lassoPatient <- Patient[,g$s12]


#####time check

set.seed(223646)
z1 <- unclass(Sys.time())

z2 <- unclass(Sys.time())
elapsed.time.minutes <- round((z2 - z1)/ 60,2)  
cat("\n")
cat("elapsed time - ",round(elapsed.time.minutes,2),"minutes","\n")
#######




SinglePatient <- aggregate(Patient[,4:ncol(Patient)],Patient[,1:3], FUN = max )
write.csv(SinglePatient,"SinglePatientTable.csv")

testPatient$X <- NULL
testPatient$dmIndicator <- NULL
testSinglePatient <- aggregate(testPatient[,3:ncol(testPatient)],testPatient[,1:2], FUN = max )
write.csv(testSinglePatient,"testSinglePatientTable.csv")




library(ROCR)
#ROC Curve
p <- prediction(benchmark[,10], benchmark[,2])
perf <- performance(p, measure="lift", x.measure="rpp")
plot(perf)

