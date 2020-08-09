# Libraries extablished
# require(gridExtra)
# require(grid)
# require(doBy)
# require(ROCR)


#loocv remove observations
#intermediate steps
#transformations (multiplying, ratios, random number operations, exp, log)
#make 1's equal to size of 0 (remove bias)

#//////////////////////////////////////////////////////////////////////////////////// Functions

# confusion matrix function provided by  professor Bhat in ctg.R
myconfusion = function(pfit, y)
{
  confusion = matrix(nrow = 2, ncol = 2)
  actual0 = which(y == 0)
  actual1 = which(y == 1)
  confusion[1, 1] = sum(pfit[actual1] == 1)
  confusion[2, 1] = sum(pfit[actual0] == 1)
  confusion[1, 2] = sum(pfit[actual1] == 0)
  confusion[2, 2] = sum(pfit[actual0] == 0)
  return(confusion)
}

normalize = function(data)
{
  data = (data - min[data])/(max(data) - min(data))
  return(data)
}

# A function tailored specifically for logistic regression provided by professor Bhat
mythresh <- function(q)
{
  med = median(q)
  q[which(q >= med)] = 1
  q[which(q < med)] = 0
  return(q)
}

# Function that finds the minimum value of any given Mean_Squared_Error
findMin = function(Mean_Squared_Error) {
  # Establishing the minimum value as a random index
  minVal = Mean_Squared_Error[1]
  for (j in 1:length(Mean_Squared_Error)) {
    # If an index is smaller than the first index
    if (Mean_Squared_Error[j] < minVal) {
      # The index value is equal to the minimum value
      minVal = Mean_Squared_Error[j]
    }
  }
  return(minVal)
}

#////////////////////////////////////////////////////////////////////////////////////

# This clears the memory
rm(list = ls(all = TRUE))
# Loads the data onto the global environment
load('parkdata.RData')
load("terr.rdata")
options(warn = -1)
options(digits = 15)
set.seed(42)
# Get rid of the first four columns because they are irrelevant to what we are trying to do
# parkdata = parkdata[,-c(1:4)]


#//////////////////////////////////////////////////////////////////////////////////// Exploring Data
#Find the covariance of all the variables and determine which ones correspond the most, then find the correlation to solidify
#that the data selected is the most corresponding

# A vector that stores all the correlation values based on the columns
cor_vals = c(1:(ncol(parkdata)))

#Check for all the rows and columns in terms of seeing their relationship
# correlatedVariables = parkdata[c(16,13,10,8,11,12,11,14,10,6)]
# MCV = c(16,13,10,8,11,12,11,14,10,6)
# correlationTest = matrix(0, 10, 10)
# for(i in c(1:11)){
#   for(j in c(1:11)){
#     if(i != j){
#       tempCorrelation = cor(parkdata[[MCV[i]]], parkdata[[MCV[j]]])
#       correlationTest[i - 1, j - 1] = tempCorrelation
#     }
#   }
# }

# Modify correlation test values in order to get max and min correlations
# correlationMax = max(correlationTest)

# Making a table for all of the given correlation tests
# grid.table(correlationMax)

#Kernel Denisty Estimation Graphing of the variables that correspond the most with eachother

age <- parkdata$Shimmer.APQ11
kernel_density = density(parkdata$Shimmer.dB., parkdata$Shimmer.APQ11,  n = 60)
plot(kernel_density,
     main = "Kernel Density of Most Correlated Parameters",
     ylab = "Shimmer APQ11",
     xlab = "Shimmer DB")

# par(mfrow=c(3,4))
# for(element in colnames(newData)){
#   hist(newData[[element]], breaks = 20, main = paste(element, " broken into 20 buckets", sep = ""), xlab = paste(element))
# }

#//////////////////////////////////////////////////////////////////////////////////// Modeling and Methods

# Find a logistic regression model that takes the variables with the most dependence
n = length(rownames(parkdata))
samplesize = seq(n)
trainind = sample(samplesize, n * 0.8, replace = F)

# 75% of the parkdata dataset
xtrain = parkdata[trainind,]
# 25% of the parkdata dataset
xtest = parkdata[-trainind,]

ytrain = (parkdata$total_UPDRS)[trainind]
ytest = (parkdata$total_UPDRS)[-trainind]

#This is the linear model that has the index as the determinant for what column
#Testing two with highest correlation
linear_model = glm(ytrain ~ . - subject. - age - sex - test_time - 
                     motor_UPDRS - total_UPDRS, data = xtrain)
# Test Mean_Squared_Error in sample
Mean_Squared_Error = mean((ytest - predict(linear_model, newdata = xtest)) ^ 2)

# LOOCV Cross Validation
# exclude = c(290,1088,2017,2041,2674,2675,2679,2684,2685,2688,2695,2696,2697,2702,2706,2717,2718,2721,2722,2723,2729,2730,2740,2743,2744,2745,2746,2752,2767,2970,2988,2990,3102,3642,3648,3656,3659,3664,4064)
# parkdata = parkdata[-exclude, ]
# allmodels = list(NULL)
# numBuckets = 10
# n = nrow(parkdata) / numBuckets
# indiverrors = numeric(length=n)
# folds <- cut(seq(1,nrow(parkdata)),breaks=numBuckets,labels=FALSE)
# # thresh = which(parkdata$total_UPDRS >= 33.50)
# # parkdata$total_UPDRS[thresh] = 1
# # parkdata$total_UPDRS[-thresh] = 0
# 
# for (i in c(1:n)){
#   testIndices = which(folds==i,arr.ind=TRUE)
#   testData <- parkdata[testIndices, ]
#   trainData <- parkdata[-testIndices, ]
#   allmodels[[i]] = glm(formula = total_UPDRS ~ . - subject. - age - sex - test_time - 
#                         motor_UPDRS - total_UPDRS - Shimmer - Shimmer.APQ5 - Jitter.RAP - 
#                         Jitter.PPQ5 - Jitter.DDP, data = parkdata[-i, ])
#   trueTotalUPDRS = parkdata[-i, ]$total_UPDRS
#   predTotalUPDRS = predict(allmodels[[i]],newdata=parkdata[i,])
#   indiverrors[i] = (trueTotalUPDRS - predTotalUPDRS)^2
# }
# 
# mean(indiverrors)
# plot(linear_model)

# Find nonlinear transformations of predictors

# #Testing two with highest correlation
# logit_model = glm(ytrain ~ ytrain, data = xtrain)
# 
# # Test Mean_Squared_Error in sample
# Mean_Squared_Error_logit = mean((ytest - predict(logit_model, newdata = xtest)) ^ 2)

#//////////////////////////////////////////////////////////////////////////////////// Results

#Find singular variable that has the most impact on both UPDRS values.
errorResultsInSample = numeric(length = length(parkdata))
errorResultsOutSample = numeric(length = length(parkdata))
i = 1

for (element in colnames(parkdata)){
  mylm = glm(parkdata$motor_UPDRS ~ parkdata[[element]], data = xtrain)
  errorResultsInSample[i] = mean((ytrain - predict(mylm))^2)
  errorResultsOutSample[i] = mean((ytest - predict(mylm, newdata = xtest))^2)
  i = i + 1
}

errorResultsInSample = errorResultsInSample[-c(5,6)]
errorResultsOutSample = errorResultsOutSample[-c(5,6)]

# Console testing with models
# badlm = lm(ytrain ~ . -subject. -age -sex -test_time -motor_UPDRS -total_UPDRS -Shimmer -Shimmer.APQ5 -Jitter.RAP -Jitter.PPQ5 -Jitter.DDP, data = xtrain)
# summary(badlm)
# mean((ytest - predict(badlm, newdata = xtest)) ^ 2)

# Make a confusion matrix that determines whether or not the model did well in predicting parkinsons

#thresholding and logisticalling the data
newdata = parkdata
thresh = which(newdata$total_UPDRS > mean(newdata$total_UPDRS))
newdata$total_UPDRS[thresh] = 1
newdata$total_UPDRS[-thresh] = 0
while(sum(newdata$total_UPDRS == 1) < sum(newdata$total_UPDRS == 0)){
  sampledRow = sample(thresh, size = 1, replace = TRUE)
  newdata = rbind(newdata, newdata[sampledRow, ])
}

n = length(rownames(newdata))
samplesize = seq(n)
trainind = sample(samplesize, n * 0.8, replace = F)

# 75% of the parkdata dataset
xtrain = newdata[trainind,]
# 25% of the parkdata dataset
xtest = newdata[-trainind,]

ytrain = (newdata$total_UPDRS)[trainind]
ytest = (newdata$total_UPDRS)[-trainind]

loglm = glm(ytrain ~ (DFA / RPDE) * (HNR / NHR), data = xtrain, family = binomial())

a = predict(tlm, newdata = xtest)
raw = 1/(1 + exp(-a))
thresh = 0.03053883

indexROC = which(raw > thresh)
rawROC = raw
rawROC[indexROC] = 1
rawROC[-indexROC] = 0

index50 = which(raw > .5)
raw50 = raw
raw50[index50] = 1
raw50[-index50] = 0

matrixROC = myconfusion(rawROC, ytest)
matrix50 = myconfusion(raw50, ytest)

(matrixROC[1,1] + matrixROC[2,2]) / length(rawROC)
(matrix50[1,1] + matrix50[2,2]) / length(raw50)

# #Training data
# pfit = mythresh(predict(model1, type = 'response'))
# conf_matrix = myconfusion(pfit, qtest)
# n = length(qtrain)
# print(100 * (conf[1, 1] + conf[2, 2]) / n)
# 
# #Testing data
# pfit = mythresh(predict(model1, newdata = mtest, type = 'response'))
# conf_matrix = myconfusion(pfit, qtest)
# n = length(qtrain)
# print(100 * (conf[1, 1] + conf[2, 2]) / n)
# # roc = performance(predTotalUPDRS,"tpr","fpr")
# plot(roc, colorize = T, lwd = 2)
# abline(a = 0, b = 1)
# print(conf_matrix)

cvmatrix = matrix(data = 0, nrow = 5, ncol = 4)
datamatrix = matrix(data = 0, nrow = 5, ncol = 4)

parkdata = parkdata[-terr, ]
parkdata = normalize(parkdata)

# CV for 10 fold
allmodels1 = list(NULL)
allmodels2 = list(NULL)
allmodels3 = list(NULL)
numBuckets = 10
n = numBuckets
indiverrors1 = numeric(length=n)
indiverrors2 = numeric(length=n)
indiverrors3 = numeric(length=n)

indiverrors1x = numeric(length=n)
indiverrors2x = numeric(length=n)
indiverrors3x = numeric(length=n)
folds <- cut(seq(1,nrow(parkdata)),breaks=numBuckets,labels=FALSE)

for (i in c(1:n)){
  testIndices = which(folds==i,arr.ind=TRUE)
  # testData <- parkdata[testIndices, ]
  # trainData <- parkdata[-testIndices, ]
  
  allmodels1[[i]] = lm(formula = total_UPDRS ~ . - subject. - age - sex - test_time - 
                          motor_UPDRS - total_UPDRS - Shimmer - Shimmer.APQ5, data = parkdata[-testIndices, ])
  
  allmodels2[[i]] = lm(formula = total_UPDRS ~ RPDE + DFA + Jitter.DDP + Shimmer.APQ11, data = parkdata[-testIndices, ])
  
  allmodels3[[i]] = lm(formula = total_UPDRS ~ (DFA / RPDE) * (HNR / NHR), data = parkdata[-testIndices, ])
  
  trueTotalUPDRS1 = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS1 = predict(allmodels1[[i]],newdata=parkdata[testIndices,])
  
  trueTotalUPDRS2 = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS2 = predict(allmodels2[[i]],newdata=parkdata[testIndices,])
  
  trueTotalUPDRS3 = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS3 = predict(allmodels3[[i]],newdata=parkdata[testIndices,])

  indiverrors1x[i] = (trueTotalUPDRS1 - predTotalUPDRS1)^2
  indiverrors2x[i] = (trueTotalUPDRS2 - predTotalUPDRS2)^2
  indiverrors3x[i] = (trueTotalUPDRS3 - predTotalUPDRS3)^2
}

datamatrix[1,1] = mean(indiverrors1x)
datamatrix[1,2] = mean(indiverrors2x)
datamatrix[1,3] = mean(indiverrors3x)

# CV for 50 fold
allmodels1 = list(NULL)
allmodels2 = list(NULL)
allmodels3 = list(NULL)
numBuckets = 50
n = numBuckets
indiverrors1 = numeric(length=n)
indiverrors2 = numeric(length=n)
indiverrors3 = numeric(length=n)

indiverrors1x = numeric(length=n)
indiverrors2x = numeric(length=n)
indiverrors3x = numeric(length=n)
folds <- cut(seq(1,nrow(parkdata)),breaks=numBuckets,labels=FALSE)

for (i in c(1:n)){
  testIndices = which(folds==i,arr.ind=TRUE)
  # testData <- parkdata[testIndices, ]
  # trainData <- parkdata[-testIndices, ]
  
  allmodels1[[i]] = glm(formula = total_UPDRS ~ . - subject. - age - sex - test_time - 
                          motor_UPDRS - total_UPDRS - Shimmer - Shimmer.APQ5, data = parkdata[-testIndices, ])
  
  allmodels2[[i]] = glm(formula = total_UPDRS ~ RPDE + DFA + Jitter.DDP + Shimmer.APQ11, data = parkdata[-testIndices, ])
  
  allmodels3[[i]] = glm(formula = total_UPDRS ~ (DFA / RPDE) * (HNR / NHR), data = parkdata[-testIndices, ])
  
  trueTotalUPDRS1 = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS1 = predict(allmodels1[[i]],newdata=parkdata[testIndices,])
  
  trueTotalUPDRS2 = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS2 = predict(allmodels2[[i]],newdata=parkdata[testIndices,])
  
  trueTotalUPDRS3 = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS3 = predict(allmodels3[[i]],newdata=parkdata[testIndices,])
  
  indiverrors1x[i] = (trueTotalUPDRS1 - predTotalUPDRS1)^2
  indiverrors2x[i] = (trueTotalUPDRS2 - predTotalUPDRS2)^2
  indiverrors3x[i] = (trueTotalUPDRS3 - predTotalUPDRS3)^2
}

datamatrix[2,1] = mean(indiverrors1x)
datamatrix[2,2] = mean(indiverrors2x)
datamatrix[2,3] = mean(indiverrors3x)

# CV for LOOCV
allmodels1 = list(NULL)
allmodels2 = list(NULL)
allmodels3 = list(NULL)
numBuckets = 1
n = nrow(parkdata) / numBuckets
indiverrors1 = numeric(length=n)
indiverrors2 = numeric(length=n)
indiverrors3 = numeric(length=n)

indiverrors1x = numeric(length=n)
indiverrors2x = numeric(length=n)
indiverrors3x = numeric(length=n)

for (i in c(1:n)){
  # testData <- parkdata[i, ]
  # trainData <- parkdata[-i, ]
  
  
  if(i %% (4700/100) == 0){
    cat("| ")
  }
  
  allmodels1[[i]] = glm(formula = total_UPDRS ~ . - subject. - age - sex - test_time - 
                          motor_UPDRS - total_UPDRS - Shimmer - Shimmer.APQ5, data = parkdata[-i, ])
  
  allmodels2[[i]] = glm(formula = total_UPDRS ~ RPDE + DFA + Jitter.DDP + Shimmer.APQ11, data = parkdata[-i, ])
  
  allmodels3[[i]] = glm(formula = total_UPDRS ~ (DFA / RPDE) * (HNR / NHR), data = parkdata[-i, ])
  
  trueTotalUPDRS1 = parkdata[i, ]$total_UPDRS
  predTotalUPDRS1 = predict(allmodels1[[i]],newdata=parkdata[i,])
  
  trueTotalUPDRS2 = parkdata[i, ]$total_UPDRS
  predTotalUPDRS2 = predict(allmodels2[[i]],newdata=parkdata[i,])
  
  trueTotalUPDRS3 = parkdata[i, ]$total_UPDRS
  predTotalUPDRS3 = predict(allmodels3[[i]],newdata=parkdata[i,])
  
  indiverrors1x[i] = (trueTotalUPDRS1 - predTotalUPDRS1)^2
  indiverrors2x[i] = (trueTotalUPDRS2 - predTotalUPDRS2)^2
  indiverrors3x[i] = (trueTotalUPDRS3 - predTotalUPDRS3)^2
}


datamatrix[3,1] = mean(indiverrors1x)
datamatrix[3,2] = mean(indiverrors2x)
datamatrix[3,3] = mean(indiverrors3x)

# CV for 10 fold
allmodels1 = list(NULL)
allmodels2 = list(NULL)
allmodels3 = list(NULL)
numBuckets = 10
n = numBuckets
pparkdata = normalize(parkdata)
indiverrors1 = numeric(length=n)
indiverrors2 = numeric(length=n)
indiverrors3 = numeric(length=n)

indiverrors1x = numeric(length=n)
indiverrors2x = numeric(length=n)
indiverrors3x = numeric(length=n)
folds <- cut(seq(1,nrow(parkdata)),breaks=numBuckets,labels=FALSE)

j = 1

for(element in c(1:length(colnames(pparkdata)))){
  for (i in c(1:n)){
    testIndices = which(folds==i,arr.ind=TRUE)
    # testData <- parkdata[testIndices, ]
    # trainData <- parkdata[-testIndices, ]
    
    M = mean(parkdata$total_UPDRS[testIndices])
    
    allmodels1[[i]] = lm(formula = total_UPDRS ~ pparkdata[-testIndices, ][element], data = pparkdata[-testIndices, ])
    
    trueTotalUPDRS1 = pparkdata[testIndices, ]$total_UPDRS
    predTotalUPDRS1 = predict(allmodels1[[i]],newdata=pparkdata[testIndices,])
    
    indiverrors1x[i] = (trueTotalUPDRS1 - predTotalUPDRS1)^2
  }
  allmodels2[j] = (trueTotalUPDRS1 - predTotalUPDRS1)^2
  j = j + 1
}

# CV for 10 fold
allmodels1 = list(NULL)
allmodels2 = list(NULL)
allmodels3 = list(NULL)
numBuckets = 10
n = numBuckets
indiverrors1 = numeric(length=n)
indiverrors2 = numeric(length=n)
indiverrors3 = numeric(length=n)

indiverrors1x = numeric(length=n)
indiverrors2x = numeric(length=n)
indiverrors3x = numeric(length=n)
folds <- cut(seq(1,nrow(parkdata)),breaks=numBuckets,labels=FALSE)

for (i in c(1:n)){
  testIndices = which(folds==i,arr.ind=TRUE)
  # testData <- parkdata[testIndices, ]
  # trainData <- parkdata[-testIndices, ]
  
  M = mean(parkdata$total_UPDRS[testIndices])
  
  allmodels1[[i]] = lm(formula = total_UPDRS ~ DFA + Jitter.DDP + Jitter.RAP + Jitter... + HNR, data = parkdata[-testIndices, ])
  
  allmodels2[[i]] = lm(formula = total_UPDRS ~ DFA * Jitter.DDP * Jitter.RAP * Jitter... * HNR, data = parkdata[-testIndices, ])
  
  allmodels3[[i]] = lm(formula = total_UPDRS ~ (DFA / HNR) * (Jitter.DDP / Jitter...) * Jitter.RAP, data = parkdata[-testIndices, ])
  
  trueTotalUPDRS1 = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS1 = predict(allmodels1[[i]],newdata=parkdata[testIndices,])
  
  trueTotalUPDRS2 = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS2 = predict(allmodels2[[i]],newdata=parkdata[testIndices,])
  
  trueTotalUPDRS3 = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS3 = predict(allmodels3[[i]],newdata=parkdata[testIndices,])

  
  indiverrors1x[i] = (trueTotalUPDRS1 - predTotalUPDRS1)^2
  indiverrors2x[i] = (trueTotalUPDRS2 - predTotalUPDRS2)^2
  indiverrors3x[i] = (trueTotalUPDRS3 - predTotalUPDRS3)^2
}

# CV for 10 fold
numBuckets = 100
n = numBuckets

parkdata = parkdata[-terr, ]
newdata = Normalize(parkdata)

trueTotalUPDRS1 = numeric(length = n)
trueTotalUPDRS2 = numeric(length = n)
trueTotalUPDRS3 = numeric(length = n)
trueTotalUPDRS4 = numeric(length = n)

predTotalUPDRS1 = numeric(length = n)
predTotalUPDRS2 = numeric(length = n)
predTotalUPDRS3 = numeric(length = n)
predTotalUPDRS4 = numeric(length = n)

indiverrors1x = numeric(length=n)
indiverrors2x = numeric(length=n)
indiverrors3x = numeric(length=n)
folds <- cut(seq(1,nrow(parkdata)),breaks=numBuckets,labels=FALSE)

for (i in c(1:n)){
  testIndices = which(folds==i,arr.ind=TRUE)
  # testData <- parkdata[testIndices, ]
  # trainData <- parkdata[-testIndices, ]
  
  allmodels1 = lm(formula = total_UPDRS ~ . - subject. - age - sex - test_time - 
                    motor_UPDRS - total_UPDRS - Shimmer - Shimmer.APQ5, data = parkdata[-testIndices, ])
  
  allmodels2 = lm(formula = total_UPDRS ~ RPDE + DFA + Jitter.DDP + Shimmer.APQ11, data = parkdata[-testIndices, ])
  
  allmodels3 = lm(formula = total_UPDRS ~ (DFA / RPDE) * (HNR / NHR), data = parkdata[-testIndices, ])
  
  allmodels4 = lm(formula = total_UPDRS ~ (DFA / RPDE) * (Jitter.DDP / Jitter...) * HNR, data = parkdata[-testIndices, ])
  
  trueTotalUPDRS1[i] = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS1[i] = predict(allmodels1,newdata=parkdata[testIndices,])
  
  trueTotalUPDRS2[i] = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS2[i] = predict(allmodels2,newdata=parkdata[testIndices,])
  
  trueTotalUPDRS3[i] = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS3[i] = predict(allmodels3,newdata=parkdata[testIndices,])
  
  trueTotalUPDRS4[i] = parkdata[testIndices, ]$total_UPDRS
  predTotalUPDRS4[i] = predict(allmodels4,newdata=parkdata[testIndices,])
  
  indiverrors1x[i] = (trueTotalUPDRS1[i] - predTotalUPDRS1[i])^2
  indiverrors2x[i] = (trueTotalUPDRS2[i] - predTotalUPDRS2[i])^2
  indiverrors3x[i] = (trueTotalUPDRS3[i] - predTotalUPDRS3[i])^2
}

# xvals = c(parkdata$age, parkdata$sex, parkdata$motor_UPDRS, parkdata$RPDE, parkdata$DFA, 
#           parkdata$Jitter.DDP, parkdata$Jitter..., parkdata$HR)

xvals = c(2,3,20,21,11,5,7,19)


for( i in c(1)){
  plot(x = parkdata[[xvals[i]]], y = parkdata[[6]], xlabel = parkdata[[xvals[i]]], ylabel= total_UPDRS)
}

