##Packages and Data Reading
data <- read.csv("rawDF.csv")
require(dplyr)
require(caret)
require(rpart)
require(rattle)
require(RColorBrewer)


############ a)
if(any(is.na(data))){
  print("yes")
}else{
    print("no")
}
## "no"
numericDF<- select_if(data, is.numeric)


############ b)
numericDF2 <- subset(numericDF, select = -numericDF$traffic)
for(i in 1:ncol(numericDF)){
  numericDF2[,i] = (numericDF2[,i]  - min(numericDF2[,i]))/(max(numericDF2[,i]) - min(numericDF2[,i])) 
}
numericDF2 = cbind(numericDF2, numericDF["traffic"])

############ c)
splitVec <- createDataPartition(numericDF2$h.label, p = 0.75, list = F)
trainSet <- numericDF2[splitVec,]
testSet<- numericDF2[-splitVec,] 


############ d)
calcRMSE<- function(predV, obV){
  #predV is a vector of predicted response values
  #obV is a vector of observed response values
  myRMSE = sum(( predV - obV)^2 / length(predV))
}


############ e)
mytree <- rpart(trainSet$traffic ~., data = trainSet)
fancyRpartPlot(mytree)
result <- predict(mytree, newdata=testSet)
obj_val = calcRMSE(result, testSet$traffic)
print(paste("The RMSE is: ", obj_val))
##"The RMSE is:  0.0102672882917386"
