rm(list = ls())
setwd("~/PUBL0055")

#Load relevant datasets
Training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
Testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

#Load necessary packages, may be required to install them...
library(rattle)
library(caret)
library(kernlab)
library(ggplot2)
library(ISLR)
library(ElemStatLearn)
library(randomForest)
library(rpart)
library(rpart.plot)

#Take a look at the dataset
dim(Training)
str(Training)
dim(Testing)
str(Testing)


#Cleaning the datq can be done with several processes  
#Preliminary check for missing values. Proceed to correct the missing values
training <- Training[ , colSums(is.na(Training)) == 0]
#head(training1)
#training3 <- training.decor[ rowSums(is.na(training.decor)) == 0, ]
dim(training)
str(training)

#Remove irrelevant variables (those that are unlikley to be correlated with Y variable)
remove = c('user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window',
           'num_window','X')
training.1 <- training[, -which(names(training) %in% remove)]
dim(training.1)
#We now need to check the variables with extremely low variance
zeroVar= nearZeroVar(training.1[sapply(training.1, is.numeric)], saveMetrics = TRUE)
training.nonzerovar = training.1[,zeroVar[, 'nzv']==0]
dim(training.nonzerovar)
str(training.nonzerovar)

#Check for highly correlated variables
corrMatrix <- cor(na.omit(training.nonzerovar[sapply(training.nonzerovar, is.numeric)]))
dim(corrMatrix)

#The plot demonstrates those variables are correlated 
corrDF <- expand.grid(row = 1:52, col = 1:52)
corrDF$correlation <- as.vector(corrMatrix)
levelplot(correlation ~ row+ col, corrDF)

#No jusfication to remove variables due to correlation 
training.2<-training.nonzerovar

#Prepare training and testing datasets
inTrain <- createDataPartition(y=training.2$classe, p=0.7, list=FALSE)
training3 <- training.2[inTrain,]; testing <- training.2[-inTrain,]
dim(training3);dim(testing)
#Decision tree with rpart from the caret package
library(caret)
set.seed(1000)
Model1_dt<-rpart(classe~., data= training3, control = rpart.control(xval = 5))
tree.training<-train(classe ~., method = 'rpart', data=training3)
summary(tree.training)
print(tree.training$Model1_dt)
#Decision tree plot with rattle 
library(rattle)
fancyRpartPlot(tree.training$Model1_dt)
#Cross-validation of decision tree
tree.pred<-predict(Model1_dt,testing)
predMatrix<-with(testing,table(tree.pred,classe))
sum(diag(predMatrix))/sum(as.vector(predMatrix)) # error rate
#We can see that the tree is not that accurate, it is necessary to build a better model...
#Pruning the tree: (Improving the model)
cv.training=cv.tree(Model1_dt,FUN=prune.misclass)
cv.training
plot(cv.training)
#Pruning the tree makes little difference to the overall quality of the model

#Trees are typically are not competitive with the best supervised learning approaches in terms of prediction accuracy
#We estimate a random forest model...
library(randomForest)
set.seed(1000)

#Random forest model
Model2_rf<- train(classe~., data = training3, method = "rf", prox = TRUE)
Model2_rf
#To look at the random-forest tree we can call the following line of code:
getTree(Model2_rf$finalModel,k=2)
#How good is the random forest model at predicting?
rf_pred<-predict(modFit,testing); testing$predRight<-pred==testing$classe
table(rf_pred,testing$classe)
#The results show that the random forest model is far superior at predicting than the decision tree model...
#We can now apply the predictions to the testing data
FinalPredictions <- predict(Model2_rf, testing, type = "class")
FinalPredictions

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(FinalPredictions)