setwd("D:/Data Science/Hackathons/Analytics Vidhya/Practice Probelms/Loan Prediction")
# load required libraries
library(caret)
library(corrplot)
library(plyr)

################ Model Building ################################
train<-read.csv("train_data.csv")
test1<-read.csv("test_data.csv")

# ############ Create Dummy Variables ################################
# library(dummies)
# 
#create a dummy data frame
# train1 <- dummy.data.frame(train, names = c("Credit_History","Property_Area"))
# test1<-dummy.data.frame(test, names = c("Credit_History","Property_Area"))
# str(train1)




###############Random Forest Model ###################################
library(caret)
# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3,verboseIter = T)
seed <- 7
metric <- "Accuracy"
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Loan_Status~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
summary(rf_default)

pred<-predict(rf_default,newdata = test1)
final<-data.frame(Loan_ID=test$Loan_ID,Loan_Status=pred)
write.csv(final,"ensemble_rf1_1.csv",row.names = F,quote = F)

#############################  Method 1 ####################################
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random",verboseIter = T)
mtry <- sqrt(ncol(train))
rf_random <- train(Loan_Status~., data=train, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
summary(rf_random)

pred<-predict(rf_random,newdata = test1)
final<-data.frame(Loan_ID=test$Loan_ID,Loan_Status=pred)
write.csv(final,"ensemble_rf2.2.csv",row.names = F,quote = F)



################################### Method 2 ###########################################

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(Loan_Status~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


pred<-predict(rf_gridsearch,newdata = test1)
final<-data.frame(Loan_ID=test$Loan_ID,Loan_Status=pred)
write.csv(final,"ensemble_rf3.csv",row.names = F,quote = F)


################################### Method 3 ###############################################

# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid",verboseIter = T)
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500,3000,3500)) {
  set.seed(seed)
  fit <- train(Loan_Status~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)


pred<-predict(fit,newdata = test1)
final<-data.frame(Loan_ID=test$Loan_ID,Loan_Status=pred)
write.csv(final,"ensemble_rf4.csv",row.names = F,quote = F)


# # Example of Stacking algorithms
# # create submodels
# library(caret)
# library(caretEnsemble)
# control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE,verboseIter = T,classProbs=TRUE)
# algorithmList <- c('rf','xgbTree')
# models <- caretList(Loan_Status~., data=train1, trControl=control, methodList=algorithmList)
# results <- resamples(models)
# summary(results)
# dotplot(results)
# 
# # correlation between results should be less than 0.75
# # correlation between results
# modelCor(results)
# splom(results)
# 
# # stack using rf
# stackControl <- trainControl(method="repeatedcv", number=10, repeats=3,verboseIter = T,classProbs=TRUE)
# stack.glm <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
# print(stack.glm)
# summary(stack.glm)
# pred<-predict(stack.glm,newdata = test1)
# final<-data.frame(Loan_ID=test$Loan_ID,Loan_Status=pred)
# write.csv(final,"ensemble_rf1.csv",row.names = F,quote = F)