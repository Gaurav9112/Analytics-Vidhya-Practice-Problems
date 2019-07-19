setwd("D:/Data Science/Hackathons/Analytics Vidhya/Practice Probelms/Loan Prediction")
train <-read.csv("train_u6lujuX_CVtuZ9i.csv",na.strings = c(""," ",NA))
test <-read.csv("test_Y3wMUE5_7gLdaTN.csv",na.strings = c(""," ",NA))



################################### EDA ########################################



library(mlr)
summarizeColumns(train)

summarizeColumns(test)

barplot(table(train$Loan_Status))
prop.table(table(train$Loan_Status))

colSums(is.na(train))
colSums(is.na(test))

########################### Gender ################################3

par(mfrow=c(1,2))
barplot(table(train$Gender),main="train set")
barplot(table(test$Gender),main="test set")
prop.table(table(train$Gender))
prop.table(table(test$Gender))


########################### Married ################################3

par(mfrow=c(1,2))
barplot(table(train$Married),main="train set")
barplot(table(test$Married),main="test set")
prop.table(table(train$Married))
prop.table(table(test$Married))


########################### Dependants################################3

par(mfrow=c(1,2))
levels(train$Dependents)
barplot(table(train$Dependents),main="train set")
barplot(table(test$Dependents),main="test set")
prop.table(table(train$Dependents))
prop.table(table(test$Dependents))


########################### Education ################################3

par(mfrow=c(1,2))
levels(train$Education)
barplot(table(train$Education),main="train set")
barplot(table(test$Education),main="test set")

prop.table(table(train$Education))
prop.table(table(test$Education))


par(mfrow=c(1,2))
levels(train$Self_Employed)
barplot(table(train$Self_Employed),main="train set")
barplot(table(test$Self_Employed),main="test set")
prop.table(table(test$Self_Employed))
prop.table(table(train$Self_Employed))


##################### Applicant Income and Co-Applicant Income #####################
par(mfrow=c(1,2))
boxplot(train$ApplicantIncome,train$CoapplicantIncome,names=c("App Income","Coapp Income"),main="train set")
boxplot(test$ApplicantIncome,test$CoapplicantIncome,names=c("App Income","Coapp Income"),main="test set")


par(mfrow=c(1,2))
boxplot(train$LoanAmount,main="train set")
boxplot(test$LoanAmount,main="test set")

par(mfrow=c(1,2))
hist(train$Loan_Amount_Term,breaks=500,main="train set")
hist(test$Loan_Amount_Term,breaks=500,main="test set")

summary(train$Loan_Amount_Term)
summary(test$Loan_Amount_Term)


par(mfrow=c(1,2))
train$Credit_History <-as.factor(train$Credit_History)
test$Credit_History <- as.factor(test$Credit_History)
barplot(table(train$Credit_History),main="train set")
barplot(table(test$Credit_History),main="test set")


par(mfrow=c(1,2))
barplot(table(train$Property_Area),main="train set")
barplot(table(test$Property_Area),main="test set")
# This is the only predictor whose distribution looks different in the two sets.


print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Gender)+ggtitle("Loan Status by Gender of Applicant"))

print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Married)+ggtitle("Loan Status by Marital Status of Applicant"))
#a larger proportion of not married applicants are refused than mmaried ones

print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Dependents)+ggtitle("Loan Status by number of Dependents of Applicant"))
#a smaller proportion of applicants with 2 dependents is refused than other numbers

print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Education)+ggtitle("Loan Status by Education of Applicant"))
#a larger proportion on non graduates are refused than graduates

print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Self_Employed)+ggtitle("Loan Status by Employment status of Applicant"))
#not self employed seesm to be slightly preferred

print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Loan_Amount_Term)+ggtitle("Loan Status by terms  of loan"))
#difficult to see any patterns, most of the loans are for 360 months


print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Credit_History)+ggtitle("Loan Status by credit history of Applicant"))
#this looks very important! Almost all applicants with history=0 are refused


print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Property_Area)+ggtitle("Loan Status by property area"))
#it's easiest to get a loan if the property is semi urban and hardest if it is rural

print(ggplot(train, aes(x=Loan_Status,y=ApplicantIncome))+geom_boxplot()+ggtitle("Loan Status by Applicant income"))
#doesn't look like there's much difference

print(ggplot(train, aes(x=Loan_Status,y=CoapplicantIncome))+geom_boxplot()+ggtitle("Loan Status by coapplicant income"))
#this seems to make a difference

print(ggplot(train, aes(x=Loan_Status,y=LoanAmount))+geom_boxplot()+ggtitle("Loan Status by Loan Amount"))






####################Preprocessing #########################################33

alldata<-rbind(train[,2:12],test[,2:12])
#pairs(alldata)

library(ggplot2)
#Applicants with higher than 20000 income have been truncated from the plot 
print(ggplot(data=alldata[alldata$ApplicantIncome<20000,],aes(ApplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))

print(ggplot(data=alldata[alldata$ApplicantIncome<20000,],aes(CoapplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))

library(plyr)
alldata2<-mutate(alldata,TotalIncome=ApplicantIncome+CoapplicantIncome)
print(ggplot(data=alldata2,aes(TotalIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))


alldata2$Married[is.na(alldata2$Married) & alldata2$CoapplicantIncome==0]<-"No"
alldata2$Married[is.na(alldata2$Married)]<- "Yes"

alldata2[is.na(alldata2$Gender) & is.na(alldata2$Dependents),]

alldata2$Gender[is.na(alldata2$Gender) & is.na(alldata2$Dependents)] <- "Male"
print(ggplot(alldata2,aes(x=Dependents, fill=Gender)) + geom_bar() + facet_grid(.~Married))  

alldata2$Dependents[is.na(alldata2$Dependents) & alldata2$Married=="No"]<- "0"

mm <- alldata2[(alldata2$Gender=="Male" & alldata2$Married=="Yes"),c(3,6:9,11)]
mmtrain<-mm[!is.na(mm$Dependents),]
mmtest<- mm[is.na(mm$Dependents),]
library(rpart)
library(rattle)
depFit <- rpart(data=mmtrain,Dependents~.,xval=3)
fancyRpartPlot(depFit)

#accuracy
p<-predict(depFit,mmtrain,type="class")
acc=sum(p==mmtrain[,1])/length(p)
acc

alldata2$Dependents[is.na(alldata2$Dependents) & alldata2$Gender=="Male" & alldata2$Married == "Yes"]<- predict(depFit,newdata=mmtest,type="class")


gtrain<-alldata2[!is.na(alldata2$Gender),1:7]
gtest<-alldata2[is.na(alldata2$Gender),1:7]
genFit<-rpart(data=gtrain,Gender~.,xval=3)
fancyRpartPlot(genFit)

#accuracy
p<-predict(genFit,gtrain,type="class")
acc<-sum(p==gtrain[,1])/length(p)
acc

#impute missing genders
alldata2$Gender[is.na(alldata2$Gender)]<-predict(genFit,gtest,type="class")

alldata2$Self_Employed[is.na(alldata$Self_Employed)] <- "No"

library(car)
alldata2$Credit_History<-recode(alldata2$Credit_History,"NA=2")


ltrain<-alldata2[!is.na(alldata2$LoanAmount) & alldata2$LoanAmount<500,c(1:8,10)]
ltest <- alldata2[is.na(alldata2$LoanAmount),c(1:8,10)]
loanFit <- glm(data=ltrain,LoanAmount~.,na.action=na.exclude)
#impute
alldata2$LoanAmount[is.na(alldata2$LoanAmount)] <- predict(loanFit,newdata=ltest)

alldata2$Loan_Amount_Term <- as.factor(alldata2$Loan_Amount_Term)
print(ggplot(data=alldata2,aes(x=Loan_Amount_Term))+geom_bar())

alldata2$Loan_Amount_Term[is.na(alldata2$Loan_Amount_Term)]<-"360"
alldata2$Loan_Amount_Term <- recode(alldata2$Loan_Amount_Term,"'350'='360';'6'='60'")


##################### Creating more features #######################################

numDependents <- recode(alldata2$Dependents,"'3+'='3' ")
numDependents <- as.numeric(as.character(numDependents))
alldata2$FamilySize <- ifelse((alldata2$CoapplicantIncome>0 |alldata2$Married=="Y"),numDependents+2,numDependents+1)
alldata2$IncomePC <- alldata2$TotalIncome/alldata2$FamilySize

alldata2$LoanAmountByTotInc <- alldata2$LoanAmount/alldata2$TotalIncome
alldata2$LoanAmountPC <- alldata2$LoanAmount/alldata2$IncomePC

alldata2$Loan_Amount_Term <- as.numeric(as.character(alldata2$Loan_Amount_Term))
alldata2$LoanPerMonth <- alldata2$LoanAmount/alldata2$Loan_Amount_Term

alldata2$LoanPerMOnthByTotInc  <- alldata2$LoanPerMonth/alldata2$TotalIncome
alldata2$LoanPerMonthPC <- alldata2$LoanPerMonth/alldata2$LoanAmountPC

#make loan term variable factor again
alldata2$Loan_Amount_Term <- as.factor(alldata2$Loan_Amount_Term)

bins<-cut(alldata2$ApplicantIncome,breaks=20)
barplot(table(bins),main="Applicant Income")

logbins<-cut(ifelse(alldata2$ApplicantIncome<2.72,0,log(alldata2$ApplicantIncome)),breaks=20)
barplot(table(logbins),main="Log of Applicant Income")

alldata2$LogApplicantIncome <- ifelse(alldata2$ApplicantIncome<2.72,0,log(alldata2$ApplicantIncome))
alldata2$LogCoapplicantIncome <- ifelse(alldata2$CoapplicantIncome<2.72,0,log(alldata2$CoapplicantIncome))

summary(alldata2$LoanAmount)
alldata2$LogLoanAmount <- log(alldata2$LoanAmount)

summary(alldata2$TotalIncome)
alldata2$LogTotalIncome <- log(alldata2$TotalIncome)

summary(alldata2$IncomePC)
alldata2$IncomePC <- log(alldata2$IncomePC)


summary(alldata2$LoanAmountByTotInc)
#This one looks symmetric, no transform

summary(alldata2$LoanAmountPC)
alldata2$LogLoanAmountPC <- log(1000*alldata2$LoanAmountPC)

summary(alldata2$LoanPerMonth)
alldata2$LogLoanPerMOnth <- log(alldata2$LoanPerMonth)

summary(alldata2$LoanPerMOnthByTotInc)
summary(alldata2$LoanPerMonthPC)


alldata2$LogLoanPerMOnthPC <- log(alldata2$LoanPerMonthPC)

nums <- sapply(alldata2,class)=="numeric"
numvars <- alldata2[,nums]
m<-cor(numvars)
v<-as.vector(m) 
id1<- rep(rownames(m),17)
id2<-as.vector(sapply(rownames(m),function(x)rep(x,17)))
d<-data.frame(v,id1,id2)
d<-d[d$v>0.8 & d$v<1,]
d

#Half of the rows are symmetric repeats of the other rows
d<-d[c(1:5,8),]
d

#remove the columns of alldata2 with names in id1
alldata2<-alldata2[,!(names(alldata2) %in% d$id1)]


newtrain <- cbind(Loan_Status=train$Loan_Status,alldata2[1:614,])


# Training and Predicting Loan status


#bogus Loan status for test set
Loan_Status <- as.factor(sample(c("N","Y"),replace=TRUE,size=dim(test)[1]))
newtest <- cbind(Loan_Status,alldata2[615:981,])

#create task
trainTask <- makeClassifTask(data = newtrain,target = "Loan_Status")
testTask <- makeClassifTask(data = newtest, target = "Loan_Status")

#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")


tree <- makeLearner("classif.rpart", predict.type = "response")

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#Search for hyperparameters
treepars <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

#try 100 different combinations of values
tpcontrol <- makeTuneControlRandom(maxit = 100L)

#hypertune the parameters
rm(acc)
set.seed(11)
treetune <- tuneParams(learner = tree, resampling = set_cv, 
                       task = trainTask, par.set = treepars, control = tpcontrol, measures = acc)
treetune

#using hyperparameters for modeling
tunedtree <- setHyperPars(tree, par.vals=treetune$x)

#train the model
treefit <- train(tunedtree$par.vals, trainTask)
par(mfrow=c(1,1))
fancyRpartPlot(getLearnerModel(treefit))
################# Sampling ##############################
# ############# Sampling using SMOTE  ############################
# library(DMwR)
# 
# smote_train <- SMOTE(Loan_Status ~ ., data  = newtrain)
# table(smote_train$Loan_Status)

smote_train<-newtrain

library(randomForest)
library(caret)
library(corrplot)
library(plyr)  
#Train Random Forest
rf <-randomForest(Loan_Status~.,data=smote_train, importance=TRUE,ntree=2000,do.trace=T)

# Plot Important Variables
varImpPlot(rf, type=1)

smote_train$Gender<-NULL
smote_train$Education<-NULL
smote_train$Self_Employed<-NULL
smote_train$Property_Area<-NULL
smote_train$Married<-NULL
smote_train$Dependents<-NULL

newtest$Gender<-NULL
newtest$Education<-NULL
newtest$Self_Employed<-NULL
newtest$Property_Area<-NULL
newtest$Married<-NULL
newtest$Dependents<-NULL


############################# Ensemble Model in R ###########################
newtrain <- cbind(Loan_Status=train$Loan_Status,alldata2[1:614,])

#bogus Loan status for test set
Loan_Status <- as.factor(sample(c("N","Y"),replace=TRUE,size=dim(test)[1]))
newtest <- cbind(Loan_Status,alldata2[615:981,])


newtrain$Gender<-NULL
newtrain$Education<-NULL
newtrain$Self_Employed<-NULL
newtrain$Property_Area<-NULL
newtrain$Married<-NULL
newtrain$Dependents<-NULL

  
newtest$Gender<-NULL
newtest$Education<-NULL
newtest$Self_Employed<-NULL
newtest$Property_Area<-NULL
newtest$Married<-NULL
newtest$Dependents<-NULL

#create task
trainTask <- makeClassifTask(data = newtrain,target = "Loan_Status")
testTask <- makeClassifTask(data = newtest, target = "Loan_Status")

#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")





tree <- makeLearner("classif.xgboost", predict.type = "response")

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#Search for hyperparameters
treepars <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

#try 100 different combinations of values
tpcontrol <- makeTuneControlRandom(maxit = 100L)

#hypertune the parameters
rm(acc)
set.seed(11)
treetune <- tuneParams(learner = tree, resampling = set_cv, 
                       task = trainTask, par.set = treepars, control = tpcontrol, measures = acc)
treetune


#using hyperparameters for modeling
tunedtree <- setHyperPars(tree, par.vals=treetune$x)

detach("package:caret", unload=TRUE)

#train the model
treefit <- train(tunedtree, trainTask)
par(mfrow=c(1,1))
fancyRpartPlot(getLearnerModel(treefit))



#make predictions
treepred <- predict(treefit, testTask)

#create a submission file
submit1 <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = treepred$data$response)
 write.csv(submit1, "sol1.csv",row.names = F)





