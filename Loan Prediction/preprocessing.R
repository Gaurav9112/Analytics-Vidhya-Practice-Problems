setwd("D:/Data Science/Hackathons/Analytics Vidhya/Practice Probelms/Loan Prediction")

train<-read.csv(file = file.choose(),sep = ",",na.strings = "")
str(train)
head(train)
colSums(is.na(train))

test<-read.csv(file = file.choose(),sep = ",",na.strings = "")
str(test)
head(test)
colSums(is.na(test))

#turn binary variable Credit_History into factor
train$Credit_History<-factor(train$Credit_History,labels = c("N","Y"))
test$Credit_History<-factor(test$Credit_History,labels = c("N","Y"))

#convert factor variable Dependents into numeric
levels(train$Dependents)[levels(train$Dependents)=="3+"] <- "3"
train$Dependents<-as.integer(as.character(train$Dependents))
levels(test$Dependents)[levels(test$Dependents)=="3+"] <- "3"
test$Dependents<-as.integer(as.character(test$Dependents))

#remove the id column 
train<-train[-1]

summary(train)

# Some of the first thoughts from summary:
#   
# There must be one or more extreme value of applicants' income
# More than 25% of the applicants do not have coaaplicant 
#Co-Applicant income 0 error, Impute misssing value
train$CoapplicantIncome[train$CoapplicantIncome==0]<-NA
test$CoapplicantIncome[test$CoapplicantIncome==0]<-NA


# The applicants has trends toward male, graduate, or self-employed
# Most applicants' credit history meet the guidlines
# The response variable, loan status is not balanced with a ratio of about 2:1
# There are variables that have missing values in data

# create a new feature of the number of Na in an observation
train_NAs<-NULL
test_NAs<-NULL
for(i in 1:nrow(train)) train_NAs[i]<-sum(is.na(train[i, ]))
train$NA_number<-train_NAs
for(i in 1:nrow(test)) test_NAs[i]<-sum(is.na(test[i, ]))
test$NA_number<-test_NAs

#the ratio of missing for each variable
names<-names(train)
missing<-data.frame(variable=names,missing_proportion=sapply(names,function(x) sum(is.na(train[x]))/nrow(train)))
missing #The missing rate does not exceed 10%



######################################### Impute Missing Values ####################################
library(mice)
#impute missing values by package mice, considering that the missing values are not MNAR(missing not at random)
trainimp<-mice(data=train,m=5,maxit = 10,method="pmm",printFlag=FALSE,seed=0817) #estimate fitting values for continuous variables using predictive mean matching of mice
newtrain<-complete(trainimp) #imput the estimated missing values
sum(is.na(newtrain)) #all missing value are imputed

testimp<-mice(data=test[-1],m=5,maxit = 10,method="pmm",printFlag=FALSE,seed=0817) 
newtest<-complete(testimp) #imput the estimated missing values for test dataset as well


#################### Feature Engineering ###########################
# Income_by_loan: The sum of applicant and co-applicant income, 
# then divided by loan amount
a<-newtrain$ApplicantIncome
b<-newtrain$CoapplicantIncome
c<-cbind(a,b)
c<-as.data.frame(c)
d<-rowSums(c)
e<-newtrain$LoanAmount
Income_by_loan<-(d/e)
newtrain$Income_by_loan<-Income_by_loan


# ApplicantIncome_by_loan: The sum of applicant income itself, dived by loan amount
a<-newtrain$ApplicantIncome
e<-newtrain$LoanAmount
ApplicantIncome_by_loan<-(a/e)
newtrain$ApplicantIncome_by_loan<-ApplicantIncome_by_loan


# EMI: Calculated equated monthly installment
# Formula [P x R x (1+R)^N]/[(1+R)^N-1]
p<-newtrain$LoanAmount
r<-8.70
n<-(newtrain$Loan_Amount_Term/12)
EMI <- ((p*r*((1+r)^n)))/(((1+r)^(n-1)))
newtrain$EMI<-EMI


#################### Feature Engineering  on Test ###########################
# Income_by_loan: The sum of applicant and co-applicant income, 
# then divided by loan amount
a<-newtest$ApplicantIncome
b<-newtest$CoapplicantIncome
c<-cbind(a,b)
c<-as.data.frame(c)
d<-rowSums(c)
e<-newtest$LoanAmount
Income_by_loan<-(d/e)
newtest$Income_by_loan<-Income_by_loan


# ApplicantIncome_by_loan: The sum of applicant income itself, dived by loan amount
a<-newtest$ApplicantIncome
e<-newtest$LoanAmount
ApplicantIncome_by_loan<-(a/e)
newtest$ApplicantIncome_by_loan<-ApplicantIncome_by_loan


# EMI: Calculated equated monthly installment
# Formula [P x R x (1+R)^N]/[(1+R)^N-1]
p<-newtest$LoanAmount
r<-8.70
n<-(newtest$Loan_Amount_Term/12)
EMI <- ((p*r*((1+r)^n)))/(((1+r)^(n-1)))
newtest$EMI<-EMI


colSums(is.na(newtrain))
colSums(is.na(newtest))


############################# Re-ordering #################################
train<-newtrain
test<-newtest
rm(newtrain,newtest)

# ################ Variable Importance ###################################
library(randomForest)
library(caret)
library(corrplot)
library(plyr)
#Train Random Forest
rf <-randomForest(Loan_Status~.,data=train, importance=TRUE,ntree=1000)

#Evaluate variable importance
imp = importance(rf, type=1)
imp <- data.frame(predictors=rownames(imp),imp)

# Order the predictor levels by importance
imp.sort <- arrange(imp,desc(MeanDecreaseAccuracy))
imp.sort$predictors <- factor(imp.sort$predictors,levels=imp.sort$predictors)

# Select the top 20 predictors
imp.20<- imp.sort[1:15,]
print(imp.20)

# Plot Important Variables
varImpPlot(rf, type=1)

############## Remove irrelevant features ######################
# Dependents, Education, Gender, Married,
# NA_number

train$Dependents<-NULL
train$Education<-NULL
train$Married<-NULL
train$NA_number<-NULL
train$Self_Employed<-NULL
train$Gender<-NULL
train$Loan_Amount_Term<-NULL


test$Loan_Amount_Term<-NULL
test$Dependents<-NULL
test$Education<-NULL
test$Married<-NULL
test$NA_number<-NULL
test$Self_Employed<-NULL
test$Gender<-NULL


############# Sampling using SMOTE  ############################
library(DMwR)

smote_train <- SMOTE(Loan_Status ~ ., data  = train)
table(smote_train$Loan_Status)


library(ROSE)
rose_train <- ROSE(Loan_Status ~ ., data  = train)$data                         
table(rose_train$Loan_Status) 



up_train <- upSample(x = train[, -4],
                     y = train$Loan_Status) 
up_train$Loan_Status<-up_train$Class
up_train$Class<-NULL

down_train <- downSample(x = train[, -4],
                         y = train$Loan_Status)
down_train$Loan_Status<-down_train$Class
down_train$Class<-NULL

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,verboseIter = T)

orig_fit <- train(Loan_Status ~ ., data = train, 
                  method = "treebag",
                  nbagg = 50,
                  metric = "ROC",
                  trControl = ctrl)


down_outside <- train(Loan_Status ~ ., data = down_train, 
                      method = "treebag",
                      nbagg = 50,
                      metric = "ROC",
                      trControl = ctrl)

up_outside <- train(Loan_Status ~ ., data = up_train, 
                    method = "treebag",
                    nbagg = 50,
                    metric = "ROC",
                    trControl = ctrl)

rose_outside <- train(Loan_Status ~ ., data = rose_train, 
                      method = "treebag",
                      nbagg = 50,
                      metric = "ROC",
                      trControl = ctrl)

smote_outside <- train(Loan_Status ~ ., data = smote_train, 
                       method = "treebag",
                       nbagg = 50,
                       metric = "ROC",
                       trControl = ctrl)

###############################################################
outside_models <- list(original = orig_fit,
                       down = down_outside,
                       up = up_outside,
                       SMOTE = smote_outside,
                       ROSE = rose_outside)

outside_resampling <- resamples(outside_models)

################################################################################
test_roc <- function(model, data) {
  library(pROC)
  roc_obj <- roc(data$Loan_Status, 
                 predict(model, data, type = "prob")[, "Y"],
                 levels = c("N", "Y"))
  ci(roc_obj)
}

outside_test <- lapply(outside_models, test_roc, data = train)
outside_test <- lapply(outside_test, as.vector)
outside_test <- do.call("rbind", outside_test)
colnames(outside_test) <- c("lower", "ROC", "upper")
outside_test <- as.data.frame(outside_test)

summary(outside_resampling, metric = "ROC")

#SMOTE OR UP Sampling to use

# #               Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# original 0.7021505 0.7593652 0.7924731 0.7980914 0.8322994 0.9166667    0
# down     0.6903226 0.7749870 0.7981183 0.7985637 0.8273933 0.8940860    0
# up       0.6789802 0.7697407 0.7961325 0.8003755 0.8427016 0.9247312    0
# SMOTE    0.9796418 0.9886927 0.9932353 0.9923135 0.9967013 0.9998664    0
# ROSE     0.6865204 0.7679149 0.8049242 0.8016519 0.8450629 0.8934169    0


write.csv(smote_train,"train_data.csv",row.names = F)
write.csv(test,"test_data.csv",row.names = F)
