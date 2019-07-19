# load required libraries
library(caret)
library(corrplot)
library(plyr)


setwd("D:/Data Science/Hackathons/Analytics Vidhya/Practice Probelms/Black Friday")
train<-read.csv("train.csv",sep=",",na.strings = "")
test<-read.csv("test.csv",sep=",",na.strings = "")

head(train)
head(test)


###################### Data pre-processing ###########################
#  Missing Values 
(colSums(is.na(train)))
(colSums(is.na(test)))

####### Bind Both the datasets ###################
Purchase<-train$Purchase
test$Purchase<-0
data<-rbind(train,test)


# Remove variables having high missing percentage (50%)
data1 <- data[, colMeans(is.na(data)) <= .5]
dim(data1)
colSums(is.na(data1))

# Product_Category 2 is missing 
############## Impute missing values ###################
str(data1$Product_Category_2)
unique(data1$Product_Category_2)


################ Exploratory Data Analysis ###################
str(data1)
unique(data1$Age)

unique(data1$Occupation)
data1$Occupation<-as.factor(data1$Occupation)
str(data1$Occupation)

unique(data1$Marital_Status)
data1$Marital_Status<-as.factor(data1$Marital_Status)

unique(data1$Product_Category_1)
data1$Product_Category_1<-as.factor(data1$Product_Category_1)

unique(data1$Product_Category_2)
data1$Product_Category_2<-as.factor(data1$Product_Category_2)




# Impute Missing Values
library(Hmisc)
# impute with mean value
data1$Product_Category_2_imputed <- with(data1, impute(Product_Category_2, median))
data1$Product_Category_2<-data1$Product_Category_2_imputed 
data1$Product_Category_2_imputed <-NULL

colSums(is.na(data1))

############################## PCA ####################
#remove the dependent and identifier variables
my_data <- subset(data1, select = -c(User_ID, Product_ID,Purchase))

#check available variables
colnames(my_data)

#check variable class
str(my_data)

#load library
library(dummies)

#create a dummy data frame
 new_my_data <- dummy.data.frame(my_data, names = c("Gender","Age",
                                                     "Occupation","City_Category","Stay_In_Current_City_Years",
                                                     "Marital_Status","Product_Category_1",
                                                    "Product_Category_2"))
 
 #check the data set
str(new_my_data)

#divide the new data
pca.train <- new_my_data[1:nrow(train),]
pca.test <- new_my_data[-(1:nrow(train)),]

#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)


#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale


prin_comp$rotation

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2


#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
sum(prop_varex[1:65])

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")


#add a training set with principal components
train.data <- data.frame(Purchase = train$Purchase, prin_comp$x)

#we are interested in first 65 PCAs
train.data <- train.data[,1:66]

#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

#select the first 65 components
test.data <- test.data[,1:65]

################# Predictive Modelling ##########################
library(caret)

ctrl <- trainControl(method="repeatedcv",number = 10,repeats=2,verboseIter = T)

############################### gbm   model ######################################################
# #Creating grid
# grid <- expand.grid(n.trees=c(120,150,200),shrinkage=c(0.01,0.05,0.1,0.15),n.minobsinnode = c(5,10,15),interaction.depth=c(2,3))

gbm_fit <- train(Purchase~ .,data = train.data,method = "gbm", metric = "RMSE", trControl = ctrl)

saveRDS(gbm_fit,"gbm_model.rds")

#make prediction on test data
gbm.prediction <- predict(gbm_fit, test.data)

final<-data.frame(User_ID=test$User_ID,Product_ID=test$Product_ID,Purchase=gbm.prediction)
write.csv(final,"gbm_basic.csv",row.names = F)