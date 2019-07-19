library(h2o)
library(data.table)
library(dplyr)
setwd("D:/Data Science/Hackathons/Analytics Vidhya/Practice Probelms/Black Friday")

#Read in the data. Using data.table significantly improves performance in all kind of 
# data munging activities
train <- fread("train1.csv")
test <- fread("test1.csv")

#Initialise H2o server with 3 threads. I have a 4 core processor, so keeping one for other
#activities (like browsing & FB!). Though not necessary for this data set :) 
#Download and run H2O (http://h2o.ai/)
h2o.server <- h2o.init( nthreads= -1)

## Preprocessing the training data

#Converting all columns to factors
selCols = names(train)[1:7]
train = train[,(selCols) := lapply(.SD, as.factor), .SDcols = selCols]

#Converting to H2o Data frame & splitting
train.hex <- as.h2o(train)

#features=names(train)[!names(train) %in% c("Purchase")]
features=c("User_ID","Product_ID")


gbmF_model_1 = h2o.gbm( x=features,
                        y = "Purchase",
                        training_frame =train.hex ,
                        #validation_frame =testHex ,
                        max_depth = 3,
                        distribution = "gaussian",
                        ntrees =500,
                        learn_rate = 0.05,
                        nbins_cats = 5891
)

gbmF_model_2 = h2o.gbm( x=features,
                        y = "Purchase",
                        training_frame =train.hex ,
                        #validation_frame =testHex ,
                        max_depth = 3,
                        distribution = "gaussian",
                        ntrees =430,
                        learn_rate = 0.04,
                        nbins_cats = 5891
)


dl_model_1 = h2o.deeplearning( x=features,
                               y = "Purchase",
                               training_frame =train.hex ,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=60,
                               adaptive_rate =F
)


dl_model_2 = h2o.deeplearning( x=features,
                               # x=features,
                               y = "Purchase",
                               training_frame =train.hex ,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=60,
                               epochs=40,
                               adaptive_rate =F
)


dl_model_3 = h2o.deeplearning( x=features,
                               y = "Purchase",
                               training_frame =train.hex ,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=120,
                               adaptive_rate =F
)



MySubmission = test[, c("User_ID", "Product_ID"), with = FALSE]
#test = test[,c("User_ID", "Product_ID") := NULL, with = FALSE]

#Converting all columns to factors
selCols = names(test)
test = test[,(selCols) := lapply(.SD, as.factor), .SDcols = selCols]

# Converting to H2o.DataFrame
test.hex  = as.h2o(test)

#Making the predictions
testPurchase_gbm_1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = test.hex) )
testPurchase_gbm_2 = as.data.frame(h2o.predict(gbmF_model_2, newdata = test.hex) )

testPurchase_dl_model_1 = as.data.frame(h2o.predict(dl_model_3, newdata = test.hex) )
testPurchase_dl_model_2 = as.data.frame(h2o.predict(dl_model_3, newdata = test.hex) )
testPurchase_dl_model_3 = as.data.frame(h2o.predict(dl_model_3, newdata = test.hex) )

testPurchase_gbm_1$predict=ifelse(testPurchase_gbm_1$predict<0,0,testPurchase_gbm_1$predict)
testPurchase_gbm_2$predict=ifelse(testPurchase_gbm_2$predict<0,0,testPurchase_gbm_2$predict)

testPurchase_dl_model_1$predict=ifelse(testPurchase_dl_model_1$predict<0,0,testPurchase_dl_model_1$predict)
testPurchase_dl_model_2$predict=ifelse(testPurchase_dl_model_2$predict<0,0,testPurchase_dl_model_2$predict)
testPurchase_dl_model_3$predict=ifelse(testPurchase_dl_model_3$predict<0,0,testPurchase_dl_model_3$predict)

final<-test
final$predict=(((testPurchase_dl_model_1$predict)+
  (testPurchase_dl_model_2$predict)+
  (testPurchase_dl_model_3$predict)+
  (testPurchase_gbm_1$predict)+
  (testPurchase_gbm_2$predict))/5)



#Final Submission
MySubmission$Purchase = final$predict


write.csv(MySubmission, "GBM_h20_r_studio_2_3.csv", row.names = F)