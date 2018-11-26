library(dplyr)
library(rpart)
library(randomForest)
library(ROCR)
library(ROCR)
library(rpart.plot)
library(dummies)
library(caret)
library(ggplot2)
library(pROC)
library(DT)
library(caret)


#Best vars DATA FRAME
best_vars_df <- subset(historical_hr_churn_satisfaction, select = c("Dependents", "NumCompaniesWorked", "NumberProjectsLastYear", "LastPromotion", "TenureManager", "MonthlyIncome", "DistanceHomeOffice", "NewAfterHours", "NewEducation", "NewChurn"))

save(best_vars_df,file="best_vars_df.Rda")
load("best_vars_df.Rda")

str(best_vars_df) 

#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------NORMALIZE DATA-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#
norm_params <- preProcess(best_vars_df, method=c('range'))
norm_data <- predict(norm_params, best_vars_df)
summary(norm_data)

test <- best_vars_df
test$NewChurn <- NULL

str(test)


test$NewAfterHours <- as.numeric(as.character(test$NewAfterHours))


log_data <- log1p(test)

#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------SPLIT DATA-------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#

log_data$NewChurn <- historical_hr_churn_satisfaction$NewChurn
str(log_data)
set.seed(1234)
set.seed(9876)
set.seed(4738)


ind <- sample(2, nrow(log_data), replace=TRUE, prob=c(0.7,0.3))
trainData=log_data[ind==1,]
testData=log_data[ind==2,]

str(trainData)
str(testData)

save(trainData,file="trainData.Rda")
load("trainData.Rda")

save(testData,file="testData.Rda")
load("testData.Rda")


#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------LOGISTIC REGRESSION MODEL-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#
trainData$NewChurn <- as.numeric(as.character(trainData$NewChurn))
testData$NewChurn <- as.numeric(as.character(testData$NewChurn))



model_glm <- glm(NewChurn ~ ., data = trainData, family = binomial("logit"), maxit=100)

summary(model_glm)
model_glm.probs <- predict(model_glm, testData,type="response")
model_glm.pred <- ifelse(model_glm.probs > 0.5, "1", "0")
#model_glm.pred1 <- ifelse(model_glm.probs > 0.33, "1", "0") #APLICA CUTOFF 0.33


library(pROC)
# Compute roc
res.roc <- roc(response = testData$NewChurn, predictor = model_glm.probs)
plot.roc(res.roc, print.auc = TRUE)
#PLOT
# environment setting 
library(ROCR)
library(grid)
library(broom)
library(caret)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(ggthemr)
library(ggthemes)
library(gridExtra)
library(data.table)


trainData$NewChurn<-as.factor(as.character(trainData$NewChurn))
testData$NewChurn<-as.factor(as.character(testData$NewChurn))



#confusion matrix
table(model_glm.pred, testData$NewChurn)
#Accuracy
mean(model_glm.pred == testData$NewChurn)

##Assigning Predicted class and Predict score in Test Data
predictClassTest <- predict(C50Tree,testData, type = "class")
predictScoreTest <- predict(C50Tree,testData, type = "prob")

trainData$NewChurn<-as.factor(trainData$NewChurn)
cm= confusionMatrix( model_glm.pred, testData$NewChurn) #VERIFICAR PRIMEIRO PARAMETRO - COM OU SEM CUTOFF
cm
ggplotConfusionMatrix(cm)




#CROSS-VALIDATION

library(caret)
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=3)
# Fit Naive Bayes Model
model <- train(NewChurn~., data=best_vars_df, trControl=train_control, method="glm")
# Summarise Results
print(model)

#----------------Stepwise
testData$NewChurn<-as.numeric(as.character(testData$NewChurn))
testData$NewChurn<-as.factor(as.character(testData$NewChurn))


library(ROCR)
model_glm <- glm(NewChurn ~ ., data = trainData, family = binomial)
model_step <- step(model_glm, direction = "both", steps = 10000, trace = FALSE)
predictions_step <- predict(model_step, testData, type = "response")
#model_glm.pred1 <- ifelse(predictions_step > 0.33, "1", "0") #APLICA CUTOFF 0.33

str(testData)

library(pROC)
# Compute roc
res.roc <- roc(response = testData$NewChurn, predictor = predictions_step)
plot.roc(res.roc, print.auc = TRUE)


roc_step <- roc(response = testData$NewChurn, predictor = predictions_step)
c <- coords(roc_step, "best", "threshold")
c

Churn_step <- ifelse(predictions_step >= c[[1]], 
                     "1", "0")
cm_log <- confusionMatrix(testData$NewChurn, Churn_step)
cm_log

#----------------Forward
library(MASS)
forward <- stepAIC(model_glm, direction = "forward", trace = FALSE)
forward$anova

#--------------bACKWARD
backward <- stepAIC(model_glm, direction = "backward", trace = FALSE)
backward$anova


#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------RANDOM FOREST-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#
'''
tc_rf <- trainControl(method = "repeatedcv",repeats = 2,number = 3, search = "random")
rf_train <- train(NewChurn ~ ., data = trainData, method = "rf", trainControl = tc_rf) ##Time consuming
'''
fit<- randomForest(as.factor(NewChurn)~., data=trainData, importance=TRUE, ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, testData)


cm_rf <- confusionMatrix(Prediction, testData$NewChurn)
cm_rf
ggplotConfusionMatrix(cm_rf)


#CROSS-VALIDATION

library(caret)
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=3)
# Fit Naive Bayes Model
model <- train(NewChurn~., data=numerical_df, trControl=train_control, method="ranger")
# Summarise Results
print(model)


library(pROC)
Prediction <- as.numeric(Prediction)
# Compute roc
res.roc <- roc(response = testData$NewChurn, predictor = Prediction)
plot.roc(res.roc, print.auc = TRUE)

pred <- as.factor(Prediction)
actual <- as.factor(as.character(testData$NewChurn))
precision(actual,pred)
recall(actual,pred)
F_meas(actual, pred)
#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------DECISION TREES-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#---CART
rp <- rpart(NewChurn ~ ., data = trainData, method = "class", control=rpart.control( minsplit = 100, cp=0.0001))

predictions_rp <- predict(rp, testData, type = "class")
rpart.plot(rp, type=1, extra=100, branch.lty=3, box.palette="RdYlGn", tweak = 1.2, fallen.leaves = FALSE)

str(testData)
cm_rpart <- confusionMatrix(predictions_rp, testData$NewChurn )
cm_rpart
ggplotConfusionMatrix(cm_rpart)

#pfit <- prune(rp, cp=rp$cptable[which.min(rp$cptable[, "xerror"]), "CP"])
#rpart.plot(pfit, type=1, extra=100, branch.lty=3, box.palette="RdYlGn", tweak=1.3, fallen.leaves=FALSE)

#CROSS-VALIDATION

library(caret)
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
model <- train(NewChurn~., data=numerical_df, trControl=train_control, method="rpart")
# Summarise Results
print(model)

testData$NewChurn <- as.numeric(as.character(testData$NewChurn))

measurePrecisionRecall(predictions_rp, testData$NewChurn)

library(pROC)
# Compute roc
predictions_rp<-as.numeric(predictions_rp)
res.roc <- roc(response = testData$NewChurn, predictor = predictions_rp)
plot.roc(res.roc, print.auc = TRUE)

pred <- as.factor(as.character(predictions_rp))
actual <- as.factor(as.character(testData$NewChurn))
precision(actual,pred)
recall(actual,pred)
F_meas(actual, pred)
#---------------------------BAGGING CART
library(ipred)

# fit model
fit <- bagging(NewChurn~., data=trainData, coob = TRUE)

# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, testData, type="class")
# summarize accuracy
table(predictions, trainData$NewChurn)
confusionMatrix(predictions, testData$NewChurn)

str(testData)
trainData$NewChurn <- as.numeric(as.character(trainData$NewChurn))

fit$err #0.1730

library(pROC)
# Compute roc
predictions<-as.numeric(predictions)
res.roc <- roc(response = testData$NewChurn, predictor = predictions)
plot.roc(res.roc, print.auc = TRUE)

#-------------------------C5.0
prop.table(table(trainData$NewChurn))
prop.table(table(testData$NewChurn))

#train data
library(C50)

churn_model <- C5.0(x=trainData[,-10], y=as.factor(trainData$NewChurn))
churn_model

summary(churn_model)

#Evaluate model performance
churn_pred <- predict(churn_model, testData)
library(gmodels)
gmodels::CrossTable(testData$NewChurn, churn_pred,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual churn', 'predicted churn'))

cm_dec <- confusionMatrix(churn_pred, testData$NewChurn )
cm_dec


library(pROC)
# Compute roc
churn_pred<-as.numeric(churn_pred)
res.roc <- roc(response = testData$NewChurn, predictor = churn_pred)
plot.roc(res.roc, print.auc = TRUE)

#CROSS-VALIDATION

library(caret)
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=3)
# Fit Naive Bayes Model
model <- train(NewChurn~., data=numerical_df, trControl=train_control, method="C5.0")
# Summarise Results
print(model)

#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------NAIVE BAYES-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#
library(e1071)

set.seed(2345)
set.seed(1256)
set.seed(10)

trainData$class <- NULL
testData$class<-NULL

nb <- naiveBayes(NewChurn ~ ., trainData)
(mtrx <- table(predict(nb,testData), testData$NewChurn))
(err <- 1-sum(diag(mtrx))/sum(mtrx))
pred <- predict(nb, testData, type='raw')


nb_train_predict <- predict(nb, testData[ , names(testData) != "NewChurn"])
cfm <- confusionMatrix(nb_train_predict, testData$NewChurn)
cfm

#PLOT CM
library(ggplot2)
library(scales)

ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle) + theme_minimal()
  return(p)
}

ggplotConfusionMatrix(cfm)

#CROSS-VALIDATION

library(caret)
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
model <- train(NewChurn~., data=numerical_df, trControl=train_control, method="nb")
# Summarise Results
print(model)

library(pROC)
# Compute roc
nb_train_predict<-as.numeric(nb_train_predict)
res.roc <- roc(response = testData$NewChurn, predictor = nb_train_predict)
plot.roc(res.roc, print.auc = TRUE)

#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------Neural Network-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#


trainset$NewChurn <- NULL
testset$NewChurn<-NULL
str(trainData)
trainData$NewChurn = as.numeric(as.character(trainData$NewChurn))
# Building a neural network (NN)
library( neuralnet )
n = names( trainData )
f = as.formula( paste( "NewChurn ~", paste( n[!n %in% "NewChurn"], collapse = "+" ) ) )
nn = neuralnet( f,trainData,learningrate = 0.01, hidden = 4, linear.output = FALSE, threshold = 0.01, stepmax = 1e6 )

plot( nn, rep = 'best' )
plot(nn)
# Testing the result output
nn.results = compute(nn, testData[1:9])

results = data.frame( actual = testData$NewChurn, prediction = round( nn.results$net.result ) )

# Confusion matrix
library( caret )
t = table( results )
print( confusionMatrix( t ) )



library(pROC)
nn<-as.numeric(nn)
# Compute roc
res.roc <- roc(response = testData$NewChurn, predictor = nn.results$net.result)
plot.roc(res.roc, print.auc = TRUE)

pred <- as.factor(as.character(nn$net.result))
pred <- 
actual <- as.factor(as.character(testData$NewChurn))
precision(actual,pred)
recall(actual,pred)
F_meas(actual, pred)

#CROSS-VALIDATION

library(caret)
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=3)
# Fit Naive Bayes Model
model <- train(NewChurn~., data=numerical_df, trControl=train_control, method="nnet")
# Summarise Results
print(model)
















