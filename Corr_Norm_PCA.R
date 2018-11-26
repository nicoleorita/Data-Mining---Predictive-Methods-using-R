#--------------------------------------CORRELATION MATRIX

new_historical_df <- historical_hr_churn_satisfaction
str(new_historical_df)

#### RUN THIS IF YOU WANT TO DO THE PCA WITH ALL NUMERICAL VARIABLES
new_historical_df$AfterHours <- NULL
new_historical_df$Churn <- NULL
new_historical_df$BalanceWork.Life <- NULL
new_historical_df$EducationArea <- NULL
new_historical_df$Education <- NULL
new_historical_df$TypeContract <- NULL
new_historical_df$JobRole <- NULL
new_historical_df$Department <- NULL
new_historical_df$JobType <- NULL
new_historical_df$MaritalStatus <- NULL
new_historical_df$Gender <- NULL
new_historical_df$BirthDate <- NULL
new_historical_df$`Employee ID` <- NULL
new_historical_df$DepartmentName <- NULL
new_historical_df$WeekHours <- NULL
new_historical_df$NewChurn <- NULL
new_historical_df$YearPerc <- NULL
new_historical_df$NewJobPerformance <- NULL
new_historical_df$NewGender <- NULL

### REMOVE THIS WITH THE ABOVE TO DO THE PCA ONLY ON THE ORIGINAL DATA
new_historical_df$NewAfterHours <- NULL
new_historical_df$NewBalanceWorkLife <- NULL
new_historical_df$NewRoleSatisfaction <- NULL
new_historical_df$NewFacilitiesSatisfaction <- NULL
new_historical_df$NewHierarchySatisfaction <- NULL
new_historical_df$NewJobDedication <- NULL
new_historical_df$NewJobPerformance <- NULL
new_historical_df$NewEducation <- NULL
new_historical_df$totalSatisfaction <- NULL
new_historical_df$totalJobRewarding <- NULL

#### RUN THIS NULL IF YOU WANT ONLY THE 8 MORE IMPORTANT VARIABLES TO THE PCA
new_historical_df$`Employee ID` <- NULL
new_historical_df$BirthDate <- NULL
new_historical_df$Gender <- NULL
new_historical_df$MaritalStatus <- NULL
new_historical_df$Dependents <- NULL
new_historical_df$JobType <- NULL
new_historical_df$Department <- NULL
new_historical_df$JobLevel <- NULL
new_historical_df$JobRole <- NULL
new_historical_df$Representative <- NULL
new_historical_df$TypeContract <- NULL
new_historical_df$Education <- NULL
new_historical_df$EducationArea <- NULL
new_historical_df$`SalaryRise(%)` <- NULL
new_historical_df$WeekHours <- NULL
new_historical_df$NumberProjectsLastYear <- NULL
new_historical_df$LastPromotion <- NULL
new_historical_df$DistanceHomeOffice <- NULL
new_historical_df$JobDedication <- NULL
new_historical_df$AfterHours <- NULL
new_historical_df$JobPerformance <- NULL
new_historical_df$Churn <- NULL
new_historical_df$FacilitiesSatisfaction <- NULL
new_historical_df$RoleSatisfaction <- NULL
new_historical_df$HierarchySatisfaction <- NULL
new_historical_df$BalanceWork.Life <- NULL
new_historical_df$DepartmentName <- NULL
new_historical_df$NewGender <- NULL
new_historical_df$NewChurn <- NULL
new_historical_df$NewBalanceWorkLife <- NULL
new_historical_df$NewRoleSatisfaction <- NULL
new_historical_df$NewFacilitiesSatisfaction <- NULL
new_historical_df$NewHierarchySatisfaction <- NULL
new_historical_df$NewJobDedication <- NULL
new_historical_df$NewJobPerformance <- NULL
new_historical_df$NewEducation <- NULL
new_historical_df$totalSatisfaction <- NULL
new_historical_df$totalJobRewarding <- NULL
new_historical_df$YearPerc <- NULL

new_historical_df$Churn <- historical_hr_churn_satisfaction$NewChurn
new_historical_df$Churn <- NULL
new_historical_df$Age = as.numeric(as.character(new_historical_df$Age))
new_historical_df$RoleSatisfaction = as.numeric(as.character(new_historical_df$RoleSatisfaction))
new_historical_df$NewAfterHours = as.numeric(as.character(new_historical_df$NewAfterHours))

# correlation Matrix
cormat <- round(cor(new_historical_df),2)
head(cormat)

library(corrplot)
corrplot(cormat, method="color")
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile() +
  theme(axis.text.x = element_text(angle = 90))
#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------NORMALIZE DATA-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#
norm_params <- preProcess(new_historical_df, method=c('range'))
norm_data <- predict(norm_params, new_historical_df)
str(norm_data)

# transform new variables to apply the log
norm_data$NewAfterHours <- as.numeric(norm_data$NewAfterHours)
norm_data$NewGender <- as.numeric(norm_data$NewGender)
norm_data$NewBalanceWorkLife <- as.numeric(norm_data$NewBalanceWorkLife)
norm_data$RoleSatisfaction <- as.numeric(norm_data$RoleSatisfaction)

# to do the log into the normal dataset
norm_data$class <- NULL

# apply the log
log_data = log1p(norm_data)
hist(log_data$Age)

str(new_historical_df)
#--------------------------------------------- 4 -SAVE DATA FRAME & LOAD
save(log_data,file="log_data.Rda")
load("log_data.Rd")

#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------PCA-------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
log_data$Churn <- historical_hr_churn_satisfaction$NewChurn
log_data$Churn <- as.numeric(as.character(historical_hr_churn_satisfaction$NewChurn))
str(log_data)

set.seed(3458)
set.seed(88)
set.seed(666)
ind <- sample(2, nrow(log_data), replace=TRUE, prob=c(0.7,0.3))
trainData=log_data[ind==1,]
testData=log_data[ind==2,]

library(caret)
library(dplyr)
pca_trainset=trainData[, -29]
pca_testset=testData[, -29]
str(pca_trainset)


pca_data = prcomp(pca_trainset, scale. = T)
print(pca_data)
pr_var=(pca_data$sdev)^2

#% of variance
prop_varex <- pr_var / sum(pr_var)

plot(prop_varex, xlab="Principal Components", ylab="Proportion of Variance Explained", type="b")
table(prop_varex)

plot(cumsum(prop_varex), xlab="Principal Components", ylab="Cumulative Proportion of Variance Explained", type="b")

S<- cov(log_data)
eigen_decom<-eigen(S)
perc_explained <- eigen(cov(log_data))$values/sum(diag(cov(log_data)))
cum_explain<- cumsum(perc_explained)
table<-cbind(eigen(cov(log_data))$values, perc_explained, cum_explain)
table

library("devtools")
library("factoextra")
get_eig(pca_data)
fviz_eig(pca_data, addlabels=TRUE, hjust = -0.3) + ylim(0, 30) + scale_fill_manual(values=c("#94c3e0"))

#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------STEPWISE LOGISTIC REGRESSION MODEL-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#

train = data.frame(class=trainData$Churn, pca_data$x)
t=as.data.frame(predict(pca_data, newdata=pca_testset))
new_trainset=train[, 1:10]
new_testset=t[, 1:9]


library(ROCR)
model_glm <- glm(class ~ ., data = new_trainset, family = binomial)
model_step <- step(model_glm, direction = "both", steps = 10000, trace = FALSE)
predictions_step <- predict(model_step, new_testset, type = "response")

#DECIDE CUTOFF
pred_step <- prediction(predictions_step, testData$Churn)
plot(performance(pred_step, "tpr", "fpr"), colorize = TRUE)

auc_step <- performance(pred_step, "auc")
auc_step
plot(performance(auc_step))

roc_step <- roc(response = testData$Churn, predictor = predictions_step)
c <- coords(roc_step, "best", "threshold")
c

Churn_step <- ifelse(predictions_step >= c[[1]], 
                     "1", "0")
cm_log <- confusionMatrix(actual= testData$NewChurn, Churn_step)
cm_log

# Results
results_step = data.frame( actual = testData$Churn, 
                      prediction = Churn_step)

library( caret )
library(ggthemr)
t_final_step = table( results_step ) 
print( confusionMatrix( t_final_step ) )

library(pROC)
# Compute roc
res.roc <- roc(response = testData$Churn, predictor = predictions_step)
plot.roc(res.roc, print.auc = TRUE)

##################################################################### NOVO LOGISTIC Normal
set.seed(345)
set.seed(88)
set.seed(666)
model_glm <- glm(class ~ ., data = new_trainset, family = binomial("logit"), maxit=100)

summary(model_glm)
model_glm.probs <- predict(model_glm, new_testset,type="response")
model_glm.pred <- ifelse(model_glm.probs > 0.5, "1", "0")

#confusion matrix
table(model_glm.pred, testData$Churn)
#Accuracy
mean(model_glm.pred == testData$Churn)


# Results
results = data.frame( actual = testData$Churn, 
                      prediction = model_glm.pred )

library( caret )
t_final = table( results ) 
print( confusionMatrix( t_final ) )

actual <- as.factor(testData$Churn)
pred <- as.factor(as.character(model_glm.pred))
precision(actual,pred)

library(pROC)
# Compute roc
res.roc <- roc(response = testData$Churn, predictor = model_glm.probs)
plot.roc(res.roc, print.auc = TRUE)

log_data$Churn = as.factor(log_data$Churn)
str(log_data)


#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------RANDOM FOREST-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#

fit<- randomForest(as.factor(class)~., data=new_trainset, importance=TRUE, ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, new_testset)

#confusion matrix
t = table(Prediction, testData$Churn)
#Accuracy
mean(Prediction == testData$Churn)

print( confusionMatrix( t ) )

library(pROC)
# Compute roc
res.roc <- roc(response = testData$Churn, predictor = as.numeric(as.character(Prediction)))
plot.roc(res.roc, print.auc = TRUE)
#-------------------------------------------------------------------------------------------------------#
#---------------------------------------------DECISION TREES-----------------------------------------#
#-------------------------------------------------------------------------------------------------------#
### CART
library(rpart)
library(rpart.plot)
rp <- rpart(class ~ ., data = new_trainset, method = "class")
predictions_rp <- predict(rp, new_testset, type = "class")
rpart.plot(rp, type=1, extra=100, branch.lty=3, box.palette="RdYlGn", tweak = 1.2, fallen.leaves = FALSE)

# Results
results = data.frame( actual = testData$Churn, 
                      prediction = predictions_rp )

library( caret )
t_final = table( results ) 
print( confusionMatrix( t_final ) )

prdriction_cart = as.numeric(as.character(predictions_rp))
res.roc <- roc(response = testData$Churn, predictor = prdriction_cart)
plot.roc(res.roc, print.auc = TRUE)

#-------------------------C5.0 FUNCIONA COM PCA
#train data
library(C50)
churn_model <- C5.0(x=new_trainset[,-1], y=as.factor(new_trainset$class))
churn_model

summary(churn_model)

#Evaluate model performance
churn_pred <- predict(churn_model, new_testset)

t = table(churn_pred, testData$Churn)
#Accuracy
mean(churn_pred == testData$Churn)

print( confusionMatrix( t ) )
library(gmodels)
gmodels::CrossTable(testData$Churn, churn_pred,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual churn', 'predicted churn'))

res.roc <- roc(response = testData$Churn, predictor = as.numeric(as.character(churn_pred)))
plot.roc(res.roc, print.auc = TRUE)
#-------------------------------------------------------------------------------------------------------#
#--------------------------------------Neural Network------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
## DESCONSIDERA ISSO EU FIZ ALTERAÇÕES, USA A QUE VC FEZ PARA AS 8 VARS

log_data$class = historical_hr_churn_satisfaction$NewChurn
log_data$class = as.numeric(as.character(log_data$class))
index <- sample(2, nrow(log_data), replace=TRUE, prob=c(0.7,0.3))

str(log_data)
set.seed(345)
set.seed(88)
set.seed(666)
library(magrittr)
library( dplyr )
trainset = log_data[ index==1, ]
test = log_data[ index==2, ]
testset = test %>% select( -Churn )

# Building a neural network (NN)
library( neuralnet )
n = names( trainset )
f = as.formula( paste( "class ~", paste( n[!n %in% "class"], collapse = "+" ) ) )
nn = neuralnet( f, trainset,learningrate = 0.01, hidden = 4, linear.output = TRUE, threshold = 0.01 )

plot( nn, rep = 'best' )

# Testing the result output
nn.results = compute( nn, testset )

results = data.frame( actual = test$class, prediction = round( nn.results$net.result ) )

# Confusion matrix
library( caret )
t = table( results )
print( confusionMatrix( t ) )

#-------------------------------------------------------------------------------------------------------#
#--------------------------------------Neural Network WITH PCA------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
train = data.frame(class=pca_trainset$Churn, pca_data$x)
t=as.data.frame(predict(pca_data, newdata=pca_testset))
data.frame()
new_trainset=train[, 1:10]
new_testset=t[, 1:9]
new_trainset$class = as.numeric(as.character(new_trainset$class))
str(new_trainset)

library(neuralnet)
n=names(new_trainset)
f=as.formula(paste("class~", paste(n[!n %in% "class"],
                                   collapse = "+")))

nn= neuralnet(f, new_trainset, hidden = 4,learningrate = 0.01,  linear.output=FALSE, threshold=0.01)

plot(nn, rep="best")

nn.results=compute(nn, new_testset)

#results
results=data.frame(actual=pca_testset$Churn, prediction = round(nn.results$net.result))


#confusion matrix
t = table(results)
print(t)

library( caret )
t = table( results ) 
print( confusionMatrix( t ) )

predv = as.numeric(as.character(round(nn.results$net.result)))

str(testData)
res.roc <- roc(response = testData$Churn, predictor = predv)
plot.roc(res.roc, print.auc = TRUE)



