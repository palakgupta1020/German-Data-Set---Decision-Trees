
# install.packages("rpart")
# install.packages('rpart.plot')
# install.packages('lift')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('ROCR')

library('rpart')
library('rpart.plot')
library('lift')
library('dplyr')
library('ggplot2')
library('ROCR')

## User defined Functions
# m1 is a confusion matrix table

# Accuracy Function
# cm_accuracy <- function(m1 ){
#   accuracy <- (m1[1,1]+ m1[2,2])/sum(m1)
#   return(accuracy)
# }

#  Precision Function
# cm_precision <- function(m1 ){
#   precision <- (m1[2,2]/(m1[2,2]+m1[2,1])) ## TP/(TP+FP)
#              return(precision)
# }

# Recall Funtion
# cm_recall <- function(m1 ){
#   recall <- (m1[2,2]/(m1[2,2]+m1[1,2]))  ## TP/(TP+FN)
#   return(recall)
# }

# 
# cm_f1Score <- function(m1 ){
#   precision = cm_precision(m1)
#   recall = cm_recall(m1)
#   f1Score <- (2*precision*recall)/(precision+recall)
#   return(f1Score)
# }

## import data
credit_rating_untreated <- read.csv("C:/Users/palak/OneDrive/Desktop/Data Mining/Assignment-1/credit_rating.csv", header = TRUE)
summary(credit_rating_untreated)

## copy data to another Dataframe
credit_rating_v1 <- credit_rating_untreated

## Converrt required variables to factors

credit_rating_v1$obs. <-as.factor(credit_rating_v1$obs.)
credit_rating_v1$chk_acct<-as.factor(credit_rating_v1$chk_acct)
credit_rating_v1$co.applicant<-as.factor(credit_rating_v1$co.applicant)
credit_rating_v1$education<-as.factor(credit_rating_v1$education)
credit_rating_v1$employment<-as.factor(credit_rating_v1$employment)
credit_rating_v1$foreign<-as.factor(credit_rating_v1$foreign)
credit_rating_v1$furniture<-as.factor(credit_rating_v1$furniture)
credit_rating_v1$guarantor<-as.factor(credit_rating_v1$guarantor)
credit_rating_v1$history<-as.factor(credit_rating_v1$history)
credit_rating_v1$job<-as.factor(credit_rating_v1$job)
credit_rating_v1$male_div<-as.factor(credit_rating_v1$male_div)
credit_rating_v1$male_mar_or_wid<-as.factor(credit_rating_v1$male_mar_or_wid)
credit_rating_v1$male_single<-as.factor(credit_rating_v1$male_single)
credit_rating_v1$new_car<-as.factor(credit_rating_v1$new_car)
credit_rating_v1$other_install<-as.factor(credit_rating_v1$other_install)
credit_rating_v1$own_res<-as.factor(credit_rating_v1$own_res)
credit_rating_v1$present_resident<-as.factor(credit_rating_v1$present_resident)
credit_rating_v1$prop_unkn_none<-as.factor(credit_rating_v1$prop_unkn_none)
credit_rating_v1$radio.tv<-as.factor(credit_rating_v1$radio.tv)
credit_rating_v1$real_estate<-as.factor(credit_rating_v1$real_estate)
credit_rating_v1$rent<-as.factor(credit_rating_v1$rent)
credit_rating_v1$response<-as.factor(credit_rating_v1$response)
credit_rating_v1$retraining<-as.factor(credit_rating_v1$retraining)
credit_rating_v1$sav_acct<-as.factor(credit_rating_v1$sav_acct)
credit_rating_v1$telephone<-as.factor(credit_rating_v1$telephone)
credit_rating_v1$used_car<-as.factor(credit_rating_v1$used_car)

## 1.3 Variable Summry
summary(credit_rating_v1)

## 1.1	What is the proportion of "Good" to "Bad" cases
table(credit_rating_v1$response)
plot(credit_rating_v1$response)

## Answer - 700 - 1|| 300 - 0

## since for variable new_car, used_car, furniture, education, retraining, radio.tv, 
## we have more than 75% of data missing we remove this variables from analysis

credit_rating_v2 <- credit_rating_v1 %>%
  select(-c(obs. ,new_car,	used_car,	furniture,	radio.tv,	education,	retraining))


## Examine Variable Plots - Numeric Variables 

####/* need to be made via ggplot */####

hist(credit_rating_v2$amount,ylab = "# of Cust",xlab = "Amount", main ="Amount", ylim = c(0,500))
boxplot(credit_rating_v2$duration,ylab = "Duration", main ="Duration")
boxplot(credit_rating_v2$num_credits,ylab = "num_credits", main ="age")
hist(credit_rating_v2$num_credits,ylab = "# of Cust",xlab = "num_credits", main ="num_credits")
hist(credit_rating_v2$num_dependents,ylab = "# of Cust",xlab = "num_dependents", main ="num_dependents")
hist(credit_rating_v2$install_rate,ylab = "# of Cust",xlab = "Install_rate", main ="Install_rate")
hist(credit_rating_v2$age,ylab = "# of Cust",xlab = "age", main ="age" )

################################################## Part 2: Descriptive ##################################################

summary(credit_rating_v2)

################################################# Full tree
###### Gini Index

set.seed(100)
rpModel1 = rpart(response ~ . , data=credit_rating_v2, method="class", parms = list(split ='gini'))
printcp (rpModel1)
rpart.plot (rpModel1)
rpModel1$variable.importance
summary(rpModel1)


## Accuracy for Gini Index Model
pred_gini = predict(rpModel1,credit_rating_v2, type="class")
cm_gini<-table(pred_gini, true=credit_rating_2$response)
cm_accuracy(cm_gini)

###### Information Gain

set.seed(200) 
rpModel1_info=rpart(response ~ . , data=credit_rating_v2, method="class", parms = list(split ='information'))
rpart.plot(rpModel1_info)

## Accuracy for Information Gain Model

pred_info = predict(rpModel1_info,credit_rating_v2, type="class")
cm_info<-table(pred_info, true=credit_rating_2$response)
cm_accuracy(cm_info)


###### Lift Chart using Information Gain Model

## Calculate probability(score) of for each observation per rpModel1_info on the training data
pred_Train_Prob=predict(rpModel1_info, credit_rating_v2, type='prob')
head(predTrnProb)


trnSc <- subset(credit_rating_v2, select=c("response"))  # selects the response column into trnSc
trnSc$score<-pred_Train_Prob[, 2]  #add a column named 'Score' with prob(1) values in the first column of pred_Train_Prob

trnSc<-trnSc[order(trnSc$score, decreasing=TRUE),]

# str(trnSc)

trnSc$response<-as.numeric(as.character(trnSc$response))
trnSc$cumDefault<-cumsum((trnSc$response))

plot(seq(nrow(trnSc)), trnSc$cumDefault,type = "l", xlab='#cases', ylab='#1')

### Decile Lift

#Divide the data into 10 (for decile lift) equal groups
trnSc["bucket"]<-ntile(-trnSc[,"score"], 10)


decGroups<-group_by(trnSc, bucket)
decLifts<-summarise(decGroups, count=n(), num_1=sum(response))
decLifts<-decLifts %>% 
          mutate(defRate=num_1/count, 
                 cumDefRate=cumsum(num_1)/cumsum(count),       
                 lift=cumDefRate/(sum(num_1)/sum(count)) 
                 )
plot(decLifts$bucket, decLifts$lift, xlab="deciles", ylab="Cumulative Decile Lift", type="l")
barplot(decLifts$num_1, main="numDefaults by decile", xlab="deciles", ylim = c(0,100))

decLifts

################################################## Part 3: Prediction ##################################################

set.seed(200)
nr=nrow(credit_rating_v2)
trnIndex = sample(1:nr, size = round(0.5*nr), replace=FALSE)
credit_rating_Train=credit_rating_v2[trnIndex,]
credit_rating_Test = credit_rating_v2[-trnIndex,] 

dim(credit_rating_Train)
dim(credit_rating_Test)

###### gini Index
set.seed(200)
fit <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
             control = rpart.control(cp=0))
printcp(fit)

rpart.plot(fit) ## cp = 0

pred_train <- predict(fit,credit_rating_Train, type = 'class')
mean(pred_train==credit_rating_Train$response) ## 0.838 -- cp =0
pred_test <- predict(fit,credit_rating_Test, type = 'class')
mean(pred_test==credit_rating_Test$response) ## 0.7 -- cp =0
cm <- table(pred = pred_test, true=credit_rating_Test$response)

n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
base_accuracy = sum(diag) / n 
base_precision = as.data.frame(diag / colsums)
base_recall = as.data.frame(diag / rowsums)

##################################### Pre Pruning ###########################################
################ Min Bucket #######################


minbucket <- c(5,10,20,30,40,50,60,70,80,90)

accuracy_train <- c()
precision_train <- data.frame(c(0,0))
recall_train <- data.frame(c(0,0))

accuracy <- c()
precision <- data.frame(c(0,0))
recall <- data.frame(c(0,0))


for(i in 1:length(minbucket)){
  set.seed(200)
  ## Create model
  fit_pre_prune <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
                         control = rpart.control(minbucket = minbucket[i], cp = 0))
  
  pred_train <- predict(fit_pre_prune,credit_rating_Train, type = 'class')
  cm <- table(pred = pred_train, true=credit_rating_Train$response)
  n = sum(cm) # number of instances
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 2, sum) # number of instances per class
  colsums = apply(cm, 1, sum) # number of predictions per class
  accuracy_train[i] = sum(diag) / n 
  prec1 = as.data.frame(diag / colsums)
  precision_train = cbind(precision, prec1)
  rec1 = as.data.frame(diag / rowsums)
  recall_train = cbind(recall,rec1)
  
  
  ## Prediction on test
  pred_test <- predict(fit_pre_prune,credit_rating_Test, type = 'class')
  cm <- table(pred = pred_test, true=credit_rating_Test$response)
  n = sum(cm) # number of instances
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 2, sum) # number of instances per class
  colsums = apply(cm, 1, sum) # number of predictions per class
  accuracy[i] = sum(diag) / n 
  prec = as.data.frame(diag / colsums)
  precision = cbind(precision, prec)
  rec = as.data.frame(diag / rowsums)
  recall = cbind(recall,rec)
}


prec_minbucket<- precision
acc_minbucket <- accuracy ## accuracy highest for minbucket = 40
rec_minbucket <- recall

res1 <- data.frame(minbucket,accuracy_train,accuracy)

ggplot(res1, aes(minbucket)) + 
  geom_line(aes(y = accuracy, colour = "Test Accuracy")) + 
  geom_line(aes(y = accuracy_train, colour = "Training Accuracy"))

acc_minbucket
## MIN BUCKET = 10

################ MinSplit #######################

minsplit <- c(20,30,40,50,60,70,80,90,100,150,200)

accuracy_train <- c()
precision_train <- data.frame(c(0,0))
recall_train <- data.frame(c(0,0))

accuracy <- c()
precision <- data.frame(c(0,0))
recall <- data.frame(c(0,0))

for(i in 1:length(minsplit)){
  set.seed(100)
  ## Create model
  fit_pre_prune <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
                         control = rpart.control(minsplit = minsplit[i], cp = 0, minbucket = 10))
  
  pred_train <- predict(fit_pre_prune,credit_rating_Train, type = 'class')
  cm <- table(pred = pred_train, true=credit_rating_Train$response)
  n = sum(cm) # number of instances
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 2, sum) # number of instances per class
  colsums = apply(cm, 1, sum) # number of predictions per class
  accuracy_train[i] = sum(diag) / n 
  prec1 = as.data.frame(diag / colsums)
  precision_train = cbind(precision, prec1)
  rec1 = as.data.frame(diag / rowsums)
  recall_train = cbind(recall,rec1)
  
  ## Prediction on test
  pred_test <- predict(fit_pre_prune,credit_rating_Test, type = 'class')
  cm <- table(pred = pred_test, true=credit_rating_Test$response)
  n = sum(cm) # number of instances
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 2, sum) # number of instances per class
  colsums = apply(cm, 1, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  accuracy[i] = sum(diag) / n 
  prec = as.data.frame(diag / colsums)
  precision = cbind(precision, prec)
  rec = as.data.frame(diag / rowsums)
  recall = cbind(recall,rec)
}

prec_minsplit<- precision
acc_minsplit <- accuracy
rec_minsplit <- recall

## Plot Graph
res2 <- data.frame(minsplit,accuracy_train,accuracy)

ggplot(res2, aes(minsplit)) + 
  geom_line(aes(y = accuracy, colour = "Test Accuracy")) + 
  geom_line(aes(y = accuracy_train, colour = "Training Accuracy"))
acc_minsplit

## Minsplit = 50


acc_minsplit

################ Maxdepth #######################

maxdepth  <- c(5,6,7,8,9,10,11,12,20)

accuracy_train <- c()
precision_train <- data.frame(c(0,0))
recall_train <- data.frame(c(0,0))

accuracy <- c()
precision <- data.frame(c(0,0))
recall <- data.frame(c(0,0))

for(i in 1:length(maxdepth )){
  set.seed(100)
  ## Create model
  set.seed(100)
  ## Create model
  fit_pre_prune <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
                         control = rpart.control(maxdepth = maxdepth[i], cp = 0, minbucket = 10, minsplit = 50))
  
  pred_train <- predict(fit_pre_prune,credit_rating_Train, type = 'class')
  cm <- table(pred = pred_train, true=credit_rating_Train$response)
  n = sum(cm) # number of instances
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 2, sum) # number of instances per class
  colsums = apply(cm, 1, sum) # number of predictions per class
  accuracy_train[i] = sum(diag) / n 
  prec1 = as.data.frame(diag / colsums)
  precision_train = cbind(precision, prec1)
  rec1 = as.data.frame(diag / rowsums)
  recall_train = cbind(recall,rec1)
  
  
  ## Prediction on test
  pred_test <- predict(fit_pre_prune,credit_rating_Test, type = 'class')
  cm <- table(pred = pred_test, true=credit_rating_Test$response)
  n = sum(cm) # number of instances
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 2, sum) # number of instances per class
  colsums = apply(cm, 1, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  accuracy[i] = sum(diag) / n 
  prec = as.data.frame(diag / colsums)
  precision = cbind(precision, prec)
  rec = as.data.frame(diag / rowsums)
  recall = cbind(recall,rec)
}

prec_maxdepth <- precision
acc_maxdepth  <- accuracy
rec_maxdepth  <- recall
## Plot Graph
res3 <- data.frame(maxdepth,accuracy_train,accuracy)

ggplot(res3, aes(maxdepth)) + 
  geom_line(aes(y = accuracy, colour = "Test Accuracy")) + 
  geom_line(aes(y = accuracy_train, colour = "Training Accuracy"))

#3 no effect of max depth

final_prec <- rbind(prec_minbucket,prec_minsplit, prec_maxdepth)
final_acc <- rbind(acc_minbucket,acc_minsplit, acc_maxdepth)
final_rec <- rbind(rec_minbucket,rec_minsplit, rec_maxdepth)

##################################### Post Pruning ###########################################

## Using cp value from base tree
## Complexity Parameter for Lowest cross validation error = 0.0102041
set.seed(200)
fit3 <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
              control = rpart.control(cp=0.0102041))
# printcp(fit3)


rpart.plot(fit3)

pred_train3 <- predict(fit3,credit_rating_Train, type = 'class')

mean(pred_train3==credit_rating_Train$response) ## 0.824-- no cp
pred_test3 <- predict(fit3,credit_rating_Test, type = 'class')
mean(pred_test3==credit_rating_Test$response) ## 0.74 -- no cp


cm <- table(pred = pred_test3, true=credit_rating_Test$response)

n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
precision = as.data.frame(diag / colsums)
recall = as.data.frame(diag / rowsums)

accuracy
precision
recall
##############################################################################################
##############################################################################################

## Comparing ROC and AUC of above selected models

#1.
set.seed(100)
fit2 <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
              control = rpart.control(minbucket = 10, minsplit = 50))
set.seed(200)
fit3 <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
              control = rpart.control(cp=0.0102041))


pred_test_v1 = predict(fit2, credit_rating_Test, type='prob')
pred_test_v2 = predict(fit3, credit_rating_Test, type='prob')

pred_v1<- prediction(pred_test_v1[, 2], credit_rating_Test$response)
pred_v2<- prediction(pred_test_v2[, 2], credit_rating_Test$response)

gainPerf_v1<-performance(pred_v1, "tpr", "rpp")
gainPerf_v2<-performance(pred_v2, "tpr", "rpp")

plot(gainPerf_v1)
plot(gainPerf_v2, add = TRUE, colorize = TRUE)

auc.tmp_v1 <- performance(pred_v1,"auc") 
auc_v1 <- as.numeric(auc.tmp_v1@y.values)

auc.tmp_v2 <- performance(pred_v2,"auc") 
auc_v2 <- as.numeric(auc.tmp_v2@y.values)

auc_v1
auc_v2

## Since auc for cp tree is higher,we go ahead and choose fit3 -50-50split

#####################################################################################################
#####################################################################################################

# 3(c) - Run same model on different training and test data

############################
set.seed(1000)
nr=nrow(credit_rating_v2)
trnIndex = sample(1:nr, size = round(0.5*nr), replace=FALSE)
credit_rating_Train=credit_rating_v2[trnIndex,]
credit_rating_Test = credit_rating_v2[-trnIndex,] 

dim(credit_rating_Train)
dim(credit_rating_Test)

###### gini Index
set.seed(1000)
fit <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
             control = rpart.control(cp=0))
printcp(fit)

############################
set.seed(1000)
fit3 <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
              control = rpart.control(cp=0.0102041))


pred_test_v1 = predict(fit2, credit_rating_Test, type='prob')
pred_test_v2 = predict(fit3, credit_rating_Test, type='prob')

pred_v2<- prediction(pred_test_v2[, 2], credit_rating_Test$response)

#gainPerf_v1<-performance(pred_v1, "tpr", "rpp")
gainPerf_v2<-performance(pred_v2, "tpr", "rpp")

plot(gainPerf_v2)


auc.tmp_v2 <- performance(pred_v2,"auc") 
auc_v2 <- as.numeric(auc.tmp_v2@y.values)

auc_v2

pred_test3 <- predict(fit3,credit_rating_Test, type = 'class')

mean(pred_test3==credit_rating_Test$response)
summary(fit3)

#####################################################################################################
#####################################################################################################
# 3(d)
##### 70-30 Split
set.seed(500)
nr=nrow(credit_rating_v2)
trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE)
credit_rating_Train_0.7=credit_rating_v2[trnIndex,]
credit_rating_Test_0.3 = credit_rating_v2[-trnIndex,] 

dim(credit_rating_Train_0.7)
dim(credit_rating_Test_0.3)

set.seed(500)
fit_70_30 <- rpart(response ~ ., data = credit_rating_Train_0.7, method = 'class', parms = list(split = 'gini'), 
                   control = rpart.control(cp=0))
printcp(fit_70_30)
fit_70_30_prune <- rpart(response ~ ., data = credit_rating_Train_0.7, method = 'class', parms = list(split = 'gini'), 
                         control = rpart.control(cp=0.0194175))

pred_test_70_30 <- predict(fit_70_30_prune,credit_rating_Test_0.3, type = 'class')
mean(pred_test_70_30==credit_rating_Test_0.3$response) ## 0.7 -- cp =0
cm <- table(pred = pred_test_70_30, true=credit_rating_Test_0.3$response)

n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
precision = as.data.frame(diag / colsums)
recall = as.data.frame(diag / rowsums)

##### 80-20 Split
set.seed(5000)
nr=nrow(credit_rating_v2)
trnIndex = sample(1:nr, size = round(0.8*nr), replace=FALSE)
credit_rating_Train_0.8=credit_rating_v2[trnIndex,]
credit_rating_Test_0.2 = credit_rating_v2[-trnIndex,] 

dim(credit_rating_Train_0.8)
dim(credit_rating_Test_0.2)

set.seed(500)
fit_80_20 <- rpart(response ~ ., data = credit_rating_Train_0.8, method = 'class', parms = list(split = 'gini'), 
                   control = rpart.control(cp=0))
printcp(fit_80_20)
fit_80_20_prune <- rpart(response ~ ., data = credit_rating_Train_0.8, method = 'class', parms = list(split = 'gini'), 
                         control = rpart.control(cp=0.0128205))

pred_test_80_20 <- predict(fit_80_20_prune,credit_rating_Test_0.2, type = 'class')
mean(pred_test_80_20==credit_rating_Test_0.2$response) ## 0.7 -- cp =0
cm <- table(pred = pred_test_80_20, true=credit_rating_Test_0.2$response)

n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
precision = as.data.frame(diag / colsums)
recall = as.data.frame(diag / rowsums)

pred_test_v1 = predict(fit_70_30_prune, credit_rating_Test_0.3, type='prob')
pred_test_v2 = predict(fit_80_20_prune, credit_rating_Test_0.2, type='prob')


pred_v1<- prediction(pred_test_v1[, 2], credit_rating_Test_0.3$response)
pred_v2<- prediction(pred_test_v2[, 2], credit_rating_Test_0.2$response)

gainPerf_v1<-performance(pred_v1, "tpr", "rpp")
gainPerf_v2<-performance(pred_v2, "tpr", "rpp")

plot(gainPerf_v1)
plot(gainPerf_v2, add = TRUE, colorize = TRUE)

auc.tmp_v1 <- performance(pred_v1,"auc") 
auc_v1 <- as.numeric(auc.tmp_v1@y.values)

auc.tmp_v2 <- performance(pred_v2,"auc") 
auc_v2 <- as.numeric(auc.tmp_v2@y.values)

auc_v1
auc_v2

#####################################################################################################


################################################## Part 4: Cost Benefits ##################################################


costMatrix <- matrix(c(0,1,5, 0), byrow=TRUE, nrow=2)
rownames(costMatrix) <- c('Predict Bad','Predict Good')
colnames(costMatrix) <- c('Actual Bad','Actual Good')
costMatrix

#                   Actual Bad Actual Good
# Predict Bad           0           1
# Predict Good          5           0

ct[2,1]
costMatrix[2,1]
ct[1,2]                              
costMatrix[1,2]

CTHRESH <- c(0.3,0.4,0.5,0.6,0.7,0.8)

# set.seed(200)
# fit3 <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
#               control = rpart.control(cp=0.0102041))


## Considering Pruned Model with Cp for 50-50 Split
predProb=predict(fit3, credit_rating_Test, type='prob')

# final_cost <- c()
# accuracy <- c()

# for(i in 1:length(CTHRESH)){
pred = ifelse(predProb[,'1'] >= CTHRESH[7], '1', '0')
ct = as.matrix(table( pred = pred, true=credit_rating_Test$response))
n = sum(ct) # number of instances
diag = diag(ct) # number of correctly classified instances per class 
accuracy[6] = sum(diag) / n
final_cost[6] <- (ct[2,1]*costMatrix[2,1]) + (ct[1,2]+costMatrix[1,2])
# }
ct
final_cost
accuracy

## Plot Graph
cost <- data.frame(CTHRESH,final_cost)
cost
ggplot(cost, aes(CTHRESH)) + 
  geom_line(aes(y = final_cost, colour = "Final Cost(in Hundreds)"))
#######################################

# 4(a) ROC curve for optimal value of threshold

# set.seed(200)
# fit3 <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
#               control = rpart.control(cp=0.0102041))

pred_test_v2 = predict(fit3, credit_rating_Test, type='prob')

pred_v2<- prediction(pred_test_v2[, 2], credit_rating_Test$response)
gainPerf_v2<-performance(pred_v2, "tpr", "rpp")

plot(gainPerf_v2)
auc.tmp_v2 <- performance(pred_v2,"auc") 
auc_v2 <- as.numeric(auc.tmp_v2@y.values)
auc_v2

#######################################

# 4(b) Theortical Threshold

# p* = (c10 - c00)/(c10 - c00 + c01 - c11)
### Calculate Optimal Threshold
p_star = (costMatrix[2,1] - costMatrix[1,1])/(costMatrix[2,1] - costMatrix[1,1] + costMatrix[1,2] - costMatrix[2,2])

p_star 
# 0.8333333

#######################################

# 4(c) Use misclassification cost to build tree models

## RPART

set.seed(200)
fit_v1 <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini', loss = costMatrix),
                control = rpart.control(cp=0.0102041))


#######################################

# 4(c) Use misclassification cost to build tree models

## RPART

costMatrix <- matrix(c(0,1,5, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')
costMatrix
set.seed(200)
rpTree = rpart(response ~ ., data=credit_rating_Train, method="class", 
               parms = list( prior = c(.70,.30), loss = costMatrix, split = "gini"),control = rpart.control(cp=0.0102041))

pred_test <- predict(rpTree,credit_rating_Test, type = 'class')
cm_test_gini <- table(pred = pred_test, true=credit_rating_Test$response)
acc <- cm_accuracy(cm_test_gini)
prec<- cm_precision(cm_test_gini)
rec <- cm_recall(cm_test_gini)
acc
prec
rec
summary(mod1)


install.packages('C50')
library(C50)


cost_mat <- matrix(c(0, 3, 1, 0), nrow = 2)
rownames(cost_mat) <- colnames(cost_mat) <- c("1", "0")
cost_mat

set.seed(200)
c50Tree = C5.0(response ~ ., data = credit_rating_Train,costs = cost_mat)
plot(c50Tree)

pred_test <- predict(c50Tree,credit_rating_Test, type = 'class')
cm_test_gini <- table(pred = pred_test, true=credit_rating_Test$response)
acc <- cm_accuracy(cm_test_gini)
prec<- cm_precision(cm_test_gini)
rec <- cm_recall(cm_test_gini)
acc
prec
rec
summary(c50Tree)

############################################################## Part 6  ######################################################
######### Q6 Cummulative Cost/ Benefit Analysis

set.seed(200)
fit3 <- rpart(response ~ ., data = credit_rating_Train, method = 'class', parms = list(split = 'gini'), 
              control = rpart.control(cp=0.0102041))

PROFITVAL=100
COSTVAL=-500

scoreTst=predict(fit3,credit_rating_Test, type="prob")[,'1'] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, credit_rating_Test$response)
#check what is in prLifts ....head(prLifts)

prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`credit_rating_Test$response`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))

plot(prLifts$cumProfits)

#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))
#################################################################################################