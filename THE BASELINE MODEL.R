library(caret)
library(caTools)
library(pROC)
library(nnet)
library(ggplot2)
letters<-read.csv('letters_ABPR.csv', header = TRUE)
letters_backup<-letters
letters_backup$letter<-as.factor(letters_backup$letter)
class(letters_backup$letter)
str(letters_backup)
set.seed(1000)
split<-sample.split(letters_backup,SplitRatio = 0.7)
training<-subset(letters_backup,split==TRUE)
testing<-subset(letters_backup,split==FALSE)
dim(training)
dim(testing)
letters_backup$letter<-relevel(letters_backup$letter, ref = "P")
baselinemodel<-multinom(letter~., data = training)
summary(baselinemodel)
pred<-predict(baselinemodel, newdata=testing, type = "class")
pred
confusion<-table(predicted=pred, actual=testing$letter)
confusion
accuracy_rate<-sum(diag(confusion))/sum(confusion)
accuracy_rate
confusionMatrix(pred, testing$letter)
table(testing$letter,pred)
roc<-roc(response=testing$letter, predictor=factor(pred,ordered = TRUE), levels=rev(levels(testing$letter)))

# decision tree#
library(rpart)
cart1<-rpart(letter~.,data = training,method = "class")
library(rpart.plot)
par(mfrow=c(1,2))
prp(cart1)
cart2<-rpart(letter~.,data = training,method = "class",minbucket=0.01,cp=0.01)
prp(cart2)
cart_pred<-predict(cart2,newdata = testing, type = "class")
confusionMatrix(cart_pred,testing$letter)
table(testing$letter,cart_pred)
cart_pred1<-predict(cart1,newdata = testing, type = "class")
confusionMatrix(cart_pred1,testing$letter)
cart_pred_test<-predict(cart2,newdata = training,type = "class")
confusionMatrix(cart_pred_test,training$letter)
cart_pred_test1<-predict(cart1,newdata = training,type = "class")
confusionMatrix(cart_pred_test1,training$letter)
cart2roc<-roc(response=testing$letter,cart_pred[,4],levels=rev(levels(testing$letter)))
#FOR DETERMINING AUC#
cart_pred_prob<-predict(cart2,newdata = testing, type = "prob")
cart_pred_prob
multiclass.roc(testing$letter,cart_pred_prob)
#FOR DETERMINING AUC#ON TRAINING SET#
cart_pred_test_prob<-predict(cart2,newdata = training,type = "prob")
multiclass.roc(training$letter,cart_pred_test_prob)


#random forest#
library(randomForest)
forest<-randomForest(letter~., data = training, ntree=1000)
pred_forest<-predict(forest, newdata = testing, type = "class" )
confusionMatrix( pred_forest,testing$letter)
table(testing$letter,pred_forest)
varImp(forest)
varImpPlot(forest,main="variable importance chart")
