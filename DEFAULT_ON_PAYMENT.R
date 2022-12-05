assignment<-read.csv('Data_for_Logistic_Regression.csv',header = TRUE)
assignment
str(assignment)
assignment$Default_On_Payment<-as.factor(assignment$Default_On_Payment)
str(assignment)
cols_cat<-c("Status_Checking_Acc","Credit_History","Purposre_Credit_Taken","Savings_Acc","Years_At_Present_Employment","Inst_Rt_Income","Marital_Status_Gender","Other_Debtors_Guarantors","Current_Address_Yrs","Property","Other_Inst_Plans","Housing","Num_CC","Job","Dependents","Telephone","Foreign_Worker")
class(cols_cat)
assignment[cols_cat]<-lapply(assignment[cols_cat],factor)# IMPORTANT STEP#
str(assignment)
summary(assignment)
data.frame(colSums(is.na(assignment)))
library(Hmisc)
library(sqldf)
library(dplyr)
IVCal<-function(variable,target,data,groups){
  data[,"rank"] <- cut2(assignment[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}
num<-assignment[ ,c('Duration_in_Months','Credit_Amount','Age','Default_On_Payment')]  
num
a1<-IVCal("Duration_in_Months","Default_On_Payment",num,groups = 10)
a2<-IVCal("Credit_Amount","Default_On_Payment",num,groups = 10)
a3<-IVCal("Age","Default_On_Payment",num,groups = 10)
IV_num<-data.frame(rbind(a1,a2,a3))
View(IV_num)
category<-select(assignment,-c('Duration_in_Months','Credit_Amount','Age','Count','Customer_ID'))
category
CA<-function(target,variable,data){
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}
c1<-CA("Default_On_Payment","Status_Checking_Acc",category)
c2<-CA("Default_On_Payment","Credit_History",category)
c3<-CA("Default_On_Payment","Purposre_Credit_Taken",category)
c4<-CA("Default_On_Payment","Savings_Acc",category)
c5<-CA("Default_On_Payment","Years_At_Present_Employment",category)
c6<-CA("Default_On_Payment","Inst_Rt_Income",category)
c7<-CA("Default_On_Payment","Marital_Status_Gender",category)
c8<-CA("Default_On_Payment","Other_Debtors_Guarantors",category)
c9<-CA("Default_On_Payment","Current_Address_Yrs",category)
c10<-CA("Default_On_Payment","Property",category)
c11<-CA("Default_On_Payment","Other_Inst_Plans",category)
c12<-CA("Default_On_Payment","Housing",category)
c13<-CA("Default_On_Payment","Num_CC",category)
c14<-CA("Default_On_Payment","Job",category)
c15<-CA("Default_On_Payment","Dependents",category)
c16<-CA("Default_On_Payment","Telephone",category)
c17<-CA("Default_On_Payment","Foreign_Worker",category)
IV_Cat<-data.frame(rbind(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17))
View(IV_Cat)
FINAL_IV<-data.frame(rbind(IV_num,IV_Cat))
View(FINAL_IV)
library(caTools)
set.seed(123)
split<-sample.split(assignment, SplitRatio = 0.8)
data_train<-subset(assignment, split==TRUE)
dim(data_train)
data_test<-subset(assignment,split==FALSE)
dim(data_test)
train_model<-glm(Default_On_Payment~Duration_in_Months+Credit_History, data = data_train,family = binomial())
summary(train_model)
train_model1<-glm(Default_On_Payment~Duration_in_Months+I(Credit_History=='A32')+I(Credit_History=='A33')+I(Credit_History=='A34'), data = data_train,family = binomial())
summary(train_model1)
library(car)
vif(train_model1)
train_model2<-glm(Default_On_Payment~Duration_in_Months+I(Credit_History=='A33')+I(Credit_History=='A34'), data = data_train,family = binomial())
vif(train_model2)
summary(train_model2)
library(aod)
wald.test(b=coef(train_model2),Sigma = vcov(train_model2),Terms = 1:3)
wald.test(b=coef(train_model1),Sigma = vcov(train_model1),Terms = 1:4)
wald.test(b=coef(train_model),Sigma = vcov(train_model),Terms = 1:5)
1-pchisq(deviance(train_model2),df.residual(train_model2))
base_model<-glm(Default_On_Payment~., data = data_train,family = binomial())
summary(base_model)
new_model<-glm(Default_On_Payment~Duration_in_Months+Credit_Amount+Credit_History+Purposre_Credit_Taken+Savings_Acc+Property, data = data_train,family = binomial())
summary(new_model)
vif(new_model)
wald.test(b=coef(new_model),Sigma = vcov(new_model),Terms = 1:22)
1-pchisq(deviance(new_model),df.residual(new_model))
new_model_1<-glm(Default_On_Payment~Duration_in_Months+Credit_Amount+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")+I(Purposre_Credit_Taken=='A41')+I(Purposre_Credit_Taken=='A410')+I(Purposre_Credit_Taken=='A42')+I(Purposre_Credit_Taken=='A43')+I(Purposre_Credit_Taken=='A48')+I(Purposre_Credit_Taken=='A49')+Savings_Acc+Property, data = data_train,family = binomial())
summary(new_model_1)
vif(new_model_1)
wald.test(b=coef(new_model_1),Sigma = vcov(new_model_1),Terms = 1:18)
#Lagrange Multiplier or Score Test#
modelchi<-new_model_1$null.deviance-new_model_1$deviance
chidf<-new_model_1$df.null-new_model_1$df.residual
chisq.prob<-1-pchisq(modelchi,chidf)
format(round(chisq.prob,2), nsmall=2)
#LACKFIT VARIANCE#
residuals(new_model_1)
residuals(new_model_1,"pearson")
sum(residuals(new_model_1,type="pearson")^2)
deviance(new_model_1)
lackfit_variance<-1-pchisq(deviance(new_model_1),df.residual(new_model_1))
lackfit_variance
coefficients(new_model_1)
exp(coefficients(new_model_1))
coefficients<-exp(coefficients(base_model))
class(coefficients)
phase_3_model<-glm(Default_On_Payment~Status_Checking_Acc+Duration_in_Months+I(Credit_History=='A33')+I(Credit_History=='A34')+I(Purposre_Credit_Taken=='A41')+I(Purposre_Credit_Taken=='A410')+I(Purposre_Credit_Taken=='A42')+I(Purposre_Credit_Taken=='A43')+I(Purposre_Credit_Taken=='A48')+I(Purposre_Credit_Taken=='A49')+Credit_Amount+Savings_Acc+I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=='3')+I(Inst_Rt_Income=='4')+I(Marital_Status_Gender=='A93')+I(Other_Debtors_Guarantors=='A103')+I(Current_Address_Yrs==2)+I(Current_Address_Yrs==3)+I(Current_Address_Yrs==3)+Property+Age+I(Other_Inst_Plans=='A143')+Housing+I(Num_CC==2)+
                      I(Dependents==2)+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data = data_train, family=binomial())
summary(phase_3_model)
vif(phase_3_model)
residuals(phase_3_model)
residuals(phase_3_model,"pearson")
sum(residuals(phase_3_model,type="pearson")^2)
deviance(phase_3_model)
lackfit_variance_phase3<-1-pchisq(deviance(phase_3_model),df.residual(phase_3_model))
wald.test(b=coef(phase_3_model),Sigma = vcov(phase_3_model),Terms = 1:35)
modelchi_phase3<-phase_3_model$null.deviance-phase_3_model$deviance
chidf_phase3<-phase_3_model$df.null-phase_3_model$df.residual
chisq.prob_phase3<-1-pchisq(modelchi_phase3,chidf_phase3)
format(round(chisq.prob_phase3,2), nsmall=2)
exp(coefficients(phase_3_model))
basemodel<-summary(base_model)
pratim<-glm(Default_On_Payment~Duration_in_Months+Credit_Amount+Credit_History+Purposre_Credit_Taken+Savings_Acc+Property+Status_Checking_Acc, data = data_train,family = binomial())
summary(pratim)
pratim2<-glm(Default_On_Payment~Duration_in_Months+Credit_Amount+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")+I(Purposre_Credit_Taken=='A41')+I(Purposre_Credit_Taken=='A410')+I(Purposre_Credit_Taken=='A42')+I(Purposre_Credit_Taken=='A43')+I(Purposre_Credit_Taken=='A48')+I(Purposre_Credit_Taken=='A49')+I(Savings_Acc=='A63')+I(Savings_Acc=='A64')+I(Savings_Acc=='A65')+Property+Status_Checking_Acc, data = data_train,family = binomial())
summary(pratim2)
vif(pratim2)
wald.test(b=coef(pratim2),Sigma = vcov(pratim2),Terms = 1:20)
residuals(pratim2)
residuals(pratim2,"pearson")
sum(residuals(pratim2,type="pearson")^2)
deviance(pratim2)
lackfit_variance_pratim2<-1-pchisq(deviance(pratim2),df.residual(pratim2))
lackfit_variance_pratim2
modelchi_pratim2<-pratim2$null.deviance-pratim2$deviance
chidf_pratim2<-pratim2$df.null-pratim2$df.residual
chisq.prob_pratim2<-1-pchisq(modelchi_pratim2,chidf_pratim2)
format(round(chisq.prob_pratim2,2), nsmall=2)
install.packages('generalhoslem',dependencies = TRUE)
library(generalhoslem)
logitgof(data_train$Default_On_Payment,fitted(pratim2),g=10)
logitgof(data_train$Default_On_Payment,fitted(new_model_1),g=10)
