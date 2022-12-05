setwd('F:/R/LEAD PROJECT')
data<-read.csv('Leads_data.csv',as.is=TRUE  ,header = TRUE)
colnames(data)
unique(data$Lead_Origin)#categorical#
unique((data$Lead_Source))#categorical#
unique(data$Total_Visits)#continous#
unique(data$Do_Not_Email)#categorical#
unique(data$Do_Not_Call)#categorical#
unique(data$Total_Time_Spent_on_Website)#continous#
unique(data$Page_Views_Per_Visit)#continous#
unique(data$Last_Activity)#categorical#
unique(data$Country)#categorical#
unique(data$Specialization)#categorical#
unique(data$How_did_you_hear_about_X_Education)#categorical#
unique(data$What.is.your.current.occupation)#categorical#
unique(data$What.matters.most.to.you.in.choosing.a.course)#categorical#
unique(data$Search)#categorical#
unique(data$Magazine)#categorical#
unique(data$Newspaper.Article)#categorical#
unique(data$X.Education.Forums)#categorical#
unique(data$Digital.Advertisement)#categorical#
unique(data$Through.Recommendations)#categorical#
unique(data$Receive.More.Updates.About.Our.Courses)#categorical#
unique(data$Tags)#categorical#
unique(data$Update.me.on.Supply.Chain.Content)#categorical#
unique(data$Lead.Profile)#categorical#
unique(data$City)#categorical#
unique(data$Asymmetrique_Activity_Index)#categorical#
unique(data$Asymmetrique_Profile_Index)#categorical#
unique(data$Asymmetrique_Activity_Score)#continous#
unique(data$Asymmetrique_Profile_Score)#continous#
unique(data$I.agree.to.pay.the.amount.through.cheque)#categorical#
unique(data$A_free_copy_of_Mastering_The_Interview)#categorical#
unique(data$Last_Notable_Activity)#categorical#
str(data)
colSums(is.na(data))
library(dplyr)

#conversion of numeric/character variables to factor/categorical variables#

cols_cat<-select(data,-c('Total_Visits','Total_Time_Spent_on_Website','Page_Views_Per_Visit','Asymmetrique_Activity_Score','Asymmetrique_Profile_Score','Prospect_ID','Lead_Number'))
cols_cat<-lapply(cols_cat,as.factor)
class(cols_cat)
cols_cat<-as.data.frame(cols_cat)
colnames(cols_cat)
str(cols_cat)
cols_cont<-select(data,c('Total_Visits','Total_Time_Spent_on_Website','Page_Views_Per_Visit','Asymmetrique_Activity_Score','Asymmetrique_Profile_Score','Prospect_ID','Lead_Number'))
str(cols_cont)
data1<-cbind.data.frame(cols_cont,cols_cat)
data1
str(data1)
colSums(is.na(data1))
cols_new<-c('Prospect_ID','Lead_Number')
data1<-data1[,c(cols_new,setdiff(colnames(data1),cols_new))]
data$Lead_Source<-if_else(data$Lead_Source=="google","Google",data$Lead_Source)

#counting the blank rows in factor/CATEGORICAL variables#

dim(filter(data1,Specialization==""))#1438#
dim(filter(data1,Lead_Origin==""))
dim(filter(data1,Lead_Source==""))#36#
dim(filter(data1,Do_Not_Email==""))
dim(filter(data1,Do_Not_Call==""))
dim(filter(data1,Converted==""))
dim(filter(data1,Last_Activity==""))#103#
dim(filter(data1,Country==""))#2461#
dim(filter(data1,How_did_you_hear_about_X_Education==""))#2207#
dim(filter(data1,What.is.your.current.occupation==""))#2690#
dim(filter(data1,Search==""))
dim(filter(data1,Magazine==""))
dim(filter(data1,X.Education.Forums==""))
dim(filter(data1,Newspaper==""))
dim(filter(data1,Receive.More.Updates.About.Our.Courses==""))
dim(filter(data1,Tags==""))#3353#
dim(filter(data1,Lead.Quality==""))#4767#
dim(filter(data1,Update.me.on.Supply.Chain.Content==""))
dim(filter(data1,City==""))# 1420#
dim(filter(data1,Asymmetrique_Activity_Index==""))#4218#
dim(filter(data1,Asymmetrique_Profile_Index==""))#4218#
dim(filter(data1,I.agree.to.pay.the.amount.through.cheque==""))
dim(filter(data1,A_free_copy_of_Mastering_The_Interview==""))
dim(filter(data1,Last_Notable_Activity==""))
sort(table(data1$Lead_Source),decreasing = TRUE)
data2<-data1$Lead_Source
class(data2)

#imputation of missing values#
#LEAD SOURCE#
sort(table(data2),decreasing = TRUE)
data2<-if_else(data2=="","Google",data$Lead_Source)
data2<-as.factor(data2)
data1$Lead_Source<-data2
class(data2)

#LAST ACTIVITY#
sort(table(data1$Last_Activity),decreasing = TRUE)
data3<-data1$Last_Activity
data3<-if_else(data3=="","Email Opened",data$Last_Activity)
data3<-as.factor(data3)
data1$Last_Activity<-data3

sort(table(data1$Last_Activity),decreasing = TRUE)

#COUNTRY#
data4<-data$Country
sort(table(data4),decreasing = TRUE)
data4<-if_else(data4=="","India",data$Country)
data4<-as.factor(data4)
data1$Country<-data4

sort(table(data1$Country),decreasing = TRUE)

#How_did_you_hear_about_X_Education#

data5<-data$How_did_you_hear_about_X_Education
sort(table(data5),decreasing = TRUE)
data5<-if_else(data5=="","Select",data$How_did_you_hear_about_X_Education)
data5<-as.factor(data5)
data1$How_did_you_hear_about_X_Education<-data5

sort(table(data1$How_did_you_hear_about_X_Education),decreasing = TRUE)

#Specialization#
data6<-data$Specialization
sort(table(data6),decreasing = TRUE)
data6<-if_else(data6=="","Select",data$Specialization)
data6<-as.factor(data6)
data1$Specialization<-data6

#What.is.your.current.occupation#
data7<-data$What.is.your.current.occupation
sort(table(data7),decreasing = TRUE)
data7<-if_else(data7=="","Unemployed",data$What.is.your.current.occupation)
data7<-as.factor(data7)
data1$What.is.your.current.occupation<-data7

#What.matters.most.to.you.in.choosing.a.course#
data8<-data$What.matters.most.to.you.in.choosing.a.course
sort(table(data8),decreasing = TRUE)
data8<-if_else(data8=="","Better Career Prospects",data$What.matters.most.to.you.in.choosing.a.course)
data8<-as.factor(data8)
data1$What.matters.most.to.you.in.choosing.a.course<-data8

#City#
data9<-data$City
sort(table(data9),decreasing = TRUE)
data9<-if_else(data9=="","Mumbai",data$City)
data9<-as.factor(data9)
data1$City<-data9

#Tags#
data10<-data$Tags
sort(table(data10),decreasing = TRUE)
data10<-if_else(data10=="","Will revert after reading the email",data$Tags)
data10<-as.factor(data10)
data1$Tags<-data10

#Lead.Quality#
data11<-data$Lead.Quality
sort(table(data11),decreasing = TRUE)
data11<-if_else(data11=="","Might be",data$Lead.Quality)
data11<-as.factor(data11)
data1$Lead.Quality<-data11

#Lead.Profile#
data12<-data$Lead.Profile
data12<-if_else(data12=="","Select",data$Lead.Profile)
data12<-as.factor(data12)
data1$Lead.Profile<-data12

#Asymmetrique_Activity_Index#
data13<-data$Asymmetrique_Activity_Index
sort(table(data13),decreasing = TRUE)
data13<-if_else(data13=="","Medium",data$Asymmetrique_Activity_Index)
data13<-as.factor(data13)
data1$Asymmetrique_Activity_Index<-data13

#Asymmetrique_Profile_Index#
data14<-data$Asymmetrique_Profile_Index
data14<-if_else(data14=="","Medium",data$Asymmetrique_Profile_Index)
data14<-as.factor(data14)
data1$Asymmetrique_Profile_Index<-data14

str(data1)

#Total_Visits#
quantile(data1$Total_Visits,c(0,0.25,0.35,0.45,0.5,0.60,0.7,0.8,0.9,.95,.98,.99,1),na.rm = TRUE)
data1$Total_Visits<-ifelse(data1$Total_Visits>17,17,data1$Total_Visits)

#IMPUTING MISSING VALUES#

data1[is.na(data1$Total_Visits),"Total_Visits"]<-mean(data1$Total_Visits,na.rm = TRUE)
colSums(is.na(data1))

#Page_Views_Per_Visit#
quantile(data1$Page_Views_Per_Visit,c(0,0.25,0.35,0.45,0.5,0.60,0.7,0.8,0.9,.95,.98,.99,1),na.rm = TRUE)
data1$Page_Views_Per_Visit<-ifelse(data1$Page_Views_Per_Visit>=9,9,data1$Page_Views_Per_Visit)

data1[is.na(data1$Page_Views_Per_Visit),"Page_Views_Per_Visit"]<-mean(data1$Page_Views_Per_Visit,na.rm = TRUE)
summary(data1)

#Total_Time_Spent_on_Website#
quantile(data1$Total_Time_Spent_on_Website,c(0,.25,.3,.4,.45,.5,.55,.6,.65,.68,.7,.75,.8,0.85,.9,.95,.98,1))
data1$Total_Time_Spent_on_Website<-ifelse(data1$Total_Time_Spent_on_Website>=1700,1700,data1$Total_Time_Spent_on_Website)
colnames(data)

#imputation of missing values using MICE#

library(mice)
datapratim<-subset(data1,select= c("Asymmetrique_Activity_Score","Asymmetrique_Profile_Score"))
class(datapratim)
colSums(is.na(datapratim))
md.pattern(datapratim)
imputed_Data <- mice(datapratim, m=2, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

#checking imputed data#
imputed_Data$imp$Asymmetrique_Activity_Score 
dim(imputed_Data$imp$Asymmetrique_Profile_Score)
str(imputed_Data$imp$Asymmetrique_Profile_Score)
class(data1$Asymmetrique_Activity_Score)

datapratim[is.na(datapratim$Asymmetrique_Activity_Score),"Asymmetrique_Activity_Score"]<-(imputed_Data$imp$Asymmetrique_Activity_Score)
datapratim[is.na(datapratim$Asymmetrique_Profile_Score),"Asymmetrique_Profile_Score"]<-(imputed_Data$imp$Asymmetrique_Profile_Score)

data1$Asymmetrique_Activity_Score<-datapratim$Asymmetrique_Activity_Score
data1$Asymmetrique_Profile_Score<-datapratim$Asymmetrique_Profile_Score
summary(data1)

colSums(is.na(data1))
cols_cat<-select(data1,-c('Total_Visits','Total_Time_Spent_on_Website','Page_Views_Per_Visit','Asymmetrique_Activity_Score','Asymmetrique_Profile_Score','Prospect_ID','Lead_Number'))
cols_cont<-select(data1,c('Total_Visits','Total_Time_Spent_on_Website','Page_Views_Per_Visit','Asymmetrique_Activity_Score','Asymmetrique_Profile_Score','Converted'))

colnames(cols_cat)
colnames(cols_cont)

library(sqldf)
library(Hmisc)

IVCal<-function(variable,target,data,groups){
  data[,"rank"] <- cut2(data1[,variable],g=groups)
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
  return(IV1)}

a1<-IVCal("Total_Visits","Converted",cols_cont,groups = 10)
a2<-IVCal('Total_Time_Spent_on_Website',"Converted",cols_cont,groups = 10)
a3<-IVCal('Page_Views_Per_Visit',"Converted",cols_cont,groups = 10)
a4<-IVCal('Asymmetrique_Activity_Score',"Converted",cols_cont,groups = 10)
a5<-IVCal('Asymmetrique_Profile_Score',"Converted",cols_cont,groups = 10)
ivcont<-data.frame(rbind(a1,a2,a3,a4,a5))
View(ivcont)

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

cols_cat<-rename(cols_cat,What_is_your_current_occupation=What.is.your.current.occupation)
cols_cat<-rename(cols_cat,What_matters_most_to_you_in_choosing_a_course=What.matters.most.to.you.in.choosing.a.course)
cols_cat<-rename(cols_cat,Newspaper_Article=Newspaper.Article,X_Education_Forums=X.Education.Forums,Digital_Advertisement=Digital.Advertisement,
                 Through_Recommendations=Through.Recommendations,Receive_More_Updates_About_Our_Courses=Receive.More.Updates.About.Our.Courses,
                 Lead_Quality=Lead.Quality,Update_me_on_Supply_Chain_Content=Update.me.on.Supply.Chain.Content,
                 Get_updates_on_DM_Content=Get.updates.on.DM.Content,Lead_Profile=Lead.Profile,
                 I_agree_to_pay_the_amount_through_cheque=I.agree.to.pay.the.amount.through.cheque)
                 


b1<-CA('Converted','Lead_Origin',cols_cat)
b2<-CA('Converted','Lead_Source',cols_cat)
b3<-CA('Converted','Do_Not_Email',cols_cat)
b4<-CA('Converted','Do_Not_Call',cols_cat)
b5<-CA('Converted','Last_Activity',cols_cat)
b6<-CA('Converted','Country',cols_cat)
b7<-CA('Converted','Specialization',cols_cat)
b8<-CA('Converted','How_did_you_hear_about_X_Education',cols_cat)
b9<-CA('Converted',"Search",cols_cat)
b10<-CA('Converted',"Magazine",cols_cat)
b11<-CA('Converted',"Tags",cols_cat)
b12<-CA('Converted',"City",cols_cat)
b13<-CA('Converted', "Asymmetrique_Activity_Index",cols_cat)
b14<-CA('Converted', "Asymmetrique_Profile_Index",cols_cat)
b15<-CA('Converted', "A_free_copy_of_Mastering_The_Interview",cols_cat)

b16<-CA('Converted', "What_is_your_current_occupation",cols_cat)
b17<-CA('Converted', "What_matters_most_to_you_in_choosing_a_course",cols_cat)
b18<-CA('Converted', "Newspaper_Article",cols_cat)
b19<-CA('Converted', "Digital_Advertisement",cols_cat)
b20<-CA('Converted', "X_Education_Forums",cols_cat)
b21<-CA('Converted', "Through_Recommendations",cols_cat)
b22<-CA('Converted', "Receive_More_Updates_About_Our_Courses",cols_cat)
b23<-CA('Converted', "Lead_Quality",cols_cat)
b24<-CA('Converted', "Update_me_on_Supply_Chain_Content",cols_cat)
b25<-CA('Converted', "Lead_Profile",cols_cat)
b26<-CA('Converted', "I_agree_to_pay_the_amount_through_cheque",cols_cat)
b27<-CA('Converted', "Get_updates_on_DM_Content",cols_cat)
b28<-CA('Converted', "Newspaper",cols_cat)
b29<-CA('Converted', "Last_Notable_Activity",cols_cat)

ivcat<-data.frame(rbind(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11
                        ,b12,b13,b14,b15,b16,b17,b18,b19,
                        b20,b21,b22,b23,b24,b25,b26,b27,b28))

View(ivcat)

final_IV<-data.frame(rbind(ivcont,ivcat))
View(final_IV)
summary(data1)


data1<-rename(data1,What_is_your_current_occupation=What.is.your.current.occupation)
data1<-rename(data1,What_matters_most_to_you_in_choosing_a_course=What.matters.most.to.you.in.choosing.a.course)
data1<-rename(data1,Newspaper_Article=Newspaper.Article,X_Education_Forums=X.Education.Forums,Digital_Advertisement=Digital.Advertisement,
                 Through_Recommendations=Through.Recommendations,Receive_More_Updates_About_Our_Courses=Receive.More.Updates.About.Our.Courses,
                 Lead_Quality=Lead.Quality,Update_me_on_Supply_Chain_Content=Update.me.on.Supply.Chain.Content,
                 Get_updates_on_DM_Content=Get.updates.on.DM.Content,Lead_Profile=Lead.Profile,
                 I_agree_to_pay_the_amount_through_cheque=I.agree.to.pay.the.amount.through.cheque)


library(caTools)
set.seed(123)
split<-sample.split(data1,SplitRatio = 0.7)
train<-subset(data1,split== TRUE)
test<-subset(data1,split==FALSE)
dim(train)
dim(test)


model<-glm(Converted~ Total_Time_Spent_on_Website+Asymmetrique_Activity_Score+Lead_Origin+Lead_Source+Tags+What_is_your_current_occupation+
             Lead_Quality+Lead_Profile, data = train,family = binomial())
summary(model)

model1<-glm(Converted~ Total_Time_Spent_on_Website+Asymmetrique_Activity_Score+I(Lead_Origin=='Lead Add Form')+I(Tags=="Busy")+I(Tags=="Closed by Horizzon")+
              I(Tags=="in touch with EINS")+I(Tags=="Lost to EINS")+I(Tags=="Will revert after reading the email")+
              I(What_is_your_current_occupation=='Student')+I(What_is_your_current_occupation=='Working Professional')
            +I(Lead_Quality=='Might be')+I(Lead_Quality=='Worst')
            , data = train,family = binomial())

summary(model1)

library(car)
vif(model1)

model2<-glm(Converted~ Total_Time_Spent_on_Website+Asymmetrique_Activity_Score+I(Lead_Origin=='Lead Add Form')+I(Tags=="Busy")+I(Tags=="Closed by Horizzon")+
              I(Tags=="Lost to EINS")+
              I(What_is_your_current_occupation=='Student')+I(What_is_your_current_occupation=='Working Professional')
            +I(Lead_Quality=='Might be')+I(Lead_Quality=='Worst')
            , data = train,family = binomial())

summary(model2)

vif(model2)

library(aod)
wald.test(b=coef(model2),Sigma = vcov(model2),Terms = 1:10)

#LACKFIT VARIANCE TEST#

residuals(model2)
residuals(model2,"pearson")
sum(residuals(model2,type="pearson")^2)deviance(model2)
lackfit_variance<-1-pchisq(deviance(model2),df.residual(model2))
lackfit_variance

#LAGRANGE MULTIPLIER TEST#
modelchi<-model2$null.deviance-model2$deviance
chidf<-model2$df.null-model2$df.residual
chisq.prob<-1-pchisq(modelchi,chidf)
format(round(chisq.prob,2), nsmall=2)

#CONFUSION MATRIX#

library(caret)
library(caTools)

library(rpart)
library(rpart.plot)
library(pROC)

pred<-predict(model2, newdata= train, type="response")


prediction<-ifelse(pred>=0.5,1,0)
confusion<-table(train$Converted,prediction)
View(prediction)
accuracy_rate<-sum(diag(confusion))/sum(confusion)

accuracy_rate
class(prediction)

pred_factor<-as.factor(prediction)
confusionMatrix(train$Converted,pred_factor)

pred_test<-predict(model2,newdata=test,type="response")

prediction_test<-ifelse(pred_test>=0.5,1,0)
confusion_test<-table(test$Converted,prediction_test)
accuracy_on_test<-sum(diag(confusion_test))/sum(confusion_test)
accuracy_on_test

predfactor_on_test<-as.factor(prediction_test)
confusionMatrix(predfactor_on_test,test$Converted)


model2_test<-glm(Converted~ Total_Time_Spent_on_Website+Asymmetrique_Activity_Score+I(Lead_Origin=='Lead Add Form')+I(Tags=="Busy")+I(Tags=="Closed by Horizzon")+
              I(Tags=="Lost to EINS")+
              I(What_is_your_current_occupation=='Student')+I(What_is_your_current_occupation=='Working Professional')
            +I(Lead_Quality=='Might be')+I(Lead_Quality=='Worst')
            , data = test,family = binomial())

summary(model2)

###RECEREATING THE LOGISTIC MODEL###
new_model<-glm(Converted~ Total_Visits+Total_Time_Spent_on_Website+
                 Page_Views_Per_Visit+
                 Asymmetrique_Activity_Score+Asymmetrique_Profile_Score
               +Lead_Origin+Lead_Source+Do_Not_Email+Do_Not_Call+
                 Last_Activity+Country+Specialization+How_did_you_hear_about_X_Education+
                 Search+Tags+City+Asymmetrique_Activity_Index+
                 Asymmetrique_Profile_Index+
                 A_free_copy_of_Mastering_The_Interview+What_is_your_current_occupation+
                 What_matters_most_to_you_in_choosing_a_course+Newspaper_Article+
                 Digital_Advertisement+Through_Recommendations+
                 Lead_Quality+Lead_Profile,
                 data=train,family = binomial())

summary(new_model)


new_model1<-glm(Converted~ Total_Visits+Total_Time_Spent_on_Website+
                 Page_Views_Per_Visit+
                 Asymmetrique_Activity_Score+Asymmetrique_Profile_Score
                 +I(Do_Not_Email=='Yes')+
                 I(Specialization=="Select")+
                 I(Tags=="Busy")+I(Tags=="Closed by Horizzon")+I(Tags=="Graduation in progress")
                  +I(Tags=="in touch with EINS")+
                  I(Tags=="Lost to EINS")+I(Tags=="opp hangup")+
                  I(Tags=="Want to take admission but has financial problems")
                +I(Tags=="Will revert after reading the email")
                +I(City=="Metro Cities")+I(City=="Select")+
                  I(City=="Tier II Cities") +I(Asymmetrique_Activity_Index=="Medium")+
                 I(Asymmetrique_Profile_Index=="Medium")+
                 I(What_is_your_current_occupation=="Other")+I(What_is_your_current_occupation=="Student")+
                  I(What_is_your_current_occupation=="Working Professional")+
               
                 Lead_Quality,
               data=train,family = binomial())           


summary(new_model1)

nm_pred<-predict(new_model1,newdata=train,type="response")
nm_prediction<-ifelse(nm_pred>=0.5,1,0)
table(nm_prediction,train$Converted)



new_model2<-glm(Converted~ Total_Visits+Total_Time_Spent_on_Website+
      Page_Views_Per_Visit+
      Asymmetrique_Activity_Score+Asymmetrique_Profile_Score
    +I(Do_Not_Email=='Yes')+
      I(Specialization=="Select")+
      I(Tags=="Busy")+I(Tags=="Closed by Horizzon")+I(Tags=="Graduation in progress")
    +I(Tags=="in touch with EINS")+
      I(Tags=="Lost to EINS")+
      I(Tags=="Want to take admission but has financial problems")
    +I(Tags=="Will revert after reading the email")
    +I(City=="Select")+
      I(Asymmetrique_Activity_Index=="Medium")+
      I(Asymmetrique_Profile_Index=="Medium")+
      I(What_is_your_current_occupation=="Student")+
      I(What_is_your_current_occupation=="Working Professional")+
      
      Lead_Quality,
    data=train,family = binomial())

summary(new_model2)

vif(new_model2)

wald.test(b=coef(new_model2),Sigma = vcov(new_model2),Terms = 1:20)

#LACKFIT VARIANCE TEST#

residuals(new_model2)
residuals(new_model2,"pearson")
sum(residuals(new_model2,type="pearson")^2)deviance(new_model2)
lackfit_variance_newmodel2<-1-pchisq(deviance(new_model2),df.residual(new_model2))
lackfit_variance_newmodel2

#LAGRANGE MULTIPLIER TEST#
modelchi_nm<-new_model2$null.deviance-new_model2$deviance
chidf_nm<-new_model2$df.null-new_model2$df.residual
chisq.prob_nm<-1-pchisq(modelchi_nm,chidf_nm)
format(round(chisq.prob,2), nsmall=2)


nm_pred2<-predict(new_model2,newdata=train,type="response")
nm_prediction2<-ifelse(nm_pred2>=0.5,1,0)#setting the threshold#
confusion_nm<-table(nm_prediction2,train$Converted)
accuracy_nm<-sum(diag(confusion_nm))/sum(confusion_nm)
accuracy_nm

predasfactor_nm<-as.factor(nm_prediction2)
confusionMatrix(predasfactor_nm,train$Converted)

nm_predtest2<-predict(new_model2,newdata=test,type="response")
nm_predictiontest<-ifelse(nm_predtest2>=0.5,1,0)#setting the threshold#
conf_test<-table(test$Converted,nm_predictiontest)
accuracy_nm_test<-sum(diag(conf_test))/sum(conf_test)

accuracy_nm_test

predasfactor_nm_test<-as.factor(nm_predictiontest)
confusionMatrix(predasfactor_nm_test,test$Converted)


#AUC AND ROC MEASURE#
ROC_LM<-roc(response=test$Converted,predictor=
      factor(nm_predtest2,ordered=TRUE),
    levels=rev(levels(test$Converted)))
ROC_LM
plot(ROC_LM)

#DECISION TREES AND RANDOM FOREST#

library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)

chaid1<-rpart(Converted~ Total_Visits+Total_Time_Spent_on_Website+
                Page_Views_Per_Visit+
                Asymmetrique_Activity_Score+Asymmetrique_Profile_Score
              +Lead_Origin+Lead_Source+Do_Not_Email+Do_Not_Call+
                Last_Activity+Country+Specialization+How_did_you_hear_about_X_Education+
                Search+Tags+City+Asymmetrique_Activity_Index+
                Asymmetrique_Profile_Index+
                A_free_copy_of_Mastering_The_Interview+What_is_your_current_occupation+
                What_matters_most_to_you_in_choosing_a_course+Newspaper_Article+
                Digital_Advertisement+Through_Recommendations+
                Lead_Quality+Lead_Profile,
              data=train,method = "class")
prp(chaid1)



chaid2<-rpart(Converted~ Total_Visits+Total_Time_Spent_on_Website+
                Page_Views_Per_Visit+
                Asymmetrique_Activity_Score+Asymmetrique_Profile_Score
              +Lead_Origin+Lead_Source+Do_Not_Email+Do_Not_Call+
                Last_Activity+Country+Specialization+How_did_you_hear_about_X_Education+
                Search+Tags+City+Asymmetrique_Activity_Index+
                Asymmetrique_Profile_Index+
                A_free_copy_of_Mastering_The_Interview+What_is_your_current_occupation+
                What_matters_most_to_you_in_choosing_a_course+Newspaper_Article+
                Digital_Advertisement+Through_Recommendations+
                Lead_Quality+Lead_Profile,
              data=train,method = "class", minbucket=1,cp=0.01)
prp(chaid2)

pred_dt<-predict(chaid2,newdata=test,type="class")
table(test$Converted,pred_dt)
library(caret)
confusionMatrix(pred_dt,test$Converted)

chaid2_roc<-roc(response=test$Converted,predictor=factor(pred_dt,ordered = TRUE),
    levels=rev(levels(test$Converted)))
chaid2_roc
plot(chaid2_roc)



#DECISION TREE BASED ON NEW MODEL2#

chaid3<-rpart(Converted~ Total_Visits+Total_Time_Spent_on_Website+
    Page_Views_Per_Visit+
    Asymmetrique_Activity_Score+Asymmetrique_Profile_Score
  +I(Do_Not_Email=='Yes')+
    I(Specialization=="Select")+
    I(Tags=="Busy")+I(Tags=="Closed by Horizzon")+I(Tags=="Graduation in progress")
  +I(Tags=="in touch with EINS")+
    I(Tags=="Lost to EINS")+
    I(Tags=="Want to take admission but has financial problems")
  +I(Tags=="Will revert after reading the email")
  +I(City=="Select")+
    I(Asymmetrique_Activity_Index=="Medium")+
    I(Asymmetrique_Profile_Index=="Medium")+
    I(What_is_your_current_occupation=="Student")+
    I(What_is_your_current_occupation=="Working Professional")+
    
    Lead_Quality,
  data=train,method = "class",minbucket=1,cp=0.01)


prp(chaid3)

pred_dt1<-predict(chaid3,newdata=test,type="class")
table(test$Converted,pred_dt1)
confusionMatrix(pred_dt1,test$Converted)
library(pROC)

chaid3_roc<-roc(response=test$Converted,predictor=factor(pred_dt1 ,ordered=TRUE),
                level=rev(levels(test$Converted)))
chaid3_roc
plot(chaid3_roc)


#RANDOM FOREST #

rf<-randomForest(Converted~.,data=train,ntree=500 )
rf_pred<-predict(rf,newdata=test,type="class")
table(test$Converted,rf_pred)
confusionMatrix(rf_pred,test$Converted)

varImpPlot(rf,main="VARIABLE IMPORTANCE CHART")

rf_roc<-roc(response=test$Converted,predictor=factor(rf_pred,ordered=TRUE),
             levels=rev(levels(test$Converted)))
rf_roc
plot(rf_roc)


#RANDOM FOREST ON NEW MODEL#

rf1<-randomForest(Converted~ Total_Visits+Total_Time_Spent_on_Website+
    Page_Views_Per_Visit+
    Asymmetrique_Activity_Score+Asymmetrique_Profile_Score
  +Lead_Origin+Lead_Source+Do_Not_Email+Do_Not_Call+
    Last_Activity+Country+Specialization+How_did_you_hear_about_X_Education+
    Search+Tags+City+Asymmetrique_Activity_Index+
    Asymmetrique_Profile_Index+
    A_free_copy_of_Mastering_The_Interview+What_is_your_current_occupation+
    What_matters_most_to_you_in_choosing_a_course+Newspaper_Article+
    Digital_Advertisement+Through_Recommendations+
    Lead_Quality+Lead_Profile,
  data=train,ntree=500)


varImpPlot(rf1,main="VARIABLE IMPORTANCE CHART")

rf1_pred<-predict(rf1,newdata=test,type="class")
table(test$Converted,rf1_pred)
confusionMatrix(rf1_pred,test$Converted)

rf1_roc<-roc(response=test$Converted,predictor=factor(rf1_pred,ordered=TRUE),
    levels=rev(levels(test$Converted)))
rf1_roc
plot(rf1_roc)


