setwd('F:/R/LEAD PROJECT')
data<-read.csv('Leads_data.csv',as.is=TRUE  ,header = TRUE)
colnames(data)
str(data)
summary(data)
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
data$Lead_Source<-if_else(data$Lead_Source=="google","Google",data$Lead_Source)

#conversion of numeric/character variables to factor/categorical variables#

cols_cat<-select(data,-c('Total_Visits','Total_Time_Spent_on_Website','Page_Views_Per_Visit','Asymmetrique_Activity_Score','Asymmetrique_Profile_Score','Prospect_ID','Lead_Number'))
cols_cat<-lapply(cols_cat,factor)
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


data1<-rename(data1,What_is_your_current_occupation=What.is.your.current.occupation)
data1<-rename(data1,What_matters_most_to_you_in_choosing_a_course=What.matters.most.to.you.in.choosing.a.course)
data1<-rename(data1,Newspaper_Article=Newspaper.Article,X_Education_Forums=X.Education.Forums,Digital_Advertisement=Digital.Advertisement,
              Through_Recommendations=Through.Recommendations,Receive_More_Updates_About_Our_Courses=Receive.More.Updates.About.Our.Courses,
              Lead_Quality=Lead.Quality,Update_me_on_Supply_Chain_Content=Update.me.on.Supply.Chain.Content,
              Get_updates_on_DM_Content=Get.updates.on.DM.Content,Lead_Profile=Lead.Profile,
              I_agree_to_pay_the_amount_through_cheque=I.agree.to.pay.the.amount.through.cheque)

filter(data,Asymmetrique_Activity_Score=" ")
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
dim(filter(data1,What_is_your_current_occupation==""))#2690#
dim(filter(data1,Search==""))
dim(filter(data1,Magazine==""))
dim(filter(data1,X_Education_Forums==""))
dim(filter(data1,Newspaper==""))
dim(filter(data1,Receive_More_Updates_About_Our_Courses==""))
dim(filter(data1,Tags==""))#3353#
dim(filter(data1,Lead_Quality==""))#4767#
dim(filter(data1,Update_me_on_Supply_Chain_Content==""))
dim(filter(data1,City==""))# 1420#
dim(filter(data1,Asymmetrique_Activity_Index==""))#4218#
dim(filter(data1,Asymmetrique_Profile_Index==""))#4218#
dim(filter(data1,I_agree_to_pay_the_amount_through_cheque==""))
dim(filter(data1,A_free_copy_of_Mastering_The_Interview==""))
dim(filter(data1,Last_Notable_Activity==""))
library(naniar)
data1$Country<-as.factor(data$Country)

#Conversion of missing values to NA#
data1$Specialization[data1$Specialization==""]<-NA
data1$Country[data1$Country==""]<-NA
data1$How_did_you_hear_about_X_Education[data1$How_did_you_hear_about_X_Education==""]<-NA
data1$What_is_your_current_occupation[data1$What_is_your_current_occupation==""]<-NA
data1$Tags[data1$Tags==""]<-NA
data1$Lead_Quality[data1$Lead_Quality==""]<-NA
data1$Asymmetrique_Activity_Index[data1$Asymmetrique_Activity_Index==""]<-NA
data1$Asymmetrique_Profile_Index[data1$Asymmetrique_Profile_Index==""]<-NA



pratim<-subset(data1,select = c("Specialization","Country","How_did_you_hear_about_X_Education"
                                ,"What_is_your_current_occupation","Tags","Lead_Quality",
                                "Asymmetrique_Activity_Index","Asymmetrique_Profile_Index"))

str(pratim)
dim(pratim)
#imputation of missing values#
#LEAD SOURCE#
data2<-data1$Lead_Source
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


#Lead.Profile#
data12<-data$Lead.Profile
data12<-if_else(data12=="","Select",data$Lead.Profile)
data12<-as.factor(data12)
data1$Lead_Profile<-data12


summary(data1)
str(data1)

#OUTLIER TREATMENT#
#Total_Visits#
quantile(data1$Total_Visits,c(0,0.25,0.35,0.45,0.5,0.60,0.7,0.8,0.9,.95,.98,.99,1),na.rm = TRUE)
data1$Total_Visits<-ifelse(data1$Total_Visits>17,17,data1$Total_Visits)


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
imputed_Data1<- mice(datapratim, m=2, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

#checking imputed data#
imputed_Data1$imp$Asymmetrique_Activity_Score 
dim(imputed_Data1$imp$Asymmetrique_Profile_Score)
str(imputed_Data1$imp$Asymmetrique_Profile_Score)
class(data1$Asymmetrique_Activity_Score)

datapratim[is.na(datapratim$Asymmetrique_Activity_Score),"Asymmetrique_Activity_Score"]<-(imputed_Data1$imp$Asymmetrique_Activity_Score)
datapratim[is.na(datapratim$Asymmetrique_Profile_Score),"Asymmetrique_Profile_Score"]<-(imputed_Data1$imp$Asymmetrique_Profile_Score)

data1$Asymmetrique_Activity_Score<-datapratim$Asymmetrique_Activity_Score
data1$Asymmetrique_Profile_Score<-datapratim$Asymmetrique_Profile_Score

colSums(is.na(data1))

summary(data1)

class(pratim)
md.pattern(pratim)
imputed_Data <- mice(pratim, m=8, maxit = 50, method = 'pmm', seed = 500)
imputed_Data$imp$Specialization
imputed_Data$imp$Country
imputed_Data$imp$How_did_you_hear_about_X_Education
imputed_Data$imp$What_is_your_current_occupation
imputed_Data$imp$Tags
imputed_Data$imp$Lead_Quality
imputed_Data$imp$Asymmetrique_Activity_Index
imputed_Data$imp$Asymmetrique_Profile_Index

pratim[is.na(pratim$Specialization),"Specialization"]<-imputed_Data$imp$Specialization
pratim[is.na(pratim$Country),"Country"]<-imputed_Data$imp$Country
pratim[is.na(pratim$How_did_you_hear_about_X_Education),"How_did_you_hear_about_X_Education"]<-imputed_Data$imp$How_did_you_hear_about_X_Education
pratim[is.na(pratim$What_is_your_current_occupation),"What_is_your_current_occupation"]<-imputed_Data$imp$What_is_your_current_occupation
pratim[is.na(pratim$Tags),"Tags"]<-imputed_Data$imp$Tags
pratim[is.na(pratim$Lead_Quality),"Lead_Quality"]<-imputed_Data$imp$Lead_Quality
pratim[is.na(pratim$Asymmetrique_Activity_Index),"Asymmetrique_Activity_Index"]<-imputed_Data$imp$Asymmetrique_Activity_Index
pratim[is.na(pratim$Asymmetrique_Profile_Index),"Asymmetrique_Profile_Index"]<-imputed_Data$imp$Asymmetrique_Profile_Index



data1$Specialization<-pratim$Specialization
data1$Country<-pratim$Country
data1$How_did_you_hear_about_X_Education<-pratim$How_did_you_hear_about_X_Education
data1$What_is_your_current_occupation<-pratim$What_is_your_current_occupation
data1$Tags<-pratim$Tags
data1$Lead_Quality<-pratim$Lead_Quality
data1$Asymmetrique_Activity_Index<-pratim$Asymmetrique_Activity_Index
data$Asymmetrique_Profile_Index<-pratim$Asymmetrique_Profile_Index
colSums(is.na(data1))

summary(data1)
data1[is.na(data1$Asymmetrique_Profile_Index),"Asymmetrique_Profile_Index"]<-"Medium"
str(data1)
data1$Lead.Profile<-NULL
data1$What.matters.most.to.you.in.choosing.a.course<-NULL
cols_cat<-select(data1,-c('Total_Visits','Total_Time_Spent_on_Website','Page_Views_Per_Visit','Asymmetrique_Activity_Score','Asymmetrique_Profile_Score','Prospect_ID','Lead_Number'))
cols_cont<-select(data1,c('Total_Visits','Total_Time_Spent_on_Website','Page_Views_Per_Visit','Asymmetrique_Activity_Score','Asymmetrique_Profile_Score','Converted'))

colnames(cols_cat)
colnames(data1)
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

final_IV<-data.frame(rbind(ivcont,ivcat))
View(final_IV)


#LOGISTIC REGRESSION#

library(caTools)
set.seed(123)
split<-sample.split(data1,SplitRatio = 0.7)
train<-subset(data1,split== TRUE)
test<-subset(data1,split==FALSE)
dim(train)
dim(test)

#CREATING MODEL BASED ON INFORMATION VALUES#
model1<-glm(Converted~ Total_Time_Spent_on_Website+
                 Asymmetrique_Activity_Score
               + Lead_Origin+Lead_Source+
                 Last_Activity+Tags+Asymmetrique_Activity_Index+
                 What_is_your_current_occupation+
                 What_matters_most_to_you_in_choosing_a_course+
                 Lead_Quality+Lead_Profile,
                data=train,family = binomial())
summary(model1)
model2<-glm(Converted~ Total_Time_Spent_on_Website+
              Asymmetrique_Activity_Score
            + I(Lead_Origin=="Landing Page Submission")+
             
            I(Tags=="Closed by Horizzon")+
              +I(Tags=="Diploma holder (Not Eligible)")+I(Tags=="Graduation in progress")+
              I(Tags=="In confusion whether part time or DLP")+I(Tags=="Interested  in full time MBA")+
              I(Tags=="Interested in other courses")+I(Tags=="Lost to EINS")+
              I(Tags=="Not doing further education")+I(Tags=="number not provided")+
              I(Tags=="opp hangup")+I(Tags=="Ringing")+I(Tags=="Still Thinking")+
              I(Tags=="switched off")+I(Tags=="Will revert after reading the email")+
              I(Tags=="wrong number given")+
              I(Asymmetrique_Activity_Index=="Medium")+
              I(What_is_your_current_occupation=="Student")+
              I(What_is_your_current_occupation=="Unemployed")+
              I(Lead_Profile=="Lateral Student")+
              I(Lead_Profile=="Other Leads")+I(Lead_Profile=="Potential Lead")+
              I(Lead_Profile=="Select"),data=train,family = binomial())

summary(model2)

library(car)
vif(model2)


model3<-glm(Converted~ Total_Time_Spent_on_Website+
              Asymmetrique_Activity_Score
            + I(Lead_Origin=="Landing Page Submission")+
              
              I(Tags=="Closed by Horizzon")+
              +I(Tags=="Diploma holder (Not Eligible)")+I(Tags=="Graduation in progress")+
              I(Tags=="Interested  in full time MBA")+
              I(Tags=="Interested in other courses")+I(Tags=="Lost to EINS")+
              I(Tags=="Not doing further education")+
              I(Tags=="opp hangup")+I(Tags=="Ringing")+
              I(Tags=="switched off")+I(Tags=="Will revert after reading the email")+
              I(Tags=="wrong number given")+
              I(Asymmetrique_Activity_Index=="Medium")+
              I(What_is_your_current_occupation=="Unemployed")+
              I(Lead_Profile=="Lateral Student")+
              I(Lead_Profile=="Other Leads")+I(Lead_Profile=="Potential Lead")
              ,data=train,family = binomial())
summary(model3)
vif(model3)

library(aod)
wald.test(b=coef(model3),Sigma = vcov(model3),Terms = 1:20)

#LACKFIT DEVIANCE TEST#

residuals(model3)
residuals(model3,"pearson")
sum(residuals(model3,type="pearson")^2)deviance(model3)
lackfit_deviance<-1-pchisq(deviance(model3),df.residual(model3))
lackfit_deviance

#LAGRANGE MULTIPLIER TEST#
modelchi<-model3$null.deviance-model3$deviance
chidf<-model3$df.null-model3$df.residual
chisq.prob<-1-pchisq(modelchi,chidf)
format(round(chisq.prob,2), nsmall=2)

#CONFUSION MATRIX#

library(caret)
library(caTools)

library(rpart)
library(rpart.plot)
library(pROC)

pred<-predict(model3, newdata= train, type="response")


prediction<-ifelse(pred>=0.5,1,0)
confusion<-table(train$Converted,prediction)
View(prediction)
accuracy_rate<-sum(diag(confusion))/sum(confusion)

accuracy_rate
class(prediction)

pred_factor<-as.factor(prediction)
confusionMatrix(train$Converted,pred_factor)

pred_test<-predict(model3,newdata=test,type="response")

prediction_test<-ifelse(pred_test>=0.5,1,0)
confusion_test<-table(test$Converted,prediction_test)
accuracy_on_test<-sum(diag(confusion_test))/sum(confusion_test)
accuracy_on_test

predfactor_on_test<-as.factor(prediction_test)
confusionMatrix(predfactor_on_test,test$Converted)

#AUC AND ROC MEASURE#

ROC_LM<-roc(response=test$Converted,predictor=
              factor(pred_test,ordered=TRUE),
            levels=rev(levels(test$Converted)))
ROC_LM
plot(ROC_LM)

#END OF LOG REG MODELLING BASED ON INFORMATION VALUES#

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

pred_df<-predict(chaid1,newdata=test,type="class")

ROC_DF<-roc(response=test$Converted,predictor=
              factor(pred_df,ordered=TRUE),
            levels=rev(levels(test$Converted)))
ROC_DF
plot(ROC_DF)

confusionMatrix(pred_df,test$Converted)

#decision tree based on important variables as per IV table#
chaid2<-rpart(Converted~ Total_Time_Spent_on_Website
                +Asymmetrique_Activity_Score
               +Lead_Origin+Lead_Source+
                Last_Activity+Tags+Asymmetrique_Activity_Index+
                What_is_your_current_occupation+
                What_matters_most_to_you_in_choosing_a_course+
                Lead_Quality+Lead_Profile+Specialization,
              data=train,method = "class")




prp(chaid2)
par("mar")
par(mar=c(4,2,2,3))

pred_df2<-predict(chaid2,newdata=test,type="class")

ROC_DF2<-roc(response=test$Converted,predictor=
              factor(pred_df2,ordered=TRUE),
            levels=rev(levels(test$Converted)))
ROC_DF2
plot(ROC_DF2)


confusionMatrix(pred_df2,test$Converted)

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


#RANDOM FOREST BASED ON IMP VARIABLES FROM IV CHART#
rf_1<-randomForest(Converted~Total_Time_Spent_on_Website
+Asymmetrique_Activity_Score
+Lead_Origin+Lead_Source+
  Last_Activity+Tags+Asymmetrique_Activity_Index+
  What_is_your_current_occupation+
  What_matters_most_to_you_in_choosing_a_course+
  Lead_Quality+Lead_Profile,
data=train,ntree=500)

rf_pred1<-predict(rf_1,newdata=test,type="class")
table(test$Converted,rf_pred1)
confusionMatrix(rf_pred1,test$Converted)

rf_roc1<-roc(response=test$Converted,predictor=factor(rf_pred1,ordered=TRUE),
            levels=rev(levels(test$Converted)))
rf_roc1
plot(rf_roc1)

varImpPlot(rf_1,main="VARIABLE IMPORTANCE CHART rf_1")


#RANDOM FOREST BASED ON ALL VARIABLES#
rf_2<-randomForest(Converted~ Total_Visits+Total_Time_Spent_on_Website+
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


rf_pred2<-predict(rf_2,newdata=test,type="class")
table(test$Converted,rf_pred2)
confusionMatrix(rf_pred2,test$Converted)

rf_roc2<-roc(response=test$Converted,predictor=factor(rf_pred2,ordered=TRUE),
             levels=rev(levels(test$Converted)))
rf_roc2
plot(rf_roc2)

varImpPlot(rf_2,main="VARIABLE IMPORTANCE CHART rf_2")

summary(data1)

#combining IV variables and specialization# BEST MODEL#
rf_3<-randomForest(Converted~Total_Time_Spent_on_Website
                   +Asymmetrique_Activity_Score
                   +Lead_Origin+Lead_Source+
                     Last_Activity+Tags+Asymmetrique_Activity_Index+
                     What_is_your_current_occupation+
                     What_matters_most_to_you_in_choosing_a_course+
                     Lead_Quality+Lead_Profile+Specialization,
                   data=train,ntree=500)


rf_pred3<-predict(rf_3,newdata=test,type="class")
table(test$Converted,rf_pred3)
confusionMatrix(rf_pred3,test$Converted)

rf_roc3<-roc(response=test$Converted,predictor=factor(rf_pred3,ordered=TRUE),
             levels=rev(levels(test$Converted)))
rf_roc3
plot(rf_roc3)
par(mar=c(4,2,2,3))
varImpPlot(rf_3,main="VARIABLE IMPORTANCE CHART rf_3")

cols_cont[,(1:5)]

library(pastecs)
options(scipen = 100)
stat.desc(cols_cont[,(1:5)])
sort(table(cols_cat$Lead_Origin),decreasing = TRUE)
sort(table(cols_cat$Lead_Source),decreasing = TRUE)
table(cols_cat$Do_Not_Email)
table(cols_cat$Do_Not_Call)
sort(table(cols_cat$Last_Activity),decreasing = TRUE)
sort(table(cols_cat$ Country),decreasing = TRUE)
sort(table(cols_cat$Specialization),decreasing = TRUE)
sort(table(cols_cat$How_did_you_hear_about_X_Education),decreasing = TRUE)
sort(table(cols_cat$What_is_your_current_occupation),decreasing = TRUE)
sort(table(cols_cat$What_matters_most_to_you_in_choosing_a_course),decreasing = TRUE)
sort(table(cols_cat$Search),decreasing = TRUE)
table(cols_cat$Magazine)
table(cols_cat$Newspaper_Article)
table(cols_cat$Through_Recommendations)
table(cols_cat$Receive_More_Updates_About_Our_Courses)
table(cols_cat$Digital_Advertisement)
table(cols_cat$Newspaper)
sort(table(cols_cat$Tags),decreasing = TRUE)
sort(table(cols_cat$Lead_Quality),decreasing = TRUE)
table(cols_cat$Update_me_on_Supply_Chain_Content)
table(cols_cat$Get_updates_on_DM_Content)
sort(table(cols_cat$City),decreasing = TRUE)
sort(table(cols_cat$Asymmetrique_Activity_Index),decreasing = TRUE)
sort(table(cols_cat$Asymmetrique_Profile_Index),decreasing = TRUE)
table(cols_cat$I_agree_to_pay_the_amount_through_cheque)
table(cols_cat$A_free_copy_of_Mastering_The_Interview)
table(cols_cat$Last_Notable_Activity)


rf_4<-randomForest(Converted~Total_Time_Spent_on_Website
                   +Asymmetrique_Activity_Score
                   +Lead_Origin+Lead_Source+
                     Last_Activity+Tags+Asymmetrique_Activity_Index+
                     What_is_your_current_occupation+
                     What_matters_most_to_you_in_choosing_a_course+
                     Lead_Quality+Lead_Profile+Specialization+City+Page_Views_Per_Visit+
                     Total_Visits+How_did_you_hear_about_X_Education+Asymmetrique_Profile_Score,
                   data=train,ntree=500)

rf_pred4<-predict(rf_4,newdata=test,type="class")
table(test$Converted,rf_pred4)
confusionMatrix(rf_pred4,test$Converted)

rf_roc4<-roc(response=test$Converted,predictor=factor(rf_pred4,ordered=TRUE),
             levels=rev(levels(test$Converted)))
rf_roc4
plot(rf_roc4)
par(mar=c(4,2,2,3))
varImpPlot(rf_4,main="VARIABLE IMPORTANCE CHART rf_4")

