library(caret)
library(gridExtra)
library(cluster)
setwd('F:/R/K MEANS')
airlines<-read.csv('AirlinesCluster.csv',as.is=TRUE,header = TRUE)
View(airlines)
max(airlines$Balance)
str(airlines)
summary(airlines)
(colSums(is.na(airlines)))
quantile(airlines$Balance,c(0,0.25,0.5,0.75,0.8,0.85,0.90,0.95,1))
quantile(airlines$QualMiles,c(0,0.25,0.5,0.75,0.8,0.85,0.90,0.95,1))
quantile(airlines$BonusMiles,c(0,0.25,0.5,0.75,0.8,0.85,0.90,0.95,1))
quantile(airlines$FlightMiles,c(0,0.25,0.5,0.75,0.8,0.85,0.90,0.95,1))
quantile(airlines$FlightTrans,c(0,0.25,0.5,0.75,0.75,0.8,0.85,0.90,0.95,1))
quantile(airlines$DaysSinceEnroll,c(0,0.25,0.5,0.75,0.75,0.8,0.85,0.90,0.95,1))
quantile(airlines$BonusTrans,c(0,0.25,0.5,0.75,0.75,0.8,0.85,0.90,0.95,1))

#calculating interquartile range#

IQR_BALANCE<-IQR(airlines$Balance,na.rm=TRUE)
cap_balance<- 92404.0+1.5*(IQR_BALANCE)
cap_balance
IQR_QM<-IQR(airlines$QualMiles,na.rm = TRUE)
cap_qm<-0+1.5*(IQR_QM)
IQR_BM<-IQR(airlines$BonusMiles,na.rm = TRUE)
cap_bm<-23800.5+1.5*(IQR_BM)
IQR_BT<-IQR(airlines$BonusTrans,na.rm = TRUE)
cap_BT<- 17+1.5*(IQR_BT)
IQR_FM<-IQR(airlines$FlightMiles,na.rm = TRUE)
cap_fm<-311.0+1.5*(IQR_FM)
IQR_FTR<-IQR(airlines$FlightTrans,na.rm = TRUE)
cap_ftr<- 1+1.5*(IQR_FTR)
IQR_DSE<-IQR(airlines$DaysSinceEnroll,na.rm = TRUE)
cap_dse<-5790.5+1.5*(IQR_DSE)


airlines$Balance<-ifelse(airlines$Balance>=cap_balance,cap_balance,airlines$Balance)
airlines$QualMiles<-ifelse(airlines$QualMiles>=cap_qm,cap_qm,airlines$QualMiles)
airlines$BonusMiles<-ifelse(airlines$BonusMiles>=cap_bm,cap_bm,airlines$BonusMiles)
airlines$BonusTrans<-ifelse(airlines$BonusTrans>=cap_BT,cap_BT,airlines$BonusTrans)
airlines$FlightMiles<-ifelse(airlines$FlightMiles>=cap_fm,cap_fm,airlines$FlightMiles)
airlines$FlightTrans<-ifelse(airlines$FlightTrans>=cap_ftr,cap_ftr,airlines$FlightTrans)
airlines$DaysSinceEnroll<-ifelse(airlines$DaysSinceEnroll>=cap_dse,cap_dse,airlines$DaysSinceEnroll)

summary(airlines)



#NORMALIZING THE DATA FOR CLUSTERING#
preproc<-preProcess(airlines)
airlinesnorm<-predict(preproc,airlines)#scaling the data around mean#
summary(airlinesnorm)

#HIERARCHIAL CLUSTERING#
distance<-dist(airlinesnorm,method = 'euclidean')# calculating euclidean distance#
clusters<-hclust(distance,method = 'ward.D')#creating the hierarchial clusters#
plot(clusters)

#BASED ON CLUSTER DENDROGRAM WE DECIDE K=7#
airlinescluster<-cutree(clusters,k=7)#assigning datapoints/frequencies to a cluster#
table(airlinescluster)# displays the no.of datapoints in a cluster#
mean_comp<-function(var,clustergrp,meas){
  z<-tapply(var,clustergrp,meas)
  print(z)
}
Balance_mean<-mean_comp(airlines$Balance,airlinescluster,mean)
Balance_dayssinceenroll<-mean_comp(airlines$DaysSinceEnroll,airlinescluster,mean)
airlines_new<-data.frame(airlines,airlinescluster)
View(airlines_new)

#ANSWER TO Q4 OF ASSIGNMENT#
Q4<-cutree(clusters,k=5)
table(Q4)
balance_mean<-mean_comp(airlines$Balance,Q4,mean)
QualMiles_mean<-mean_comp(airlines$QualMiles,Q4,mean)
BonusMiles_mean<-mean_comp(airlines$BonusMiles,Q4,mean)
BonusTrans_mean<-mean_comp(airlines$BonusTrans,Q4,mean)
FlightMiles_mean<-mean_comp(airlines$FlightMiles,Q4,mean)
FlightTrans_mean<-mean_comp(airlines$FlightTrans,Q4,mean)
DaysSinceEnroll_mean<-mean_comp(airlines$DaysSinceEnroll,Q4,mean)

#K MEANS CLUSTERING#
set.seed(88)
K1<-kmeans(airlinesnorm,centers=5, iter.max = 1000)
table(K1$cluster)
library(factoextra)
p1<-fviz_cluster(K1,geom="point",data= airlinesnorm)+ggtitle("k=5")
grid.arrange(p1,nrow=2)

K_mod<-aggregate(airlines,by=list(cluster=K1$cluster),mean)
K_mod

set.seed(88)
fviz_nbclust(airlinesnorm,kmeans,method="wss")
k2<-kmeans(airlinesnorm,centers=5,iter.max = 1000)
table(k2$cluster)        
k2_mod<-aggregate(airlines,by=list(k2$cluster),mean)
k2_mod

