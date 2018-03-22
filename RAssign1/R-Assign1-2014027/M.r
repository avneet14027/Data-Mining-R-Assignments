library(lsa)

finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);

#Ans13
cosineM<-as.matrix(cosine(Data));
minCos<-which(cosineM==min(cosineM), arr.ind=TRUE)
minDist<-min(cosineM)