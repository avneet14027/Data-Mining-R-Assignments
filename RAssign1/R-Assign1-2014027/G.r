finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);


#Ans7
Median.matrix<-as.matrix(apply(Data,2,FUN="median"))