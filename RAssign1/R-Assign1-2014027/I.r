finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);

#EquiDepth histogram

#Ans9
col1Data<-as.matrix(Data[,1])
hist(col1Data)
#hist(col1Data,breaks=c(0,16,350))
plot(cut(col1Data,quantile(col1Data)))