finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);

#Ans 3
#columnwise mean is calculated
ColMean<-as.matrix(colMeans(Data))
View(ColMean)