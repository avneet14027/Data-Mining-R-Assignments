finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);

#Ans10
#if any element in correlation matrix is 0, then that pair(i,j) of columns(vectors) are orthogonal
corMat<-as.matrix(cor(Data))
orthInd<-which(x==0, arr.ind=TRUE)
View(othInd)
View(corMat)
#none exist so shows null