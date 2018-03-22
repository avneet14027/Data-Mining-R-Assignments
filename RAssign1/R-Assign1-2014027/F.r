finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);
covMatrix<-cov(Data)

#Ans6
#Decreasing Variances
#Diagonals of covariance matrix represent variance
covDiag<-as.matrix(diag(covMatrix))
decVar<-sort(covDiag,decreasing=TRUE)
DecVar<-as.matrix(order(covDiag,decreasing=TRUE))
View(covDiag)
View(decVar)
View(DecVar)
