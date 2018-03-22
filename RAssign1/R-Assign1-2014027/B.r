finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);

#Ans2
#diagonals of the covariance matrix gives us the variance of the attributes 
covMatrix<-as.matrix(cov(Data))
maxDispEle<-max(diag(covMatrix))
maxDispInd<-which(covMatrix==max(diag(covMatrix)),arr.ind=TRUE)
View(maxDispEle)
View(maxDispInd)