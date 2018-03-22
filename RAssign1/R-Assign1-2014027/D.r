finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);

#Ans4
#--->first, correlation matrix is found, then max element is found excluding the diagonals of the matrix
corMatrix<-cor(Data)
corDiag<-diag(corMatrix)
maxCorCoeffIndex<-which(corMatrix==max(corMatrix[(row(corMatrix)!=col(corMatrix))]),arr.ind=TRUE)
maxCorrCoeff<-max(corMatrix[(row(corMatrix)!=col(corMatrix))])
View(corMatrix)
View(maxCorCoeffIndex)
View(maxCorrCoeff)