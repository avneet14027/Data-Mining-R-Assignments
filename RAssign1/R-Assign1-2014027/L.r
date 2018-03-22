finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);

#Ans12
#Eucledian distance is found and stored in a matrix, then maximum in found out
f<-as.matrix(t(Data))
EucDist<-as.matrix(dist(f,method="euclidean"))
View(EucDist)
#g<-max(EucDist)
indices<-which(EucDist==min(EucDist),arr.ind=TRUE)
View(indices)