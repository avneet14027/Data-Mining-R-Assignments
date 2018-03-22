library(lsa)

finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);


#Ans1

my.matrix=c()
for(i in 1:10){
  colData<-as.matrix(Data[,i])
  a<-sweep(colData,2,colMeans(colData),"-")
  my.matrix<-cbind(my.matrix,a)
}
N<-nrow(Data)
transpose.matrix=t(my.matrix)
covMatrix<-1/N*(t(my.matrix)%*%(my.matrix))

#Ans2

maxDispEle<-max(diag(covMatrix))
maxDispInd<-which(covMatrix==max(diag(covMatrix)),arr.ind=TRUE)

#Ans3

ColMean<-as.matrix(colMeans(Data))

#Ans4

corMatrix<-cor(Data)
corDiag<-diag(corMatrix)
maxCorCoeffIndex<-which(corMatrix==max(corMatrix[(row(corMatrix)!=col(corMatrix))]),arr.ind=TRUE)
maxCorrCoeff<-max(corMatrix[(row(corMatrix)!=col(corMatrix))])

#Ans5
col1Data<-as.matrix(Data[,1])
colData1<-as.matrix(sweep(col1Data,2,mean(col1Data),"-"))
yaxis<-dnorm(col1Data,colMeans(col1Data),sd(col1Data))
xaxis<-colData1
plot(xaxis,yaxis)

#Ans6
covDiag<-as.matrix(diag(covMatrix))
DecVar<-as.matrix(order(covDiag,decreasing=TRUE))

#Ans7
Median.matrix<-as.matrix(apply(Data,2,FUN="median"))

#Ans8
NewData<-as.matrix(Data[,1:5])
pairs(NewData)

#Ans9
col1Data<-as.matrix(Data[,1])
hist(col1Data)
hist(col1Data,breaks=c(0,16,350))

#Ans10
corMat<-as.matrix(cor(Data))
orthInd<-which(x==0, arr.ind=TRUE)

#Ans11
f<-as.matrix(t(Data))
EucDist<-as.matrix(dist(f,method="euclidean"))
#g<-max(EucDist)
indices<-which(EucDist==min(EucDist),arr.ind=TRUE)


#Ans12
f<-as.matrix(t(Data))
EucDist<-(dist(f,method="euclidean"))
indices<-which(EucDist==max(EucDist),arr.ind=TRUE)

#Ans13
cosineM<-as.matrix(cosine(Data));
minCos<-which(cosineM==min(cosineM), arr.ind=TRUE)
minDist<-min(cosineM)


