finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);


#Ans5
# Two plots are plotted
#---->in one of them all points are spread around 0, in the other it is plotted without subtracting the mean from the column vector
col1Data<-as.matrix(Data[,1])
colData1<-as.matrix(sweep(col1Data,2,mean(col1Data),"-"))
yaxis<-dnorm(col1Data,colMeans(col1Data),sd(col1Data))
xaxis<-colData1
plot(xaxis,yaxis)
colData<-as.matrix(Data[,1])
y<-dnorm(colData,colMeans(colData),sd(colData))
x<-colData
plot(x,y)