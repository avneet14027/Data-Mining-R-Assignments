finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);

#first, it is plotted with pairs() function then individually

#Ans8
NewData<-as.matrix(Data[,1:5])
pairs(NewData)

attr1<-Data[,1]
attr2<-Data[,2]
plot(attr1,attr2)

attr1<-Data[,1]
attr3<-Data[,3]
plot(attr1,attr3)

attr1<-Data[,1]
attr4<-Data[,4]
plot(attr1,attr4)

attr1<-Data[,1]
attr5<-Data[,5]
plot(attr1,attr5)

attr2<-Data[,2]
attr3<-Data[,3]
plot(attr2,attr3)

attr2<-Data[,2]
attr4<-Data[,4]
plot(attr2,attr4)

attr2<-Data[,2]
attr5<-Data[,5]
plot(attr2,attr5)

attr3<-Data[,3]
attr4<-Data[,4]
plot(attr3,attr4)

attr3<-Data[,3]
attr5<-Data[,5]
plot(attr3,attr5)

attr4<-Data[,4]
attr5<-Data[,5]
plot(attr4,attr5)