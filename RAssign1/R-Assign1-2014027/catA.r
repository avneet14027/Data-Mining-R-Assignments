CatData<-read.table("/home/reen/Desktop/ContingencyTableAnalysis.csv");
attach(CatData)
catData<-as.matrix(CatData);

#Ans1
max1<-0
min1<-2901978
maxIndex=c()
minIndex=c()
for(i in 1:14){
  for(j in i:14){
    col1<-as.matrix(catData[,i])
    col2<-as.matrix(catData[,(j+1)])
    tbl1<-table(col1,col2)
    x<-chisq.test(col1,col2)$statistic
    y<-as.numeric(x)
    if(y>max1){
      max1=y;
      maxIndex<-cbind(maxIndex,i)
      maxIndex<-cbind(maxIndex,j)
    }
    if(y<min1){
      min1=y
      minIndex<-cbind(minIndex,i)
      minIndex<-cbind(minIndex,j)
    }
  }    
}
print(max1)
print(min1)
