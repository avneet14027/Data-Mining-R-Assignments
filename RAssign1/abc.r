CatData<-read.table("./Desktop/RAssign1/R-Assign1-2014027/ContingencyTableAnalysis.csv");
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

#Ans2
chiStatistic=c()
chiIndexes=c()
for(i in 1:14){
    col1<-as.matrix(catData[,i])
    col2<-as.matrix(catData[,15])
    tbl1<-table(col1,col2)
    x<-chisq.test(col1,col2)$statistic
    y<-as.numeric(x)
    chiStatistic<-cbind(chiStatistic,y)
    chiIndexes<-cbind(chiIndexes,i)
}

RankingI<-as.matrix(order(chiIndexes,decreasing=FALSE))
RankingS<-as.matrix(order(chiStatistic,decreasing=FALSE))



# col1<-as.matrix(catData[,14])
# col2<-as.matrix(catData[,15])
# col1<-na.omit(col1)
# col2<-na.omit(col2)
# tbl<-table(col1,col2)
# tbl1<-as.matrix(tbl)
# tbl2<-na.omit(tbl1)
# View(tbl1)
# x<-chisq.test(tbl1)$statistic
# View(x)