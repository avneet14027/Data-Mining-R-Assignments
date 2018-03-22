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
