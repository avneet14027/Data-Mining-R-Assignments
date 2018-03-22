#library(mclust)
library(EMCluster)
d<-read.table("C:/Users/Anisha Sejwal/Downloads/iris.csv",header=FALSE,sep=",")
m <- as.matrix(d[,1:8])
x=prcomp(m)   #Doing Principal component Analysis
s <-as.matrix(x$x)
attr<- s[,c(1,2)]
first.Rnd <- init.EM(attr, nclass = 4,  method = c("Rnd.EM") )
yo=simple.init(attr,nclass=4)
plotem(first.Rnd , attr , main="em")

