library(EMCluster)
SumSSE<-c();
newSum<-c();
clusterNum<-c();
for(k in 4:4){
#k<-4;
#no_of_iterations<-7;
n<-0;
d<-read.table("/home/reen/Desktop/iris.csv",header=TRUE)
m <- as.matrix(d[,1:8])

#Choose k random indices for the first iteration
randCentroid<-(sample(1:nrow(m),k,replace=FALSE))
#print(randCentroid)
#print(length(randCentroid));
#Create Random centroid matrix
vector1=c()
for(i in 1:length(randCentroid)){
  vector1=rbind(vector1,m[randCentroid[i],])
}

#Store vector of vectors
clusterStore<-list();
for(i in 1:k){
  clusterStore[[i]]<-list();
}

#Create a function to cluster all the elements
clustering <- function(vector1){
  for(i in 1:nrow(m)){
    min<-999999;
    index<- -1;
    for(j in 1:k){
      c=(dist(rbind(vector1[j,],m[i,])));
      value<-as.numeric(c);
      if(value < min){
        min<-value;
        index<-j;
      }
    }
    a=c(i);
    clusterStore[[index]]<<-c(clusterStore[[index]],a);
  }
}
#Stores mean for all the clusters
meanMatrix<-c();
meanMatrix<-matrix(0, nrow = k, ncol = 8);
meanM<-matrix(0, nrow = k, ncol = 8);
diffMatrix<-matrix(0, nrow = k, ncol = 8);
flag<-0;
#Initialize clusterStore
initClusterStore<-function(){
  clusterStore<<-list();
  for(i in 1:k){
    clusterStore[[i]]<<-list();
  }
}

#CLustering main function
doClustering<-function(){
  while(flag==0){
    initClusterStore();
    if(n==0){
      clustering(vector1);
    }else{
      clustering(meanMatrix);
    }
    meanM<<-meanMatrix
    meanMatrix<<-c();
    for(i in 1:k){
      a<-c()
      clusterList<-clusterStore[[i]];
      for(j in 1:length(clusterList)){
        a<-rbind(a,m[clusterList[[j]],]);
      }
      meanVec<-colMeans(a);
      meanMatrix<<-rbind(meanMatrix,meanVec)
    }
    diffMatrix<<-(meanMatrix-meanM)
    if(all(diffMatrix==0)){
      flag=1;
    }
    n<<-n+1;
  }
}

#Print Clusters
printCluster<-function(n){
  clusterSelect<-clusterStore[[n]];
  for(i in clusterSelect){
    print(m[i,]);
  }
}

doClustering();

myData<-list();
myDataPlot<-list();
y<-1;
myData<<-NULL;
myDataPlot<-NULL;
getClusters<-function(){
  myData<<-NULL;
  #print(myData);
  for(i in 1:length(clusterStore)){
    vector2<<-NULL;
    for(j in 1:length(clusterStore[[i]])){
      vector2<<-rbind(vector2,m[as.numeric(clusterStore[[i]][j]),])
      vector2<<-as.matrix(vector2)
    }
    myData[[y]]<<-as.matrix(vector2);
    myDataPlot[[y]]<<-as.matrix(vector2[,c(1,5)]);
    y<<-y+1
    #print(myData);
  }  
}

getClusters();
}
# sse<-c();
# sumSq=0;
# sum=0;
# vec1<-c();
# vec2<-c();
# distance=0;
# for( l in 1:nrow(meanMatrix)){
#   sum=0;
#   for(m in 1:nrow(myData[[l]])){
#     
#     z<-lapply(myData[l], "[", i = m, j = );
#     z<-unlist(z);
#     vec1<-z;
#     vec2<-meanMatrix[l,]
#     distance<-dist(rbind(vec1,vec2))
#     sqdist=as.numeric(distance^2);
#     #print(sqdist)
#     sum = sum + sqdist;
#   }
#   sse<<-c(sse,sum)
# }
# clusterNum<-c(clusterNum,k);
# newSum<-sum(sse)
# SumSSE<<-c(SumSSE,newSum)
# }
# 
# plot(clusterNum,SumSSE,type = "o",col = "red", xlab = "Number of Clusters", ylab = "Error", 
#      main = "Error Curve")



plot(c(4:14),c(1:11),col=0);
points((myDataPlot[[1]])[,1],(myDataPlot[[1]])[,2],col=1);
points((myDataPlot[[2]])[,1],(myDataPlot[[2]])[,2],col=2);
points((myDataPlot[[3]])[,1],(myDataPlot[[3]])[,2],col=3);
points((myDataPlot[[4]])[,1],(myDataPlot[[4]])[,2],col=4);


for(i in 1:4){
my<- as.matrix(myData[[i]])
x=prcomp(m)   #Doing Principal component Analysis
s <-as.matrix(x$x)
attr<- s[,c(1,2)]
first.Rnd <- init.EM(attr, nclass = 4,  method = c("Rnd.EM") )
yo=simple.init(attr,nclass=4)
colour=c("indigo","green","purple","red")

plotem(first.Rnd , attr , main="em",col=colour)
}
