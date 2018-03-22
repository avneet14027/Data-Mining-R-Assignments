  k<-4;
  no_of_iterations<-11;
  n<-1;
  counter<-1;
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
  
  IntermediateClusterStore<-list();
  for(i in 1:no_of_iterations){
    IntermediateClusterStore[[i]]<-list();
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
  
  #Get clusters from indexes
  myData<-list();
  myDataPlot<-list();
  y<-1;
  myData<<-NULL;
  myDataPlot<-NULL;
  getClusters<-function(clusterStore){
    for(i in 1:length(clusterStore)){
      vector2<<-NULL;
      for(j in 1:length(clusterStore[[i]])){
        vector2<<-rbind(vector2,m[as.numeric(clusterStore[[i]][j]),])
        vector2<<-as.matrix(vector2)
      }
      myData[[y]]<<-as.matrix(vector2);
      myDataPlot[[y]]<<-as.matrix(vector2[,c(1,5)]);
      y<<-y+1
    }  
    return(myData)
  }
  
  
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
  myNewData<-list();
  myNewData1<-list();
  doClustering<-function(){
    while(n<=no_of_iterations){
      initClusterStore();
      if(n==1){
        clustering(vector1);
      }else{
        clustering(meanMatrix);
      }
      
      #Store intermediate cluster
      myNewData<<-getClusters(clusterStore);
      myNewData1<<-myNewData[c(counter:(counter+k-1))];
      counter<<-counter+k;
      IntermediateClusterStore[[n]]<<-c(IntermediateClusterStore[[n]],myNewData1);
      
      #Stores mean for all the clusters
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
      diffMatrix<<-(meanMatrix-meanM);
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
  



#plot intermediate cluster
  x<-c()
  inter<-c()
  interClust<<-list();
  
#   plotList<-list();
#   for(i in 1:4){
#     plotList[[i]]<-list();
#   }

  #plot intermediate Clusters
  plotClust<-function(IntermediateClusterStore,i){
  #plotData<-IntermediateClusterStore[1];
  plot(c(4:14),c(1:11),col=0,xlab="attr1",ylab="attr5");
  for(ctr in 1:k){
    inter<<-(IntermediateClusterStore[[i]][ctr])
    interClust<<-inter[[1]][,c(1,5)]
    #print(interClust)
    points(interClust[,1],interClust[,2],col=ctr);
  #plotList[[ctr]]<<-c(plotList[[ctr]],interClust[,1]);
  }
  
  #interClust<<-inter[[i]][,c(1,5)]
  #y<-IntermediateClusterStore[[1]][1]
  #a<<-interClust[,1]
  }
 
plotClust(IntermediateClusterStore,2)
plotClust(IntermediateClusterStore,11)  

 #plot final Cluster
 plotFinal<-function(){
 plot(c(4:14),c(1:11),col=0,xlab="attr1",ylab="attr5");
 for(cnt in 1:k){
 points((myDataPlot[[cnt]])[,1],(myDataPlot[[cnt]])[,2],col=cnt);  
 }
 }
  
 #plotFinal();
  
# points((myDataPlot[[1]])[,1],(myDataPlot[[1]])[,2],col=1);
# points((myDataPlot[[2]])[,1],(myDataPlot[[2]])[,2],col=2);
# points((myDataPlot[[3]])[,1],(myDataPlot[[3]])[,2],col=3);
# points((myDataPlot[[4]])[,1],(myDataPlot[[4]])[,2],col=4);


