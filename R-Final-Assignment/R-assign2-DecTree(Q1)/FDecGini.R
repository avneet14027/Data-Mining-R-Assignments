#Read data matrix fom the source, and convert to a matrix
library(data.tree)
data_matrix<-read.table("C:\\Users\\Purusharth\\Desktop\\Dsktop\\My Courses\\DMG\\R\\Assingment_2\\Titanic.csv",header=TRUE,sep=",");
data_matrix<-data_matrix[,2:6];
View(data_matrix);
#---------------------------------------------------------
#Function to calculate maximum gain for all attributes
calculateMaxGain<-function(targetAttrEntropy,training_set,non_target){
  maxGain<-(-9999);
  maxGainSel<-(-1);
  totalCountAll<-sum(training_set[,ncol(training_set)]);
  for(i in 1:ncol(non_target)){
    colSelected<-non_target[,i];
    uqClasses<-unique(colSelected);
    currentGini<-0;
    for(j in uqClasses){
      colLbl<-colnames(non_target)[i];
      currTrainingset<-training_set[training_set[,colLbl]==j,];
      totalCountAttr<-sum(currTrainingset[,ncol(currTrainingset)]);
      tempcurrentGini<-calculateGini(currTrainingset);
      currentGini<-currentGini+(tempcurrentGini*(totalCountAttr/totalCountAll));
    }
    infoGAIN<-targetAttrEntropy-currentGini;
    #print(infoGAIN);
    if(maxGain < infoGAIN){
      maxGain<-infoGAIN;
      maxGainSel<-i;
    }
  }
  chosenAttrLbl<-colnames(non_target)[maxGainSel];
  attrIndex<-which(colnames(training_set)==chosenAttrLbl);
  #print(attrIndex);
}

#---------------------------------------------------------
#Very specific implementation for this problem
calculateGini<-function(target){
  if(nrow(target)==0)return(0);
  #Calculate "No" count
  noRows<-target[which(target$Survived=="No"),];
  noCount<-sum(noRows[,ncol(noRows)]);
  
  #Calculate "Yes" count
  yesRows<-target[which(target$Survived=="Yes"),];
  yesCount<-sum(yesRows[,ncol(yesRows)]);
  totalCount<-sum(target[,ncol(target)]);
  
  #Calculate gini value
  giniValue<-(noCount/totalCount)*(yesCount/totalCount);
  giniValue;
}
readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}
#---------------------------------------------------------
#Main body function, to compute the decision tree
decisionTree<-function(non_target,training_set){
  
  if(nrow(training_set)==0){
    return(Node$new("Error"));
  }
  if(nrow(non_target)==0 || ncol(non_target)==0){
    #Calculate "No" count
    noRows<-training_set[which(training_set$Survived=="No"),];
    noCount<-sum(noRows[,ncol(noRows)]);
    #Calculate "Yes" count
    yesRows<-training_set[which(training_set$Survived=="Yes"),];
    yesCount<-sum(yesRows[,ncol(yesRows)]);
    if(yesCount>noCount){
      return(Node$new("Yes"));
    }else{
      return(Node$new("No"));
    }
  }
  targetAttrEntropy<-calculateGini(training_set);
  if(targetAttrEntropy==0){
    return(Node$new("zero"));
  };
  
  maxGainAttrIndex<-calculateMaxGain(targetAttrEntropy,training_set,non_target);
  
  #Using the maxGain attribute, select the collumn
  colSelected<-training_set[,maxGainAttrIndex];
  uqClasses<-unique(colSelected);
  colLbl<-colnames(training_set)[maxGainAttrIndex];
  newNonTarget<-(non_target[,which(colnames(non_target)!=colLbl),drop=FALSE]);
  #print(colLbl)
  currNode<-Node$new(colLbl);
  
  for(j in uqClasses){
    currTrainingset<-training_set[training_set[,colLbl]==j,];
    calcEn<-calculateGini(currTrainingset);
    thisNode<-currNode$AddChild(j);
    if(calcEn==0){
      
      noRows<-currTrainingset[which(currTrainingset$Survived=="No"),];
      noCount<-sum(noRows[,ncol(noRows)]);
      #Calculate "Yes" count
      yesRows<-currTrainingset[which(currTrainingset$Survived=="Yes"),];
      yesCount<-sum(yesRows[,ncol(yesRows)]);
      a<-NULL;
      if(yesCount>noCount){
        a<-"Yes";
      }else{
        a<-"No";
      }
      thisNode$AddChildNode(Node$new(a));
    }else{
      thisNode$AddChildNode( decisionTree(newNonTarget,currTrainingset));
      #decisionTree(newNonTarget,currTrainingset)
      
    }
  }
  return(currNode);
}
#---------------------------------------------------------
#Function called here
data_matrix<-data_matrix[which(data_matrix[,ncol(data_matrix)]!=0),];
View(data_matrix);

finalTree<-Node$new("Main");
newNode<-decisionTree(data_matrix[,1:(ncol(data_matrix)-2)],data_matrix);
finalTree$AddChildNode(newNode);

