finalData<-read.table("/home/reen/Desktop/NumericDataAnalysis.csv");
attach(finalData)
Data<-as.matrix(finalData[,1:10]);

#Ans1

# cov() has been used in the end to verify results from both methods.
#Finding out covariance matrix using formula
#Formula: 1/N((A^-1)*A)) where A is the difference matrix.
#Difference matrix: Each element is defined by subtracting the mean of that particular column from that particular element. 
#--->First Difference matrix is found by subtracting each column from column wise mean.
#--->transpose of diff matrix is found.
#--->formula applied.
my.matrix=c() #vector which stores the elements of a matrix

#--->loop for 10 columns , each time a column is extracted from the original matrix and stroed in col1 Data
#--->difference is calucalted by appying sweep function which subtract columnwise mean from each element.
#--->result stored in my.matrix vector.
for(i in 1:10){
  colData<-as.matrix(Data[,i])
  a<-sweep(colData,2,colMeans(colData),"-")
  my.matrix<-cbind(my.matrix,a)
}
N<-nrow(Data)
transpose.matrix=t(my.matrix)
covMatrix<-as.matrix(1/N*(t(my.matrix)%*%(my.matrix)))
View(covMatrix)

#using cov()
covVerify<-as.matrix(cov(Data))
covV<-as.matrix(covVerify[,1:10])
View(covVerify)


