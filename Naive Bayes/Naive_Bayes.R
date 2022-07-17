library(plyr)
data <- mtcars
head(data)

label_col_number <- 11 
input_col_number <- c(2,8,9,10)
input_data <- data[,input_col_number]
head(input_data)


# Splitting Training and Testing data -------------------------------------

Train_data_size <- 0.85
Test_data_size <- 0.15
Train_data <- data[1:round(Train_data_size*nrow(data)),]
Test_data <- data[(round(Train_data_size*nrow(data))+1):nrow(data),]
Test_data1 <- Test_data[,input_col_number]

# Calculating Conditional Probability of Attributes w.r.t Target la --------

Probabilities <- NB_probablity(Train_data,label_col_number,input_col_number)


# Predicting Labels for Test data -----------------------------------------------

A <- data.frame(Test_data1,Test_data[,label_col_number])
colnames(A)[ncol(A)] <- "Target"
Prediction <- 0
for (i in 1:nrow(Test_data)){
  New_instance <- Test_data1[i,]
  A$Prediction[i] <- NB_predict(New_instance)
  
}


# Confusion Matrix --------------------------------------------------------

L <- length(levels(factor(A$Target)))
df <- as.data.frame(matrix(0,L,L))
rownames(df) <- levels(factor(A$Target))
colnames(df) <- levels(factor(A$Target))
a <- 0
c <- 1
for (i in 1:(L*L)){
  B1 <- colnames(df)[i-a] 
  A1 <- rownames(df)[c]
  H <- 0
  for (j in 1:nrow(A)){
    if ( (A$Target[j]== A1) & (A$Prediction[j] == B1)){
      H <- H+1
      df[c,i-a] <- H
    }
  } 
  if (i%%L==0){
    a <- a + L 
  }
  if (i%%L==0){
    c <- c + 1 
  }
}
print("CONFUSION MATRIX")
df
Acc <- round((sum(diag(as.matrix(df)))/sum(df))*100,2)
paste("Accuracy of Naive Baye's algorithm is:", Acc ,"%")

# Predicting Individual New Instance --------------------------------------

New_instance <- data.frame(8,0,0,5)
colnames(New_instance) <- names(input_data)
Prediction <- NB_predict(New_instance) 
print(paste("target label for New Instance is = ", Prediction))
