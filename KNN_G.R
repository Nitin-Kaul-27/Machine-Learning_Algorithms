library(plyr)

# Reading_input and changing data type ----------------------------------------------------------
data<-mtcars
head(data)
label_col_number <- 11 
input_col_number <- 1:10
names(data)[label_col_number] <- "Target"

for (i in 1:length(input_col_number)){
  if (is.character(data[,input_col_number[i]])==TRUE){
    data[,input_col_number[i]] <- factor(data[,input_col_number[i]])
  }
  levels(data[,input_col_number[i]]) <- 0:(length(levels(data[,input_col_number[i]]))-1)
  data[,input_col_number[i]] <- as.character(data[,input_col_number[i]])
  data[,input_col_number[i]] <- as.integer(data[,input_col_number[i]])
}

# Normalize the data ------------------------------------------------------

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

norm_data <- as.data.frame(lapply(data[input_col_number], normalize))
data[input_col_number] <- norm_data
data <- data[,c(input_col_number,label_col_number)]

# Splitting Training and Testing data -------------------------------------

Train_data_size <- 0.7
Test_data_size <- 0.3
Train_data <- data[1:round(Train_data_size*nrow(data)),]
Test_data <- data[(round(Train_data_size*nrow(data))+1):nrow(data),]

# Finding nearest neighbours ----------------------------------------------

K=round(sqrt(nrow(data)))
for (m in 1:nrow(Test_data)) {
  for (i in 1:nrow(Train_data)) {
    Train_data$distance[i] <- 0
    for (j in 1:length(input_col_number)){
      Train_data$distance[i]<-((Test_data[m,j]-Train_data[i,j])^2)+Train_data$distance[i]
    }
    Train_data$distance[i] <- sqrt(Train_data$distance[i])
    
  }
  b <- Train_data[order(Train_data$distance),]
  rownames(b) <- NULL
  vars_value <- names(b)
  label_count <- count(b[1:K,],vars="Target")
  max_freq <- label_count[which(label_count$freq==max(label_count$freq)),]
  Test_data$Predicted_output[m] <- max_freq[,1] 
}

# Creating Confusion matrix -----------------------------------------------

L <- length(levels(factor(Test_data$Target)))
df <- as.data.frame(matrix(0,L,L))
rownames(df) <- levels(factor(Test_data$Target))
colnames(df) <- levels(factor(Test_data$Target))
a <- 0
c <- 1
for (i in 1:(L*L)){
  B <- colnames(df)[i-a] 
  A <- rownames(df)[c]
  H <- 0
  for (j in 1:nrow(Test_data)){
    if ( (Test_data$Target[j]== A) & (Test_data$Predicted_output[j] == B)){
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
paste("Accuracy of KNN algorithm is:", Acc ,"%")









