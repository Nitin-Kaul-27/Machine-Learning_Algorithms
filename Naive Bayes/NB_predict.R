NB_predict <- function(New_instance){
  names(Train_data)[label_col_number] <- "Target"
  input_data <- Train_data[,input_col_number]
  C<-count(Train_data[,],vars="Target")
  Prob <- 0
  for (i in 1:nrow(C)){
    Prob[i] <- C[i,2]/sum(C[2]) 
  }
  Prob <- as.data.frame(Prob)
  rownames(Prob) <- C[,1]
  colnames(Prob) <- "Probability of Target labels"
  Final_Prob <- 0
  for (i in 1:nrow(C)){
    P <- 1
    for (j in 1:ncol(input_data)){
      F <- Probabilities[j]
      F <- as.data.frame(F)
      colnames(F) <- C$Target
      P <- P*F[rownames(F)==New_instance[1,j],i]
      
    }
    Final_Prob[i] <- Prob[i,]*P
  }
  Final_Prob <- as.data.frame(Final_Prob)
  rownames(Final_Prob) <- C$Target
  Final <- rownames(Final_Prob)[which(Final_Prob==max(Final_Prob))]
}