NB_probablity <- function(data,label_col_number,input_col_number){
  names(data)[label_col_number] <- "Target"
  input_data <- data[,input_col_number]
  C<-count(data[,],vars="Target")
  Prob <- 0
  for (i in 1:nrow(C)){
    Prob[i] <- C[i,2]/sum(C[2]) 
  }
  Prob <- as.data.frame(Prob)
  rownames(Prob) <- C[,1]
  colnames(Prob) <- "Probability of Target labels"
  Probabilities <-0
  for (i in 1:ncol(input_data)){
    L <- count(input_data[,],vars = names((input_data[i])) )
    df <- as.data.frame(matrix(0,nrow(L),nrow(C)))
    rownames(df) <- L[,1]
    colnames(df) <- C[,1]
    A <- data.frame((input_data[,i]),data[,label_col_number])
    colnames(A) <- c(names(input_data)[i],"Target")
    for (k in 1:nrow(df)){
      for (j in 1:ncol(df)){
        df[k,j] <- (sum(A[1]==rownames(df)[k] & A$Target ==colnames(df)[j]))/C[C$Target==colnames(df)[j],2]
      }
    }
    Probabilities[i] <- list(df)
    
  }
  names(Probabilities) <- names(input_data)
  print(Prob)
  print(Probabilities)
}




