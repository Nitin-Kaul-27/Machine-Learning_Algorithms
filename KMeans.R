
# K-MEAN CLUSTERING

data <- read.csv(file.choose())                         #  Choose the .csv file for Clustering
input_columns <- c(2,3,4,5,6,8,9,10,11)                 #  Initialize the column number of inputs to be selected for processing
data <- data[,input_columns]
head(data)
str(data)                                               #  Check all the input columns must be of numerical category

# Initializing number of clusters
K <- 5

# Selecting Random Centroids as per K value
n <- ncol(data)
Centroid <- as.data.frame(matrix(0,n,nrow=K))
for (i in 1:K) {
  for (j in 1:ncol(data)) {
    Centroid[i,j] <- round(runif(1,min(data[,j]),max(data[,j])))
  }
}
Centroid
condition <- 0
iteration <- 0
Cluster_list <- list()
k <- 1
while (condition==0) {
  P <- paste("Iteration:::", iteration)
  print(P)
  
  # Method 1 to find distance
  Distance <- as.data.frame(matrix(0,nrow(data),nrow(Centroid)))
  for (i in 1:nrow(data)){
    for (j in 1:K){
      T <- abs(data[i,]-Centroid[j,])
      T <- (sum(T))
      Distance[i,j] <- T
    }
  }
  #print(Distance)
  
  # Method 2 to find distance (Euclidean distance)
  # for (i in 1:nrow(data)){
  #   for (j in 1:K){
  #     T <- (data[i,]-Centroid[j,])^2
  #     T <- sqrt(sum(T))
  #     Distance[i,j] <- T
  #   }
  # }
  # print(Distance)
  
  Cluster <- 0
  for (i in 1:nrow(data)){
    index <- which(Distance[i,] == min(Distance[i,]))
    if(length(index)>1){
      index <- index[1]
    }
    Cluster[i] <- paste('Cluster',index)
  }
  Distance
  Cluster
  Cluster_list[k] <- list(Cluster)
  #print(Cluster_list)
  if(length(Cluster_list)>1){
    A <- as.data.frame(Cluster_list[k])
    B <- as.data.frame(Cluster_list[k-1])
    C <- as.vector(A==B)
    count <- 0
    for (i in 1:length(C)) {
      if(isTRUE(C[i])==TRUE){
        count <- count+1
      }
    }
    if(count==length(C)){
      data$Clusters <- Cluster
      print("..............")
      print("Final Cluster are as below")
      print(data)
      print("..............")
      break
    }
  }
  
  L <- as.factor(Cluster)
  L <- levels(L)
  Centroid_update <- matrix(0,nrow(Centroid),ncol(Centroid))
  for (i in 1:length((L))) {
    L1 <- which(Cluster==L[i])
    L2 <- data[L1,]
    for (j in 1:ncol(L2)) {
      Centroid_update[i,j] <- mean(L2[,j])
    }
  }
  Centroid <- Centroid_update
  iteration <- iteration+1
  k <- k+1
  
}




