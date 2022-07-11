
# MULTIPLE LINEAR REGRESSION IMPLEMENTATION.....................

install.packages('matlib')
library(matlib)
data <- mtcars
head(data)

# Choosing the X and Y variables.................................

target_col <- 1
parameter_col_number <- 2:11 
data_1 <- data[,target_col]
data_2 <- data[,parameter_col_number]
head(data_1)
X_alpha0 <- matrix(1,nrow(data_2),1)
X_alpha0 <- data.frame(X_alpha0)
X <- data.frame(X_alpha0,data_2)
Y <- data_1
head(X)
head(Y)

# Normalizing the input data......................................

normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
X1 <- as.data.frame(lapply(X[,2:ncol(X)], normalize))
X <- data.frame(X[1],X1)
head(X)

# Converting data frame in matrix................................

X <- data.matrix(X)
Y <- data.matrix(Y)
head(X)
head(Y)

# Calculating coefficients using MATRIX method
# Formula used, coefficient matrix = (inverse(XX'))*(X'Y)
# where X is the input matrix or independent variable matrix having all parameters
# X' is the transpose.
# Y is the target variable column
Xt <- t(X)  # Calculating transpose of data

X_Xt <- Xt %*% X # Multiplying transpose with data
X_Xt_inv <- inv(X_Xt)   # Calculating inverse
Xt_Y <- Xt %*% Y # Multiplying transpose with target attribute
coefficients <- X_Xt_inv %*% Xt_Y
head(coefficients)
X <- data.frame(X)
data <- data.frame(X,Y)
head(data)

predicted_output <- matrix(0,nrow(X),1)
predicted_output <- data.frame(predicted_output)
for (i in 1:nrow(X)){
    for (j in 1:ncol(X)){
    predicted_output[i,1] <- coefficients[j] * X[i,j]+ predicted_output[i,1]
  }
}
head(predicted_output)
final_output <- data.frame(data,predicted_output)
head(final_output)


# Predicting a new instance.......................................

B <- c(4,157,100,3.85,3,17.04,0,1,3,2)
A <- rbind(data_2,B)
A <- as.data.frame(lapply(A, normalize))
A <- A[nrow(A),]
instance_prediction <- 0
for (i in 1:length(A)){
  instance_prediction <- A[i]*coefficients[i+1] + instance_prediction
}
instance_prediction <- round(instance_prediction + coefficients[1],2)
paste("Prediction for new instance is", instance_prediction)





