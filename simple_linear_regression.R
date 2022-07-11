
# SIMPLE LINEAR REGRESSION IMPLEMENTATION

install.packages('ggplot2')
library(ggplot2)

# Reading the dataset
data <- cars
head(data)

# Choosing the X and Y variables
X <- 1
Y <- 2
data_1 <- data[,c(X,Y)]
head(data_1)

#Ploting the dataset
ggplot(data,aes(x=data_1[,1],y=data_1[,2]))+geom_point(color="Blue")

# Normalizing the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data_1 <- as.data.frame(lapply(data_1, normalize))
head(data_1)

 # Calculating linear equation
num1 <- (sum(data_1[,2])*sum(data_1[,1]^2))-(sum(data_1[,1])*sum(data_1[,1]*data_1[,2]))
den  <- (nrow(data_1)*sum(data_1[,1]^2))-(sum(data_1[,1]))^2
alpha_0 <- num1/den
num2 <- (nrow(data_1)*sum(data_1[,1]*data_1[,2])) - (sum(data_1[,1])*sum(data_1[,2]))
alpha_1 <- num2/den

# Calculating predicted output
for (i in 1:nrow(data_1)){
  data_1$predicted_Y[i] <- alpha_0 + alpha_1 * data_1[i,1]
}
head(data_1)

# Ploting actual and predicted value
ggplot(data)+
  geom_point(aes(x=data_1[,1],y=data_1[,2]),color="Blue")+
  geom_line(aes(x=data_1[,1],y=data_1$predicted_Y),color="Red", size=2)

