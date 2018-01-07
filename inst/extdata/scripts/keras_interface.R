library (corrplot)
library (keras)
install_keras()


# Read in `iris` data
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE)

# Return the first part of `iris`
head(iris)

# Inspect the structure
str(iris)

# Obtain the dimensions
dim(iris)

names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

plot(iris$Petal.Length,
     iris$Petal.Width,
     pch=21, bg=c("red","green3","blue")[unclass(iris$Species)],
     xlab="Petal Length",
     ylab="Petal Width")

# Overall correlation between `Petal.Length` and `Petal.Width`
cor(iris$Petal.Length, iris$Petal.Width)

# Store the overall correlation in `M`
M <- cor(iris[,1:4])

# Plot the correlation plot with `M`
corrplot(M, method="circle")

# Pull up a summary of `iris`
summary(iris)

# convert the data to numbers
iris[,5] <- as.numeric(iris[,5]) -1

# Turn `iris` into a matrix
iris <- as.matrix(iris)

# Set `iris` `dimnames` to `NULL`
dimnames(iris) <- NULL

# Normalize the `iris` data
#iris <- normalize(iris[,1:4])

# Return the summary of `iris`
#summary(iris)

# Determine sample size
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# Split the `iris` data
iris.train.x <- iris[ind==1, 1:4]
iris.test.x <- iris[ind==2, 1:4]

# Split the class attribute
iris.train.y <- iris[ind==1, 5]
iris.test.y <- iris[ind==2, 5]

# One hot encode training target values
iris.trainLabels <- to_categorical(iris.train.y)

# One hot encode test target values
iris.testLabels <- to_categorical(iris.test.y)

# Print out the iris.testLabels to double check the result
print(iris.testLabels)
