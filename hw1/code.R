myknn <- function(xtest, xtrain, ytrain, k) {
  apply(xtest, 1, function(a){
    distances <- apply(xtrain, 1, function(b) norm(a-b, type="2"))
    mean(ytrain[order(distances)[1:k]])
  })
}

#install.packages('mvtnorm', repos = 'https://mirror.its.sfu.ca/mirror/CRAN/', quiet = FALSE, verbose = TRUE)
covar <- 0.5^abs(sapply(0:4, function(i) i:(i-4), simplify=TRUE))

library("mvtnorm")
set.seed(1)
X <- rmvnorm(1000, 1:5, covar)
colnames(X) <- sprintf("X%d", 1:5)
Y <- X[,1] + X[,2] + (X[,3]-2.5)^2 + rnorm(1000)
colnames(Y) <- "Y"
train.X <- X[1:400,]
train.Y <- Y[1:400]
test.X <- X[401:1000,]
test.Y <- Y[401:1000]

library("parallel")
knn.result <- list()
system.time({
  knn.mse <- mcmapply(function(k) {
    knn.result[[k]] <- myknn(test.X, train.X, train.Y, k)
    c(k, mean((knn.result[[k]] - test.Y)^2))
  }, c(1:9, seq(10,100,5)), mc.cores=8)
})

plot(knn.mse[1,], knn.mse[2,], xlab="k", ylab="MSE")

train.df <- as.data.frame(cbind(train.Y, train.X))
colnames(train.df)[1] <- "Y"
linearModel <- lm(Y ~ ., data=train.df)

test.df <- as.data.frame(test.X)
linearModel.mse <- mean(predict(linearModel, test.df))

mylm_g <- function(x, y, delta, epsilon, maxitr) {
  n <<- dim(x)[1]
  p <<- dim(x)[2]
  b_old <<- rep(1, p)
  xi_yi <<- rowSums(sapply(1:n, function(i){ x[i,] * y[i] })) / n
  xi_2 <<- colSums(x^2)
  for (i in 1:maxitr) {
    gradient <<- (xi_2 %*% b_old) / n - xi_yi
    b_new <<- b_old - gradient * delta
    if (norm(b_new - b_old, type="2") < epsilon) {
      break
    }
    b_old <<- b_new
  }
  print(i)
  return (b_old)
}

mylm_g <- function(x, y, delta, epsilon, maxitr) {
  n <<- dim(x)[1]
  p <<- dim(x)[2]
  b_old <<- rep(1, p)
  for (i in 1:maxitr) {
    intermediate <<- sapply(1:n, function(i){ c(y[i] - x[i,] %*% b_old) * x[i,] })
    gradient <<- -apply(intermediate, 1, mean)
    b_new <<- b_old - gradient * delta
    if (norm(b_new - b_old, type="2") < epsilon) {
      break
    }
    b_old <<- b_new
  }
  print(i)
  return (b_old)
}
