set.seed(1)
n = 300
x = runif(n)
py <- function(x) sin(4*pi*x)/3 + 0.5
y = (rbinom(n, 1, py(x))-0.5)*2

# split_scores <- function(x, y, w) {
#   sapply(x, function(c){
#     left <- x <= c
#     right <- !left
#     return(-(sum(w[left])*gini(y[left], w[left]) + sum(w[right])*gini(y[right], w[right]))/sum(w))
#   })
# }
# 
# gini <- function(y, w) {
#   p <- as.numeric(w %*% (y == 1)) / sum(w)
#   p*(1-p)
# }
# 
# scores <- split_scores(x, y, w)

setClass(
  "Stump",
  slots=c(c="numeric", left="numeric", right="numeric"),
)

setGeneric("classify", function(.Object, newx) standardGeneric("classify"))

setMethod("classify", "Stump", function(.Object, newx) {
  ifelse(newx < .Object@c, .Object@left, .Object@right)
})

Stump <- function(x, y, w) {
  n <- length(y)
  # optimized split criteria calculation by first sorting x-values and then use dynamic programming to update summations
  split_scores_dp <- function(x, y, w) {
    sum_w <- sum(w)
    left.sum_w <- 0
    left.sum_wy <- 0
    right.sum_w <- sum_w
    right.sum_wy <- as.numeric(w %*% (y==1))
    
    x_order <- order(x)
    scores <- vector("numeric", n-1)
    scores[x_order] <- mapply(function(y_i, w_i){
      wy <- w_i * (y_i==1)
      left.sum_w <<- left.sum_w + w_i
      left.sum_wy <<- left.sum_wy + wy
      right.sum_w <<- right.sum_w - w_i
      right.sum_wy <<- right.sum_wy - wy
      score <- (left.sum_w * gini_dp(left.sum_wy, left.sum_w) + right.sum_w * gini_dp(right.sum_wy, right.sum_w)) / -sum_w
      return(score)
    }, y[x_order], w[x_order])
    
    return(scores)
  }
  
  gini_dp <- function(sum_wy, sum_w) {
    p <- sum_wy / sum_w
    p * (1-p)
  }
  
  weighted_majority <- function(cond) {
    pos_cond <- cond && y == 1
    neg_cond <- cond && y == -1
    ifelse(sum(w[pos_cond]) > sum(w[neg_cond]), 1, -1)
  }
  simple_majority <- function(cond) {
    ifelse(sum(y[cond]==1) > sum(y[cond]!=1), 1, -1)
  }
  scores <- split_scores_dp(x, y, w)
  best_c <- x[which.max(scores)]
  new("Stump", c=best_c, left=weighted_majority(x <= best_c), right=weighted_majority(x > best_c))
}

setClass(
  "AdaboostModel",
  slots=c(a="numeric", c="list", exponential_loss="numeric"),
)

setGeneric("F", function(.Object, newx) standardGeneric("F"))
setMethod("F", "AdaboostModel", function(.Object, newx){
  rowSums(mapply(function(a_t, c_t){
    a_t * classify(c_t, newx)
  }, .Object@a, .Object@c))
})

setMethod("classify", "AdaboostModel", function(.Object, newx) {
  ifelse(F(.Object, newx) > 0, 1, -1)
})

adaBoost <- function(x, y, T, shrinkage) {
  n <- length(y)
  w <- rep(1/n, n)
  classifiers <- vector("list", T)
  loss <- y * 0
  exponential_loss <- vector("numeric", T)

  a <- sapply(1:T, function(k) {
    c <- Stump(x, y, w)
    classifiers[[k]] <<- c
    
    f_k <- classify(c, x)
    e_k <- sum(w * (f_k != y))
    a_k <- log((1-e_k)/e_k) / 2
    w_new <- w * exp(-a_k * shrinkage * y * f_k)
    w <<- w_new / sum(w_new)
    
    # exponential loss
    loss <<- loss - y * shrinkage * (a_k * f_k)
    exponential_loss[k] <<- mean(exp(loss))
    
    return(a_k)
  })
  
  new("AdaboostModel", a=a, c=classifiers, exponential_loss=exponential_loss)
}

testx = seq(0, 1, length.out = 1000)
testy = (rbinom(1000, 1, py(testx))-0.5)*2

shrinkageValues <- c(0.1, 0.2, 0.4, 0.7, 1.0)
shrinkageErrors <- NULL
shrinkageExpErr <- NULL
for(k in 1:length(shrinkageValues)) {
  shrinkage <- shrinkageValues[k]
  model <- adaBoost(x, y, 50, shrinkage)
  pred <- classify(model, x)
  trainErr <- sum(pred != y) / length(y) * 100
  pred <- classify(model, testx)
  testErr <- sum(pred != testy) / length(testy) * 100
  shrinkageErrors <- cbind(shrinkageErrors, c(trainErr, testErr))
  shrinkageExpErr <- cbind(shrinkageExpErr, model@exponential_loss)
}

colnames(shrinkageErrors) <- shrinkageValues
barplot(shrinkageErrors, main="Training/testing error vs shrinkage values",
        xlab="shrinkage", ylab="error %", col=c("darkblue","red"),
        legend=c("training", "test"), beside=TRUE)

matplot(shrinkageExpErr, type="b", ylim=c(0.85, 1.1), col=1:5, pch=1:5, main="Exp error vs iterations, shrinkage", xlab="Iterations", ylab="ExpErr")
legend(x="topright", lty=2, legend=shrinkageValues, col=1:5, pch=1:5)

model <- adaBoost(x, y, 10000, 0.8)

output.train <- F(model, x)
pred.train <- classify(model, x)
paste("Training error: ", mean(pred.train==y))
plot(x, y + 0.1*runif(n, -1, 1), ylim=c(-1.1, 1.1), pch=19, col=ifelse(pred.train==1, "darkorange", "deepskyblue"), ylab="y", main="Training classification")
lines(sort(x), py(x)[order(x)]-0.5)
lines(sort(x), output.train[order(x)], type="l")

n <- length(testy)
output.test <- F(model, testx)
pred.test <- classify(model, testx)
paste("Test error: ", mean(pred.test == testy))
plot(testx, testy + 0.1*runif(n, -1, 1), ylim=c(-1.1, 1.1), pch=19, lwd=0.01, col=ifelse(pred.test==1, "darkorange", "deepskyblue"), ylab="y", main="Testing classification")
lines(sort(testx), py(testx)[order(testx)]-0.5)
lines(testx, output.test, type="l")
