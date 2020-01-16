library(Matrix)
x = read.table("..//data//faithful.txt")

mu1 = c(3, 80)
mu2 = c(3.5, 60)
Sigma1 = matrix(c(0.1, 0, 0, 10), 2, 2)
Sigma2 = matrix(c(0.1, 0, 0, 50), 2, 2)

# em <- function(x, mu1, mu2, Sigma1, Sigma2) {
PI <- 0.5
for( iterations in 1:30 ) {
  # E step
  d1 <- PI * dmnorm(x, mean=mu1, varcov=Sigma1)
  d2 <- (1-PI) * dmnorm(x, mean=mu2, varcov=Sigma2)
  ez <- d2 / (d1+d2)
  
  PI <- mean(ez)
  mu1 <- colSums((1-ez)*x) / sum(1-ez)
  mu2 <- colSums(ez*x) / sum(ez)

  temp1 <- (t(x)-mu1)
  Sigma1 <- (temp1 %*% ((1-ez)*t(temp1))) / sum(1-ez)
  temp2 <- (t(x)-mu2)
  Sigma2 <- (temp2 %*% (ez*t(temp2))) / sum(ez)
  
  result <- list(mu1=mu1, mu2=mu2, Sigma1=Sigma1, Sigma2=Sigma2)
  print(result)
}
result
# }
# result <- em(faithful, mu1, mu2, Sigma1, Sigma2)
# mu1 <- result$mu1
# mu2 <- result$mu2
# Sigma1 <- result$Sigma1
# Sigma2 <- result$Sigma2

# plot the current fit 
library(mixtools)
plot(faithful)

addellipse <- function(mu, Sigma, ...)
{
  ellipse(mu, Sigma, alpha = .05, lwd = 1, ...)
  ellipse(mu, Sigma, alpha = .25, lwd = 2, ...)
}

addellipse(mu1, Sigma1, col = "darkorange")
addellipse(mu2, Sigma2, col = "deepskyblue")


quiz6kmeans <- function(pts, centers) {
  oldCenters <- 0 * centers
  while(any(oldCenters != centers)) {
    oldCenters <- centers
    k <- nrow(centers)
    distFromCenters <- apply(centers, 1, function(center){sqrt(colSums((t(pts)-center)^2))})
    assignment <- apply(distFromCenters, 1, which.min)
    centers <- t(sapply(1:2, function(k){ colMeans(pts[assignment==k, ]) }))
  }
  return(list(centers=centers, assignment=assignment))
}

