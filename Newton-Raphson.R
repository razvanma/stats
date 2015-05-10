# Generate some points.
secret.mu <- c(1, 3)
total.points <- 400
da <- c(secret.mu[1] + rnorm(total.points * 0.25),
        secret.mu[2] + rnorm(total.points * 0.75))


like <- function(X, Y) {
# Log likelihood for generation according to model:
# 0.25 * N(X, 1) + 0.75 * N(Y, 1) 
  res <- c()
  for(i in 1:length(X)) {
    res <- c(res, -sum(log((0.25*dnorm(da - X[i]) + 0.75*dnorm(da - Y[i])))))
  }
  res
}
like.mu <- function(mu) {like(mu[1], mu[2])}

aa <- seq(-2, 5, length.out=50)
bb <- seq(-2, 5, length.out=50)
xyz <- outer(aa, bb, like)

image(aa, bb, xyz)
contour(aa, bb, xyz, add=T, nlevels=100)

start <- c(runif(1, -3, 3), runif(1, -3, 3))
path <- start
for (i in 1:nlm(like.mu, start)$iterations) {
  path <- rbind(path,
                nlm(like.mu, start, iterlim = i)$estimate)
}
lines(path, pch=19,col = "blue", lwd = 3)
print(path)





