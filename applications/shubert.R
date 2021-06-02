### Shubert function
shubert <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  ii <- c(1:5)
  sum1 <- sum(ii * cos((ii+1)*x1+ii))
  sum2 <- sum(ii * cos((ii+1)*x2+ii))
  y <- sum1 * sum2
  return(y)
}
