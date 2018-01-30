function1 <- function(n,t) {
  x <- rnorm(n)
  y <- x[x<=t]
  z <- length(y)
  q <- z/n
  return(q) 
}