function2 <- function(n,t,p) {
  u <- replicate(p, function1(n,t), simplify = "array")
  k <- sum(u)
  g <- k/p
  return(g)
}