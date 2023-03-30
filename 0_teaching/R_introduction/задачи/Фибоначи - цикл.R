
fib <- function(n)
{
  fib1 <- 0
  fib2 <- 1
  vec <- c()
  if (n == 1)
  {
    print(0)
    return(0)
  }
  else if (n == 2)
  {
    print(0)
    print(1)
    return (c(0, 1))
  }
  else if (n > 2)
  {
    print(0)
    print(1)
    for (i in 3:n)
    {
      fib3 <- fib1+fib2
      fib1 <- fib2
      fib2 <- fib3
      print(fib2)
      vec <- (c(vec, fib2))
    }
    return(c(0, 1, vec))
  }
}

otvet <- fib(9)


# 0 1 1 2 3 5 8 13