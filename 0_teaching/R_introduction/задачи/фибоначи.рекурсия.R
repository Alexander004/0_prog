
fib <- function(n)
{
  if (n == 1)
    {
    return (0)
    }
  else if (n == 2) 
  {
    return (1)
  }
  else if (n > 2)
  {
    return(fib(n-1) + fib(n-2))
    #print(fib(n-1) + fib(n-2))
  }
}

otvet <- fib(7)
print(otvet)

#выводит последнее число начиная с 0
# 0 1 1 2 3 5 8 13