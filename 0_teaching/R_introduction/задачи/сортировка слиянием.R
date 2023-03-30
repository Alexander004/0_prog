
func1 <- function(left, right)
{
  a <- c() 
  if (left[length(left)] <= right[1])
  {
    a <- c(left, right)
  }
  else if (right[length(right)] <= left[1])
  {
    a <- c(right, left)
  }
  else
  {
    while (length(left) > 0 & length(right) > 0)
    {
      if (left[1] <= right[1])
      {
        a <- c(a, left[1])
        left <- left[-1]
      }
      else
      {
        a <- c(a, right[1])
        right <- right[-1]
      }
    }
    
    #отдельно проверяем, так как последний элемент еще остался
    while (length(left) > 0)
    {
      a <- c(a, left[1])
      left <- left[-1]
    }
    while (length(right) > 0)
    {
      a <- c(a, right[1])
      right <- right[-1]
    }
  }
  return(a)
}

func2 <- function(x)
{
  if (length(x) < 2)
  {#print(x)
    return(x)
    
  }
  else
  {
    #floor - в меньшую сторону
    m <- floor(length(x)/2)
    left <- func2(x[1:m])
    #print(left)
    right <- func2(x[(m+1):length(x)])
    #print(right)
    resultat <- func1(left, right)
    return(resultat)
  }
}

x <- c(-7,25,99,300,42,-100)

y <- func2(x)
print(y)

