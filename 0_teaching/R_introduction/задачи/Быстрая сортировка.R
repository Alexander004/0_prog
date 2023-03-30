
quicksort <- function(massiv)
{
  n <- length(massiv)
  oporn <- massiv[1]
  left <- c()
  right <- c()
  
  for (i in 2:n)
    {
    if(oporn > massiv[i])
      {
      left <- (c(left, massiv[i]))
      }
    else 
      right <- (c(right, massiv[i])) 
  }
  
  if (length(left) > 1)
    {
    left <- quicksort(left)
    }
  if (length(right) > 1) 
    {
    right <- quicksort(right)
    }
  
  return(c(left, oporn, right))
}

otvet <- quicksort(c(1, 0, 3, 2, 4, 8))