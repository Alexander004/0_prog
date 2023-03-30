cel <- function(x) {
  return (3*x^2-12*x+5)
}

golden <- function(a,b,eps){
  g <- (1+sqrt(5))/2  #само число золотого сечения
  x <- seq(from = a, to = b) #by = 0.05) #создаем последовательность через seq
  y <- cel(x)
  t <- c()
  plot(x, y, type = "l") #type l чтобы была линия, х у название осей
  abline(h = 0)
  while(abs(b-a) >= eps){     #abs как модуль работает
    x1 <- b - ((b-a)/g)
    x2 <- a + ((b-a)/g)
    Yx1 <- cel(x1) #значение от х1 в функции
    Yx2 <- cel(x2)
    if (Yx1 > Yx2){
      a <- x1
      t <- c(t,a)
    }
    else{
      b <- x2
      t <- c(t,b)
    }
  }
  xmin <- (a + b)/2
  ymin <- cel(xmin)
  points(t[1:5], cel(t[1:5]),col = "red") #5 - сколько красных точек
  points(t[(length(t)-4):length(t)], cel(t[(length(t)-4):length(t)]),col = "green", pch = 19)
  #print(t) 
  print((xmin))
  print((ymin))
  return(c(floor(xmin), floor(ymin)))
}

#golden(-100,100,0.01)

koor <- golden(-100,100, 0.01)