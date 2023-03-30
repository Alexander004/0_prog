zel <- function(x,y){
  return(-x^2-y^2+10)
}

dvig <- function(x0,y0,h){
  xh <- c(0,-h,+h)[which.min(c(zel (x0, y0), 
                                zel (x0-h, y0),
                                zel (x0+h, y0)))]
  yh <- c(0,-h,+h)[which.min(c(zel (x0+xh, y0), 
                                zel (x0+xh, y0-h),
                                zel (x0+xh, y0+h)))]
  return(c(xh, yh, h))
}

pattern_search <- function(x0, y0, h, E, nmax) {
  counter <- 0
  vekh <- dvig(x0, y0, h)
  counter <- counter + 6    #почему тут сразу 6 уже
while (h > E & counter <= nmax){
    while ((vekh[1]^2+vekh[2]^2) == 0 & h > E & counter <= nmax){ #все одинаково, нет разницы куда идти
      h <- h/2
      vekh <- dvig(x0, y0, h)
      counter <- counter + 6
    }
    if ((vekh[1]^2+vekh[2]^2) != 0 & h > E & counter <= nmax){ #когда есть куда шагать дальше
      x0 <- x0 + vekh[1]
      y0 <- y0 + vekh[2]
      while (zel(x0, y0) < zel(x0 + vekh[1], y0 + vekh[2])) {
        x0 <- x0 + vekh[1]
        y0 <- y0 + vekh[2]
      }
      vekh <- dvig(x0, y0, h)
      counter <- counter + 6
    }
  }
  return(c(x0, y0, counter))
}

pattern_search(10, 25, 6, 0.1, 100)