circle <- function(center = c(0,0), radius = 1, npoints = 100){
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  return(data.frame(x = xx, y = yy))
}

pointsInCircle <- function(center=c(0,0), radius=1, npoints=10, expand = 1) {
  radius <- expand*radius
  a <- runif(npoints)*2*pi
  r <- radius*sqrt(runif(npoints))
  
  x <- r*cos(a) + center[1]
  y <- r*sin(a) + center[2]
  
  return (data.frame(x, y))
}