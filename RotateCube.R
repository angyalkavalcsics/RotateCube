#degtoRadian
rad = function(deg) {
  return((deg*pi)/180)
}
# rotating cube
rotcube <- function(X){
  angles = matrix(seq(0,360, by = 2))
  for(ang in angles){
      Tz= matrix(c(cos(rad(ang)), -sin(rad(ang)),0,  
               sin(rad(ang)), cos(rad(ang)), 0 , 
               0, 0,1), 
             byrow = TRUE,  nrow = 3, ncol = 3)
      Tx= matrix(c(1,0,0,
               0,cos(rad(ang)), -sin(rad(ang)),
               0,  sin(rad(ang)), cos(rad(ang))),
             byrow = TRUE,  nrow = 3, ncol = 3)
      Ty= matrix(c(cos(rad(ang)), 0,sin(rad(ang)),
               0,1,0, 
               -sin(rad(ang)), 0,cos(rad(ang))),
             byrow = TRUE,  nrow = 3, ncol = 3)
      P = matrix(c(1,0,0,
               0,1,0), byrow = TRUE,  nrow = 2, ncol = 3)
      #update rotation and plot
      R = P%*%Ty%*%Tx%*%Tz%*%X
      plot(0,0,col='white',xlim=c(-2,2),ylim=c(-2,2),xlab="",ylab="",axes=FALSE)
      segments(X[1,1],X[2,1],X[1,3],X[2,3],lwd=3,col='blue')
      segments(X[1,3],X[2,3],X[1,4],X[2,4],lwd=3,col='red')
      segments(X[1,4],X[2,4],X[1,2],X[2,2],lwd=3, col = "black")
      segments(X[1,2],X[2,2],X[1,1],X[2,1],lwd=3, col = "green")
      segments(X[1,5],X[2,5],X[1,7],X[2,7],lwd=3, col = "gray")
      segments(X[1,7],X[2,7],X[1,8],X[2,8],lwd=3,col='deepskyblue')
      segments(X[1,6],X[2,6],X[1,5],X[2,5],lwd=3, col = "deeppink1")
      segments(X[1,5],X[2,5],X[1,7],X[2,7],lwd=3,col='chartreuse2')
      segments(X[1,1],X[2,1],X[1,5],X[2,5],lwd=3, col = "black")
      segments(X[1,2],X[2,2],X[1,6],X[2,6],lwd=3, col = "yellow")
      segments(X[1,3],X[2,3],X[1,7],X[2,7],lwd=3, col = "brown")
      segments(X[1,8],X[2,8],X[1,4],X[2,4],lwd=3, col = "red")
    }
}

y <- matrix(c(
  1,1,1,
  1,-1,1,
  -1,1,1,
  -1,-1,1,
  1,1,-1,
  1,-1,-1,
  -1,1,-1,
  -1,-1,-1),nrow = 3, ncol = 8)
rotcube(y)
