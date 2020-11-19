#' CLT Uniform function
#'
#' My Central Limit Function
#'
#' @param n
#' @param iter
#' @param a
#' @param b
#'
#' @return a Graph with all data needed to find the cetral limit
#' @export
#'
#' @examples
#' mycltu(n=20,iter=100000)
mycltu=function(n,iter,a=0,b=10){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=max(param$density)
  ymax=1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean", "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  lines(density(w),col="Blue",lwd=3)
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
