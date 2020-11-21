#' Bootstrap interval to p-value function
#'
#' Function to calculate the bootstrap interval and p-value
#'
#' @param x1
#' @param x2
#' @param conf.level
#' @param iter
#' @param mudiff
#' @param test
#'
#' @return a Histogram with the bootstrap interval and the calculated p-value
#' @export
#'
#' @examples
#' set.seed(10);x=rnorm(20,mean=4,sd=2);y=rnorm(20,mean=4,sd=2)
#' boot2pval(x1=x,x2=y)
boot2pval<-function(x1,x2,conf.level=0.95,iter=3000,mudiff=0, test="two"){
  n1=length(x1)
  n2=length(x2)
  y1=x1-mean(x1)+mean(c(x1,x2))
  y2=x2-mean(x2)+mean(c(x1,x2))
  y1rs.mat<-c()
  x1rs.mat<-c()
  y2rs.mat<-c()
  x2rs.mat<-c()
  for(i in 1:iter){
    y1rs.mat<-cbind(y1rs.mat,sample(y1,n1,replace=TRUE))
    y2rs.mat<-cbind(y2rs.mat,sample(y2,n2,replace=TRUE))

  }
  x1rs.mat<-y1rs.mat+mean(x1)-mean(c(x1,x2))
  x2rs.mat<-y2rs.mat+mean(x2)-mean(c(x1,x2))

  xbar1=mean(x1)
  xbar2=mean(x2)
  sx1sq=var(x1)
  sx2sq=var(x2)

  tcalc=(xbar1-xbar2-mudiff)/sqrt(sx1sq/n1+sx2sq/n2)

  sy1sq=apply(y1rs.mat,2,var)
  sy2sq=apply(y2rs.mat,2,var)
  y1bar=apply(y1rs.mat,2,mean)
  y2bar=apply(y2rs.mat,2,mean)

  tstat=(y1bar-y2bar-mudiff)/sqrt(sy1sq/n1+sy2sq/n2)


  alpha=1-conf.level
  pvalue=ifelse(test=="two",length(tstat[tstat>abs(tcalc) | tstat < -abs(tcalc)])/iter,
                ifelse(test=="upper",length(tstat[tstat>tcalc])/iter,
                       length(ytstat[tstat<tcalc])/iter))

  h=hist(tstat,plot=FALSE)
  mid=h$mid
  if(test=="two"){
    ncoll=length(mid[mid<= -abs(tcalc)])
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
  }
  hist(tstat,col=col,freq=FALSE)

  return(list(pvalue=pvalue))
}
