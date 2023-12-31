#' @title My Bootstrapping Function
#'
#' @param iter Number of iterations to bootstrap
#' @param x The sample to perform bootstrapping on
#' @param fun The function to be estimated
#' @param alpha The alpha error level
#' @param cx Size of plotting text and symbols
#' @param ... Other parameters for the produced histogram
#'
#' @return A histogram of the sample statistics for each iteration followed by a point estimate for the population parameter and a 100(1-alpha)% confidence interval for the same parameter. Also a list of confidence intervals, functions, and samples.
#' @export
#' @importFrom graphics segments
#' @importFrom graphics text
#' @importFrom stats quantile
#'
#' @examples
#' myboots(iter=10000, x=c(1,2,3,4,5,6,7,8,9,10), fun='mean', alpha=0.05, cx=1.5)
myboots<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  return(invisible(list(ci=ci,fun=fun,x=x)))# Some output to use if necessary
}
