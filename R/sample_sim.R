#' @title Sample Simulations
#'
#' @param x Number of items sampled
#' @param n Sample size
#' @param iter Number of iterations
#' @param time Time between each iteration
#'
#' @return A plot of proportions of x selected in each individual iteration
#' @export
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#'
#' @examples
#' sample_sim(x=10, n=100, iter=10, time=0.5)
sample_sim=function(x, n, iter=10,time=0.5){
  for( i in 1:iter){
    #Make a sample
    s=sample(1:x,n,replace=TRUE)
    #Convert the sample into a factor
    sf=factor(s,levels=1:x)
    #Make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.5)
    )

    #Timer between tables
    Sys.sleep(time)
  }
}
