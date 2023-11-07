#' @title My Maximum Likelihood Estimator Function
#'
#' @param lfun The log function of the d-stem distribution with x=x and param=param, with select sizes
#' @param x The collection of estimated parameters from the data
#' @param param The range of parameter values for the maximum likelihood estimate to choose from
#' @param ... Other parameters for the resulting plot
#'
#' @return A plot of the log function with the Maximum Likelihood Estimate labeled and a list with the index, the parameter of each index, the sum of parameters at each index, and the slope values.
#' @export
#' @importFrom graphics axis
#' @importFrom graphics points
#'
#' @examples
mymaxlik=function(lfun,x,param,...){
  np=length(param)
  # this produces a matrix
  z=outer(x,param,lfun)
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y=apply(z,2,sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,y,col="Blue",type="l",lwd=2,...)
  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y)))
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
