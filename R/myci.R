#' @title My Confidence Interval Function
#'
#' @param x The data for the function to find the confidence interval
#' @param conf.level The level of confidence set for the desired interval
#'
#' @return A confidence interval corresponding to the confidence level set in the parameters, 0.95 by default.
#' @export
#' @importFrom stats qt
#' @importFrom stats sd
#'
#' @examples
#' myci(x=c(0,1,3,2,4,5,1,3,12,3,4), conf.level=0.8)
myci = function(x, conf.level = 0.95){
  t = qt(1 - ((1-conf.level)/2), (length(x)-1)) #generate t value for conf.level
  mp = c(-1,1) #minus plus substitute
  interval = mean(x) + mp*t*(sd(x)/sqrt(length(x))) #Generate interval
  print(paste('The following is a', conf.level, 'Confidence Interval for the True Mean of the Data X', sep=' '))
  return(interval)
}
