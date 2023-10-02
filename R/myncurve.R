#' @title Normal Distribution Probability Plot
#'
#' @param a Upper limit of probability
#' @param mu Mean
#' @param sigma Standard Deviation
#' @param col Color of Shaded Probability
#'
#' @return A plot with the shaded area up to a and P(Y<=a)
#' @export
#'
#' @examples
#' myncurve(a=6, mu=10, sigma=4, col="red")
myncurve = function(a, mu, sigma, col="red"){
  #Plot the curve
  curve(dnorm(x, mean=mu, sd=sigma), xlim=c((mu - 3*sigma), (mu + 3*sigma)))
  #Add the shaded area
  x_curve = seq((mu - 3*sigma), a, length=5000)
  y_curve = dnorm(x_curve, mean=mu, sd=sigma)
  polygon(c((mu - 3*sigma),x_curve,a),c(0,y_curve,0), col=col)

  #Calculate and return probability under curve
  probability = pnorm(a, mean=mu, sd=sigma)
  return(probability)
}
