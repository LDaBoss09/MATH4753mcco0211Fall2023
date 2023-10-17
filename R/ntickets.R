#' @title N-Tickets
#'
#' @param N Total number of seats on the flight
#' @param gamma Probability of overbooking
#' @param p Probability that a person shows up
#'
#' @return A named list containing all parameters and two plots displaying the optimal number of tickets (n) to sell for a discrete distribution and continuous approximation
#' @export
#' @importFrom stats qbinom
#' @importFrom stats uniroot
#' @importFrom stats pbinom
#' @importFrom graphics abline
#' @importFrom graphics title
#'
#' @examples
#' ntickets(N=200, gamma=0.02, p=0.95)
ntickets = function(N,gamma,p){
  #Calculate number of tickets with discrete binomial
  x = N + 20 #Determine Range of possible n
  z = 21 #Iterations
  n_set = qbinom((1-gamma), (N:x), p) #Create vector n_set with possible nd
  for (y in 1:z){
    if (n_set[y] == N){
      nd = N + y #Filter nd from vector n
    }
  }

  #Calculate number of tickets with normal approximation
  Objective_c = function(x){
    1 - gamma - pnorm((N+0.5), (x * p), (x * p * (1-p))^0.5) #objective continuous     function
  }
  nc_1 = uniroot(Objective_c, lower = N, upper = (N + 20)) #Calculate the 0
  nc = nc_1[1] #Extract root from output


  #Graph discrete values
  n = (N:(N+20))
  Objective_d = 1 - gamma - pbinom(N,n,p) #objective discrete function
  plot(x=n, y=Objective_d, type = "b")
  title(main=paste("Objective vs n to find optimal tickets sold (",nd,") gamma=",gamma,"N=",N, "discrete", sep=" "), cex.main=0.9) #add title
  abline(v = nd, col='red', lwd=1.5) #add vertical line
  abline(h = 0, col='red', lwd=1.5) #add horizontal line

  #Graph continuous values
  Objective_c_lim = 1 - gamma - pnorm((N + 0.5), (n * p), (n * p * (1 - p))^0.5)
  plot(x=n, y=Objective_c_lim, type = "l")
  title(main=paste("Objective vs n to find optimal tickets sold (",nc,") gamma=",gamma,"N=",N, "continuous", sep=" "), cex.main=0.9) #add title
  abline(v = nc, col='red', lwd=1.5) #add vertical line
  abline(h = 0, col='red', lwd=1.5) #add horizontal line

  #Named list
  nlist = c(nd, nc, N, p, gamma)
  names(nlist) <- c("Discrete n (nd)", "Continuous Approximation n (nc)", "Total Seats (N)", "Probability of showing up (p)", "Probability of overbooking (gamma)")
  return(nlist)
}
