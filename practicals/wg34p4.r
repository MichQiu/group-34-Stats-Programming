# Group 34, Dickon Fell, Michael Qiu, Honglin Li
# https://github.com/MichQiu/group-34-Stats-Programming

rb <- function(theta,getg=FALSE,k=10) {
  ## Rosenbrock objective function, suitable for use by ’bfgs’
  z <- theta[1]; x <- theta[2]
  f <- k*(z-x^2)^2 + (1-x)^2 + 1
  if (getg) {
    attr(f,"gradient") <- c(2*k*(z-x^2),
                            -4*k*x*(z-x^2) -2*(1-x))
  }
  f
} ## rb

bfgs <- function(theta, f, ..., tol=1e-5, fscale=1, maxit=100){
  # An implementation of the BFGS quasi-Newton minimization method
  # Inputs:
  # theta - vector of initial values for optimization parameters
  # f - objective function to minimize
  # tol - convergence tolerance
  # fsacle - estimate of magnitude of f at optimum
  # maxit - the maximum number of iterations before stopping
  
  
  
}