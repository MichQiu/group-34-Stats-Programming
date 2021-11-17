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
  # >>> inputs:
  # theta - vector of initial values for optimization parameters
  # f - objective function to minimize
  # tol - convergence tolerance
  # fsacle - estimate of magnitude of f at optimum
  # maxit - the maximum number of iterations before stopping
  
  for (i in 1:maxit) {
    
    # finite differencing for the gradient
    fd <- theta
    f0 <- f(theta)
    eps <- 1e-7 ## finite difference interval
    th0 <- theta
    for (i in 1:length(theta)) { ## loop over parameters
      th1 <- th0; th1[i] <- th1[i] + eps ## increase th0[i] by eps
      f1 <- f(th1) ## compute resulting nll
      fd[i] <- (f1 - f0)/eps ## approximate -dl/dth[i]
    }
    
    # finite differencing for the hessian
    f0 <- f(theta) ## gran of f at th0
    eps <- 1e-7 ## finite difference interval
    Hfd <- matrix(0,2,2) ## finite diference Hessian
    th0 <- theta
    for (i in 1:length(theta)) { ## loop over parameters
      th1 <- th0; th1[i] <- th1[i] + eps ## increase th0[i] by eps
      f1 <- f(th1) ## compute resulting f
      Hfd[i,] <- (f1 - f0)/eps ## approximate second derivs
    }
    
    
  
}







