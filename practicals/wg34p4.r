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

finite_difference <- function(f, theta, eps=1e-7, Hessian=FALSE) {
  # finite differencing for the gradient
  
  n <- length(theta)
  if (Hessian==TRUE) {
    fd <- matrix(0, n, n)
  } else {
    fd <- matrix(0, n, 1)
  }
  f0 <- f(theta)
  th0 <- theta
  for (i in 1:length(theta)) { ## loop over parameters
    th1 <- th0; th1[i] <- th1[i] + eps ## increase th0[i] by eps
    f1 <- f(th1) ## compute resulting f
    fd[i,] <- (f1 - f0)/eps ## approximate -dl/dth[i]
  }
  
  return(fd)
  
}

bfgs <- function(theta, f, ..., tol=1e-5, fscale=1, maxit=100){
  # An implementation of the BFGS quasi-Newton minimization method
  # >>> inputs:
  # theta - vector of initial values for optimization parameters
  # f - objective function to minimize
  # tol - convergence tolerance
  # fsacle - estimate of magnitude of f at optimum
  # maxit - the maximum number of iterations before stopping
  
  # finite differencing for the gradient
  fd <- finite_difference(f, theta)
  
  # finite differencing for the hessian
  Hfd <- finite_difference(f, theta, Hessian=TRUE)
  
  qrx <- qr(Hfd) # get QR decomposition of hessian
  # we have R^TRB = I where I is the identity matrix
  R <- qr.R(qrh)
  RHS <- diag(length(theta))
  RB <- forwardsolve(t(R), RHS) # forward solve to find RV (since R^T is lower triangular)
  B <- backsolve(R, RB) # backsolve to find B (R is upper triangular)
  
  Delta <- - B %*% fd # calculate the step, Delta
  
  theta_new <- theta + Delta # updated theta values
  
  # finite differencing for the gradient of f(theta_{k+1} + Delta)
  fd_new <- theta_new
  f0 <- f(theta_new)
  eps <- 1e-7 ## finite difference interval
  th0 <- theta_new
  for (i in 1:length(theta)) { ## loop over parameters
    th1 <- th0; th1[i] <- th1[i] + eps ## increase th0[i] by eps
    f1 <- f(th1) ## compute resulting nll
    fd_new[i] <- (f1 - f0)/eps ## approximate -dl/dth[i]
  }
  
  while (is.infinite(f(theta_new))) { # reduce stepsize if objective function is returning as infinite
    Delta <- Delta * (2 / 3)
  }
  
  # ensure objective function decreases and that the second wolfe condition is satisfied, with c = 0.9
  while ((t(fd_new) %*% Delta < 0.9 * t(fd) %*% Delta) & (f(theta_new) > f(theta))) {
    Delta <- Delta - Delta/2
  }
  
  for (i in 1:maxit) {
    
    
  
  }
}







