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

finite_difference <- function(theta, f, ..., eps=1e-7, Hessian=FALSE) {
  # finite differencing for the gradient or hessian
  # >>> inputs:
  # theta - vector of initial values for optimization parameters
  # f - objective function to minimize
  # ... - arguments supplied in f
  # eps - epsilon parameter for finite differencing
  # Hessian - logical condition to compute the Hessian matrix
  # <<< outputs:
  # fd - gradient vector
  # Hfd - hessian matrix
  
  n <- length(theta)
  fd <- matrix(0, n, 1) # initialize gradient vector
  Hfd <- matrix(0, n, n) # initialize hessian matrix
  f0 <- f(theta, ...)
  th0 <- theta
  for (i in 1:length(theta)) { ## loop over parameters
    th1 <- th0; th1[i] <- th1[i] + eps ## increase th0[i] by eps
    f1 <- f(th1, ...) ## compute resulting f
    fd[i] <- (f1 - f0)/eps ## approximate -dl/dth[i]
    # check if the gradient is supplied by the objective function
    if (!is.null(attr(f0, "gradient"))){
      g0 <- attr(f0, "gradient") # obtain the gradients
      g1 <- attr(f1, "gradient")
      Hfd[i,] <- (g1 - g0)/eps # finite differencing the gradient vectors
    }
  }
  # check if the gradient is not supplied
  if (is.null(attr(f0, "gradient"))){
    ic <- 1 # index count
    # loop through both dimensions of the hessian
    for (i in 1:length(theta)){
      for (j in ic: length(theta)){
        if (i == j){ 
          # approximate second order derivatives
          # f2xx(x,y) = (f(x+eps,y)-2f(x,y)+f(x-eps,y))/(eps**2)
          # f2yy(x,y) = (f(x,y+eps)-2f(x,y)+f(x,y-eps))/(eps**2)
          th1a <- th0; th1a[i] <- th1a[i] + eps
          th1b <- th0; th1b[i] <- th1b[i] - eps
          Hfd[i,j] <- (f(th1a) - 2 * f0 + f(th1b)) / eps**2
        }
        else{
          # approximate second order mixed derivatives
          # f2xy(x,y) = (f(x+eps,y+eps)-f(x+eps,y-eps)-f(x-eps,y+eps)+f(x-eps,y-eps))/(4*eps**2)
          th1a <- th0; th1a <- th1a + eps
          th1b <- th0; th1b[i] <- th1b[i] + eps; th1b[j] <- th1b[j] - eps
          th1c <- th0; th1c[i] <- th1c[i] - eps; th1c[j] <- th1c[j] + eps
          th1d <- th0; th1d <- th1d - eps
          Hfd[i,j] <- (f(th1a) - f(th1b) - f(th1c) + f(th1d)) / (4 * eps**2)
          Hfd[j,i] <- Hfd[i,j] # symmetric matrix entries
        }
      }
      ic <- ic + 1 # increase the index count by 1 to skip all computation of symmetric entries where i != j
    }
  }
  if (Hessian==TRUE) return(Hfd)
  else return(fd)
}

bfgs <- function(theta, f, ..., tol=1e-5, fscale=1, maxit=100){
  # An implementation of the BFGS quasi-Newton minimization method
  # >>> inputs:
  # theta - vector of initial values for optimization parameters
  # f - objective function to minimize
  # tol - convergence tolerance
  # fscale - estimate of magnitude of f at optimum
  # maxit - the maximum number of iterations before stopping
  # <<< outputs:
  # param - named list containing the final f, theta, iteration count, gradient and hessian
  
  obj <- f(theta, ...) # get the objective function of theta
  
  # stop the optimization if initial parameters lead to an infinite objective value
  if (is.infinite(obj[1])) stop("Objective value is infinite on initial parameters")
  
  # finite differencing for the hessian
  Hfd <- finite_difference(theta, f, ..., Hessian=TRUE)
  B <- chol2inv(chol(Hfd)) # obtain the inverse hessian
  
  for (i in 1:maxit) {  
    # obtain gradient by finite differencing if no gradient supplied
    if (is.null(attr(obj, "gradient"))) grad <- finite_difference(theta, f, ...) 
    else grad <- attr(obj, "gradient") # otherwise use the supplied gradient
    
    Delta <- - B %*% grad # calculate the step, Delta
    theta_new <- theta + Delta # updated theta values
    obj_new <- f(theta_new, ...) # update objective value
    
    # finite differencing for the gradient of f(theta_{k+1} + Delta)
    if (is.null(attr(obj_new, "gradient"))) grad_new <- finite_difference(theta_new, f, ...)
    else grad_new <- attr(obj_new, "gradient") # get the gradient from the updated objective function
    
    while (is.infinite(obj_new[1])) { # reduce step size if objective function is returning as infinite
      Delta <- Delta * (1/2) # reduce by bisection
      theta_new <- theta + Delta # update values
      obj_new <- f(theta_new, ...)
    }
    
    # issue warning if objective value increased
    if (obj_new[1] > obj[1]) warning("Current objective value has increased at iteration: ", i)
    
    # mitigate the effect of computer arithmetic limitation where obj_new has the same value as obj
    # but R returns it as being different, so condition uses isTRUE(all.equal()) rather than ==
    if (isTRUE(all.equal(obj_new[1],obj[1]))){
      invisible() # if this condition is satisfied do not alter step length
    }
    # ensure objective function decreases
    else{ 
      while (obj_new[1] > obj[1]) {
        Delta <- Delta * (1/2) # Reduce step size by bisection
        theta_new <- theta + Delta # Update theta, objective value and gradient
        obj_new <- f(theta_new, ...)
        if (is.null(attr(obj_new, "gradient"))) grad_new <- finite_difference(theta_new, f, ...)
        else grad_new <- attr(obj_new, "gradient")
        # ensure the second wolfe condition is satisfied, with c = 0.9
        while(t(grad_new) %*% Delta < 0.9 * t(grad) %*% Delta){
          Delta <- Delta * (1.1) # increase step size by by 0.1
          theta_new <- theta_new + Delta
          obj_new <- f(theta_new, ...)
          if (is.null(attr(obj_new, "gradient"))) grad_new <- finite_difference(theta_new, f, ...)
          else grad_new <- attr(obj_new, "gradient")
        }
      }
    }
   
    # Check if convergence condition is satisfied
    if (max(abs(grad)) < (abs(obj_new[1])+fscale)*tol){
      H <- finite_difference(theta, f, ..., Hessian = TRUE) # approximate hessian by finite differencing
      H <- 0.5 * (t(H) + H) # adjustment to asymmetric matrix
      param <- list(f=obj_new[1], theta=theta_new, iter=i, g=grad, H=H) # return names list containing outputs
      return(param)
    }  
    
    # Check if max iteration is reached
    if (i == maxit) {
      # Check if convergence condition is not satisfied
      if (max(abs(grad)) >= (abs(obj_new[1])+fscale)*tol){
        warning("Max iteration reached. No convergence.") # issue no convergence warning
      }
      # Force end when max iteration is reached
      H <- finite_difference(theta, f, ..., Hessian = TRUE)
      H <- 0.5 * (t(H) + H)
      param <- list(f=obj_new[1], theta=theta_new, iter=i, g=grad, H=H)
      return(param)
    }
    else{
      # compute s and y vector and p values
      s <- theta_new - theta
      y <- grad_new - grad
      p <- (1/(t(s) %*% y))[1]
      # BFGS Update
      I <- diag(length(theta_new))
      B_new <- (I - p * s %*% t(y)) %*% B %*% (I - p * y %*% t(s)) + p * s %*% t(s)
      # Update theta, inverse hessian and objective value
      theta <- theta_new
      B <- B_new
      obj <- obj_new
    }
  }
}
