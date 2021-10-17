## Write a bit here about the code in general and explain SEIR

model <- function(n=5500000, E0=10, t=100, gamma=1/3, delta=1/5, lambda=0.4/n) {
# disease transmission simulation model
# n = population size; E0 = initially exposed; t = number of days;
# gamma = daily prob E -> I; delta = daily prob I -> R;
# lambda = overall viral infectivity parameter

x <- rep(0, n) # vector representing population
x[1:E0] <- 1 # expose an initial number of people

# vectors of number of people in each state each day, and:
# N, the number of new infections per day, N0.1, " " in the lowest 10% of beta values
S <- E <- I <- R <- N <- N0.1 <- rep(0, t) 

S[1] <- n-E0; E[1] <- E0
beta <- rlnorm(n, 0, 0.5); beta <- beta/mean(beta) # relative contact rates of population
beta0.1 <- quantile(beta, 0.1)

for (i in 2:t) { # loop over days
        # Add comment about using uniform random deviates to represent probabilities here
        u <- runif(n) # uniform random deviates

        # S -> E with prob lambda*beta[j]*sum(beta[i])
        se <- x==0 & u<lambda*beta*sum(beta[x==2])
        x[se] <- 1
        # this can also be use to find the number of new infections per day
        N[i] <- sum(se) 
        # and those in the lowest 10% of beta values
        N0.1[i] <- sum(se & beta<beta0.1) 

        # E -> I with prob gamma = 1/3
        x[x==1 & u<gamma] <- 2
        # I -> R with prob delta = 1/5
        x[x==2 & u<delta] <- 3

        S[i] <- sum(x==0); E[i] <- sum(x==1)
        I[i] <- sum(x==2); R[i] <- sum(x==3)

}

list(S=S, E=E, I=I, R=R, beta=beta)

}
