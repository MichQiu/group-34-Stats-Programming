# Write a bit here about the code in general and explain SEIR

model <- function(n=5500000, E0=10, t=100, gamma=1/3, delta=1/5, lambda=0.4/n) {
# covid transmission simulation model
# n = population size; E0 = initially exposed; t = number of days;
# gamma = daily prob E -> I; delta = daily prob I -> R;
# lambda = overall viral infectivity parameter

x <- rep(0, n) # vector representing population
x[1:E0] <- 1 # expose an initial number of people

# vectors of number of people in each state each day, and:
# N, the number of new infections per day,
# N0.1, " " in the lowest 10% of beta values
# NR, " " in a random 0.1% sample of the population
S <- E <- I <- R <- N <- N0.1 <- NR <- rep(0, t) 

S[1] <- n-E0; E[1] <- E0 # set initial values to the correct numbers in the S/E vectors
beta <- rlnorm(n, 0, 0.5); beta <- beta/mean(beta) # relative contact rates of population

beta0.1 <- quantile(beta, 0.1) # find 10% quantile of beta values 
SAMPLE <- sample(n, size=n/1000, replace=FALSE) # random 0.1% sample

for (i in 2:t) { # loop over days

        # generating u from the [0,1] uniform distribution to represent
        # the probability, for example, of delta
        u <- runif(n) 

        # I -> R with prob delta = 1/5
        x[x==2 & u<delta] <- 3

        # E -> I with prob gamma = 1/3
        x[x==1 & u<gamma] <- 2

        # S -> E with prob lambda*beta[j]*sum(beta[i])
        se <- x==0 & u<lambda*beta*sum(beta[x==2])
        x[se] <- 1
        # this can also be use to find the number of new infections per day
        N[i] <- sum(se) 
        # and those in the lowest 10% of beta values
        N0.1[i] <- sum(se & beta<beta0.1) 
        # new daily infections for random 0.1% sample
        NR[i] <- sum(se[SAMPLE])

        S[i] <- sum(x==0); E[i] <- sum(x==1)
        I[i] <- sum(x==2); R[i] <- sum(x==3) # update SEIR trajectories

}

return(list(S=S, E=E, I=I, R=R, beta=beta, N=N, N0.1=N0.1, NR=NR))

}

SEIR <- model() # generate trajectories


# in the following plots I standardised the trajectories by scaling down to the smallest
# size of population we examined - the random 0.1% sample. This means dividing the daily
# new infection numbers by 1000, in the case of the whole population, and 100 in the case of
# the lowest 10% of beta values.

N_std <- SEIR$N / 1000
N0.1_std <- SEIR$N0.1 / 100
NR_std <- SEIR$NR

# days upon which trajectories reach their maximum
N_maxday <- which(N_std == max(N_std))
N0.1_maxday <- which(N0.1_std == max(N0.1_std))
NR_maxday <- which(NR_std == max(NR_std))

c <- rainbow(3) # colours for lines

# plot daily new infections for whole pop., title, axis labels and y-limit
plot(1:100, N_std, 'l', col=c[1], main="Daily New COVID Infections", xlab="day", ylab="number of infections", ylim=c(0,185)) 
lines(1:100, N0.1_std, col=c[2])
lines(1:100, NR_std, col=c[3]) # add other two daily infections data to the same plot

legend("topleft", inset=.05, legend = c("Whole Pop. (/1000 ppl)","Lowest 10% of Beta (/100 ppl)", "Random 0.1% sample"), col = c, lty=1) 

# vertical lines indicating days at which maxima are attained
y1 <- -10:floor(max(N_std)); y2 <- -10:floor(max(N0.1_std)); y3 <- -10:max(NR_std)
lines(rep(N_maxday, length(y1)), y1, col=c[1], lty=2)
lines(rep(N0.1_maxday, length(y1)), y2, col=c[1], lty=2)
lines(rep(NR_maxday, length(y1)), y3, col=c[1], lty=2)


# running 10 replicate simulations to visualise the variability

SEIR <- vector(mode="list", length=10) # create empty list to store simulation results
N <- vector(mode="list", length=10)
for (i in 1:10) { # run 10 simulations

        SEIR[[i]] <- model() # generate trajectories and store in the list
        N[[i]] <- SEIR[[i]]$N

}

N_max <- N_min <- rep(0, 100)

for (i in 1:100) {

        N_max[i] <- max(SEIR[[1]]$N[i], SEIR[[2]]$N[i], ... , SEIR[[10]]$N[i])
        N_min[i] <- min(SEIR[[1]]$N[i], ... , SEIR[[10]]$N[i])

}







