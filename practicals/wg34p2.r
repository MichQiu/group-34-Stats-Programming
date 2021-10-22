# Group 34, Dickon Fell, Michael Qiu, Honglin Li
# https://github.com/MichQiu/group-34-Stats-Programming

# This code contains a function to simulate covid transmission amongst a population with various parameters. The function uses the SEIR (Susceptible-Exposed-Infectious-Recovered) framework
# to model the spread of the disease, by tracking the number of people in each group over a number of days.

# Below that is the code to produce two plots, the first of which compares daily new infections amongst three groups of the population after running one simultion, and the second of
# which shows the variability in the simulation by plotting the results of 10 replicate simulations.

# The idea of this code is to explore how daily new infection numbers from different samples of the population might fail to represent the whole, 
# and I have written some comments at the end with my conclusions on this matter.

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

# plotting vertical lines indicating days at which maxima are attained
y1 <- -10:floor(max(N_std)); y2 <- -10:floor(max(N0.1_std)); y3 <- -10:max(NR_std) # floor function avoids messy lists and sticks to integers
lines(rep(N_maxday, length(y1)), y1, col=c[1], lty=2)
lines(rep(N0.1_maxday, length(y2)), y2, col=c[2], lty=2)
lines(rep(NR_maxday, length(y3)), y3, col=c[3], lty=2)

x11() # do not replace current window with next plot




# running 10 replicate simulations to visualise the variability
# my idea here is to show the variability by plotting the lines of the maximum and mininimum new daily infections,
# and then colouring in the space between them.

SEIR <- vector(mode="list", length=10) # create empty lists to store simulation results
for (i in 1:10) { # run 10 simulations

        SEIR[[i]] <- model() # generate trajectories and store in the list

}

# the following mess is a result of my inability to find a way to check the maximum of of elements across various sublists in a vectorised way
# however, it does work
N_max <- N_min <- rep(0, 100) # initialise trajectory vectors of max and min new daily infections

for (i in 1:100) { # loop over days. At each day, find the max and min number of new infections amongst the 10 simulations

        N_max[i] <- max(SEIR[[1]]$N[i], SEIR[[2]]$N[i],SEIR[[3]]$N[i],SEIR[[4]]$N[i],SEIR[[5]]$N[i],SEIR[[6]]$N[i],SEIR[[7]]$N[i],SEIR[[8]]$N[i],SEIR[[9]]$N[i], SEIR[[10]]$N[i])
        N_min[i] <- min(SEIR[[1]]$N[i], SEIR[[2]]$N[i],SEIR[[3]]$N[i],SEIR[[4]]$N[i],SEIR[[5]]$N[i],SEIR[[6]]$N[i],SEIR[[7]]$N[i],SEIR[[8]]$N[i],SEIR[[9]]$N[i], SEIR[[10]]$N[i])

}

plot(1:100, N_min/1000, 'l', ylim=c(0,200), main="Daily New COVID Infections in 10 simulations", xlab="day", ylab="number of infections")
lines(1:100, N_max/1000) # plot min and max lines
polygon(c(1:100, 100:1), c(N_max/1000, rev(N_min/1000)), col=rgb(1,0,0,0.5)) # fill in space between them with colour

# repeat for the 10% smallest beta values and random 0.1% sample

N0.1_max <- N0.1_min <- rep(0, 100)

for (i in 1:100) {

        N0.1_max[i] <- max(SEIR[[1]]$N0.1[i], SEIR[[2]]$N0.1[i],SEIR[[3]]$N0.1[i],SEIR[[4]]$N0.1[i],SEIR[[5]]$N0.1[i],SEIR[[6]]$N0.1[i],SEIR[[7]]$N0.1[i],SEIR[[8]]$N0.1[i],SEIR[[9]]$N0.1[i],SEIR[[10]]$N0.1[i]) 
        N0.1_min[i] <- min(SEIR[[1]]$N0.1[i], SEIR[[2]]$N0.1[i],SEIR[[3]]$N0.1[i],SEIR[[4]]$N0.1[i],SEIR[[5]]$N0.1[i],SEIR[[6]]$N0.1[i],SEIR[[7]]$N0.1[i],SEIR[[8]]$N0.1[i],SEIR[[9]]$N0.1[i],SEIR[[10]]$N0.1[i])

}

lines(1:100, N0.1_min/100)
lines(1:100, N0.1_max/100)
polygon(c(1:100, 100:1), c(N0.1_max/100, rev(N0.1_min/100)), col=rgb(0,1,0,0.5))


NR_max <- NR_min <- rep(0, 100)

for (i in 1:100) {

        NR_max[i] <- max(SEIR[[1]]$NR[i], SEIR[[2]]$NR[i],SEIR[[3]]$NR[i],SEIR[[4]]$NR[i],SEIR[[5]]$NR[i],SEIR[[6]]$NR[i],SEIR[[7]]$NR[i],SEIR[[8]]$NR[i],SEIR[[9]]$NR[i], SEIR[[10]]$NR[i])
        NR_min[i] <- min(SEIR[[1]]$NR[i], SEIR[[2]]$NR[i],SEIR[[3]]$NR[i],SEIR[[4]]$NR[i],SEIR[[5]]$NR[i],SEIR[[6]]$NR[i],SEIR[[7]]$NR[i],SEIR[[8]]$NR[i],SEIR[[9]]$NR[i], SEIR[[10]]$NR[i])

}

lines(1:100, NR_min)
lines(1:100, NR_max)
polygon(c(1:100, 100:1), c(NR_max, rev(NR_min)), col=rgb(0,0,1,0.5))
legend("topleft", inset=.05, legend = c("Whole Pop. (/1000 ppl)","Lowest 10% of Beta (/100 ppl)", "Random 0.1% sample"), col = c, lty=1)

# We can see from these two plots that those with beta values in the lowest 10% have far lower daily new infection numbers than the whole population and a random sample from the population,
# even when taking into account the variability of the simulation. If indeed someone who downloads a covid symptom tracker app is more likely to restrict their contact with others
# (have a lower beta value) to avoid the disease, then we can see that using the daily new infections data from that group to represent the whole population would yield inaccurate results. 
# The random 0.1% sample of the population much more closely resembles the data for the whole population, so this appears to be a more representative way of sampling.



