library(deSolve)

## Parameters
gam <- 1/10  # recovery rate
N <- 1  # population size
r0 <- 10     # basic reproduction number
mu <- 1/(50*365)  # death rate
bet <- r0*(gam+mu)
k <- 10 # imitation rate
w <-5000


## Initial conditions
state <- c(S=N-0.05, I=0.0001, x=0.95)  # Initial proportions

## Function defining the differential equations
vaccination_game <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- mu*(1-x) - bet*S*I - mu*S
    dI <- bet*S*I - gam*I - mu*I
    dx <- k*x*(1-x)*(-1 + w*I)
    return(list(c(dS, dI, dx)))
  })
}

## Parameters list
parameters <- list(gam = gam, bet = bet, mu = mu, w=w)

## Time points for solving the equations
times <- seq(0, 365*10, by = 1)

## Solve the differential equations
out <- as.data.frame(ode(y = state, times = times, func = vaccination_game, parms = parameters))

## Plotting
library(ggplot2)
plot(S ~ time, out, type = "l", col = "blue", ylim = c(0, N))
lines(I ~ time, out, col = "red")
lines(x ~ time, out, col = "green4")
legend("topright", c("S", "I", "x"), lwd = 1, col = c("blue", "red", "green4"), cex = 0.75)

  plot(I ~ S, out, type = "l", xlim = c(0, N), ylim = c(0, 5000))

