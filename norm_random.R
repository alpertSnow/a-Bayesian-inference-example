## params
N <- 100
set.seed(123)
x <- rnorm(N, 1, 2)

## .bug file
library(R2WinBUGS)
model3 <- function(){
        # likelihood
        for (i in 1:N) {
                x[i] ~ dnorm(mu, tau)
        }
        mu ~ dnorm(0.0, 1.0E-4)
#        tau <- 0.01
        sd <- 1.0/sqrt(tau)
        y ~ dnorm(mu, tau)
        tau ~ dgamma(1E-3, 1E-3)
}
path <- file.path(getwd(), 'model3.bug')
write.model(model3, path)

## run JAGS
library(R2jags)
data <- list('x', 'N')
parameters <- c('mu', 'sd', 'y')
inits <- NULL
jags.results <- jags(data, inits, parameters, 'model3.bug', n.chains = 3, n.iter = 5000, n.burnin = 1000, n.thin = 1)
print(jags.results)

## post-process
library(coda)
jags.coda <- as.mcmc(jags.results)
densplot(jags.coda)
