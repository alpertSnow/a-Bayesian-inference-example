## params
X <- 0:50
N <- length(X)
Y <- X + rnorm(N, 0, 2)

## .bug file
library(R2WinBUGS)
model1 <- function(){
        # likelihood
        for (i in 1:N) {
                Y[i] ~ dnorm(mu[i], tau)
                mu[i] <- alpha * X[i] + beta
        }
        alpha ~ dnorm(0.0, 1.0E-4)
        beta ~ dnorm(0.0, 1.0E-4)
        sigma <- 1.0/sqrt(tau)
        tau ~ dgamma(1E-3, 1E-3)
}
path <- file.path(getwd(), 'model1.bug')
write.model(model1, path)

## run JAGS
library(R2jags)
data <- list('X', 'Y', 'N')
parameters <- c('alpha', 'beta', 'sigma')
inits <- NULL
jags.results <- jags(data, inits, parameters, 'model1.bug', n.chains = 3, n.iter = 5000, n.burnin = 1000, n.thin = 1)
print(jags.results)

## post-process
mcmc <- data.frame(jags.results$BUGSoutput$sims.list)

a.est <- jags.results$BUGSoutput$mean$alpha
b.est <- jags.results$BUGSoutput$mean$beta
plot(X,Y)
lines(X, a.est * X + b.est)

hist(mcmc$sigma)
