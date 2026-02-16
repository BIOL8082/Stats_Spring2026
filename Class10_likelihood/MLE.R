### libraries
  library(data.table)
  library(foreach)
  library(ggplot2)

#################
# True parameter
#################

  p_true <- 0.6
  n <- 50

########################
### Simulate one dataset
########################

  k <- rbinom(1, size = n, prob = p_true)

  cat("\nObserved data:\n")
  cat("Heads =", k, "out of", n, "flips\n")

############################
### Log-likelihood function
############################

### we are estimating one parameter (p) with k and n.
### k is the number of "heads" and n is the number of flips

  loglik <- function(p, k, n) {
    if (p <= 0 || p >= 1) return(-Inf)
    #k * log(p) + (n - k) * log(1 - p)
    dbinom(x=k, size=n, prob=p, log=T)
  }
  loglik(p=.5, k=32, n=50)


###############################################
# dbinom(): Probability of EXACTLY k successes
###############################################

  dt <- data.table(k = 0:n)
  dt[, prob := dbinom(k, size = n, prob = p_true)]

  cat("\nProbability of exactly 6 heads:\n")
  print(dbinom(6, size = n, prob = p_true))

  p1 <- ggplot(dt, aes(x = k, y = prob)) +
    geom_col() +
    labs(title = "dbinom(): Probability of Exactly k Heads",
         x = "Number of Heads (k)",
         y = "P(X = k)") +
    theme_minimal()

  print(p1)

########
### MLE
########
### for binomial distribution, the maximum liklihood estimate of the "true" probability of success is k/n

  p_hat <- k / n
  cat("\nMLE (p_hat) =", p_hat, "\n")

##########################
### Plot likelihood curve
##########################

  grid <- data.table(p = seq(0.001, 0.999, length.out = 1000))
  grid[,ll:=foreach(prob=grid$p, .combine="rbind")%do%loglik(p=prob, k=k, n=n)]
  grid

  p1 <- ggplot(data=grid, aes(x = p, y = ll)) +
    geom_line() +
    geom_vline(xintercept = p_hat, linetype = "dashed") +
    geom_vline(xintercept = p_true, linetype = "solid", color="red") +
    geom_point(data=grid[which.max(ll)], size=4) +
    labs(title = "Binomial Log-Likelihood",
         x = "p",
         y = "Log-Likelihood") +
    theme_bw()

    p1

############################
### Numerical optimization
############################

## par = initial parameter estimate
## fn = function to optimize
## control = this tells the function to find the max, not the min.
## he
  optim.out <- optim(fn=loglik, par=.5, lower=0, upper=1,
        k=k, n=n,
        control = list(fnscale = -1),
        method="Brent",
        hessian=T)
  optim.out

  p1 + geom_point(x=optim.out$par, y=optim.out$value, size=1, color="yellow")

##########################################
# standard errors from inverse Hessian
##########################################
### Hessian describes the curvature fo the liklihood surface.
### In other words, it is the second derivative
### For a single parameter estimate (like we've done here) it is a single value
### For multiple parameters, it is a matrix that can be turned into a variance-covariance matrix

  var_hessian <- -1 / optim.out$hessian
  se_hessian <- sqrt(var_hessian)

  cat("\nSE from inverse Hessian =", se_hessian, "\n")

######################################
### Compare to known binomial variance
######################################

  var_true_formula <- p_hat * (1 - p_hat) / n
  se_formula <- sqrt(var_true_formula)

  cat("\nSE from binomial formula =", se_formula, "\n")

#################
### Wald 95% CI
#################

  z <- 1.96
  ci_lower <- p_hat - z * se_hessian
  ci_upper <- p_hat + z * se_hessian

  cat("\n95% Wald CI:\n")
  print(c(ci_lower, ci_upper))


##########################
### likelihood ratio test
##########################
### LRTs compare "nested" models. Here, nested means that one model has a proper subset of the estimated parameters from a more complex model
### In our binomial example we are estimating one parameter, "p"
### If we have a null hypothesis, e.g p=0.5 we can test if our model that allows p to vary (and thus be estimated) is a better fit than a model where we fix p

  loglik(p=.5, n=n, k=k) ### this is the log-likelihood that p=0.5
  optim.out$value ### this is the maximum log-likelihood from our model where p was estimated to be 0.64

  ### LRT: for large sample size, 2*(ll_full - ll_reduced) follows a Chisq distribution with df=nParameters that are different
  Chisq_value <- 2*(optim.out$value - loglik(p=.5, n=n, k=k))
  Chisq_value
  pchisq(Chisq_value, 1, lower.tail=F)

  ### this is similar to the exact binomial test, and the two shoudl coverge as sample size increases.
  binom.test(x=k, n=n, p=.5)


#########################
### linear regression MLE
#########################

    n  <- 50
    x <- runif(n, 1, 10)

    beta0_true <- 2
    beta1_true <- 0.8
    sigma_true <- 1

    y <- beta0_true +
                beta1_true * x +
                rnorm(n, 0, sigma_true)

    dat <- data.table(y=y, x=x)

############################################################
### Ordinary Least Squares (OLS)
############################################################

    fit_lm <- lm(y ~ x, data = dat)
    summary(fit_lm)

############################################################
# maximum Likelihood Estimation (assuming Normal errors)
############################################################

  neg_loglik <- function(par, x, y) {
    beta0 <- par[1]
    beta1 <- par[2]
    sigma <- par[3]

    # Constrain sigma to be positive
    if (sigma <= 0) return(Inf)

    mu <- beta0 + beta1 * x

    -sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  }

  fit_mle <- optim(
    par = c(0, 0, 1),
    fn  = neg_loglik,
    x   = dat$x,
    y   = dat$y
  )

  cat("\nMLE estimates (Normal errors):\n")
  print(fit_mle$par)

  cat("\OLS estimates (Normal errors):\n")
  coef(fit_lm)
