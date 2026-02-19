install.packages("abc")
install.packages("bayesreg")
install.packages("causaldata")
install.packages("hexbin")
install.packages("viridis")
install.packages("rstanarm")

### libraries
  rm(list = ls())
  
  library(data.table)
  library(ggplot2)
  library(foreach)
  library(abc)
  library(MASS)
  library(bayesreg)
  library(causaldata)
  library(viridis)
  library(rstanarm)
  library(doMC)

  
############################################################
### let's use a dataset from the causaldata package
### We will look at the `restaurant_inspections` data
### This dataset shows the health inspection score for restaurants in Anchorage, Alaska
### For each restaurant, we also know the number of locations in the chain
### data from https://www.kaggle.com/datasets/loulouashley/inspection-score-restaurant-inspection via the causaldata package.
############################################################  
  dt_obs <- data.table(restaurant_inspections)
  str(dt_obs)
  summary(lm(inspection_score~log10(NumberofLocations), dt_obs))
  
  ggplot(data=dt_obs, aes(x=NumberofLocations, y=inspection_score)) + geom_point() + theme_bw()
  ggplot(data=dt_obs, aes(x=log10(NumberofLocations), y=inspection_score)) + geom_hex()+ theme_bw()
  ggplot(data=dt_obs, aes(x=log10(NumberofLocations), y=inspection_score)) + geom_hex() + scale_fill_viridis(trans = "log") + theme_bw()
  

###################################
# Bayesian Posterior from a package
###################################
### STAN is a popular choice for fancy implementations of MCMC
### https://khakieconomics.github.io/half_day_course/index.html for more. 
### https://stan-playground.flatironinstitute.org
  
  ### let's assume that we have no expectations about restaurant quality. 
    post_uniform <-
      stan_glm(
        inspection_score ~ log10(NumberofLocations),
        data = dt_obs,
        prior = NULL,
        prior_intercept=NULL,
        seed = 12345
      )
    post_uniform
  
  ### now let's assume that we're all adults and have been to a restaurant or two. 
    post_inform <-
      stan_glm(
        inspection_score ~ log10(NumberofLocations),
        data = dt_obs, family=gaussian(),
        prior = normal(-1,10),
        prior_intercept = normal(100, 10),
        seed = 12345
      )
    post_inform
    
    
  ### Let's visualize the priors and posterior estimates of the coeffieints
    post_uniform <- as.data.table(as.matrix(post_uniform))
    post_inform <- as.data.table(as.matrix(post_inform))
    
    post_uniform[,method:="stan_uniform"]
    post_inform[,method:="stan_inform"]

    post_stan <- rbind(post_uniform, post_inform)
    setnames(post_stan, c("(Intercept)", "log10(NumberofLocations)"), c("beta0", "beta1"))
    post_stan[,beta0:=as.numeric(beta0)]
    post_stan[,beta1:=as.numeric(beta1)]
    
    ggplot(data=post_stan, aes(beta0)) + geom_histogram() + facet_grid(~method)    
    ggplot(data=post_stan, aes(beta1)) + geom_histogram() + facet_grid(~method)    
    ggplot(data=post_stan, aes(beta1, group=method, color=method)) + geom_density()
    
    
##############
### ABC Setup
##############
    
### Why ABC? Sometimes we might want to build a model that we cannot easily describe with y~a+bx (or something equally well worked out)
### For instance, maybe you can construct an agent based simulation focusing on restaurant cleanliness?
### In population genetics, we use forward simulation and ABC all the time because you cannot easily write down likelihood models that include drift, mutation, migration, selection, and all the idiosincracies of life
### In ABC, you build a simulation. For pop gen maybe that is a 2-population model, and specify random parameters (e.g., migration rate). Then, you run your simulation, and generate summary statistics
### The goal is to take your observed data, derive summary statistics, and find parameter regime in your simulation that generates summary stats that best match your data
### Here, for simplicity, we will continue to work with a linear regression even though this is a silly example of ABC    
      
# First, we define a function that generates the prior distributions for the slope and intercept.

    rprior <- function() {
      c(beta0 = rnorm(1, 0, 100),
        beta1 = rnorm(1, 0, 100))
    }
    
# Next, we define a function that generates our summary statistics from any simulation
  
  summary_stats <- function(y, x){
    fit <- lm(y~x)
    c(beta0_hat = coef(fit)[1],
      beta1_hat = coef(fit)[2],
      sigma2_hat = summary(fit)$sigma^2)
  }
  
# Then we generate our observes summary stats
  S_obs <- summary_stats(y=dt_obs$inspection_score, x=log10(dt_obs$NumberofLocations))

###################
### rejection ABC
###################
### simplest form of ABC. Just keep the simulations that are the closest to the data
  
  nsim <- 15000
  registerDoMC(4)
  sim_results <- foreach(i = 1:nsim, .combine = rbind) %dopar% {
    
    priorDraw <- rprior()
    
    y_sim <- priorDraw["beta0"] +
             priorDraw["beta1"] * log10(dt_obs$NumberofLocations) +
             rnorm(length(dt_obs$inspection_score), mean=0, sd=sd(dt_obs$inspection_score)/2)
    
    S_sim <- summary_stats(x= log10(dt_obs$NumberofLocations), y=y_sim)
    
    dist <- sum((S_sim - S_obs)^2)
    
    data.table(
      beta0_prior=priorDraw[1], 
      beta1_prior=priorDraw[2],
      beta0 = S_sim[1],
      beta1 = S_sim[2],
      sigma2_hat = S_sim[3],
      dist = dist)
  }
  
  dt_sim <- as.data.table(sim_results)
  ggplot(data=dt_sim, aes(dist)) + geom_density()
  
### reject the 99% worst, accept 1% best simulations
  tol <- 0.01
  eps <- quantile(dt_sim$dist, tol)
  dt_sim[,accecpt:=dist<eps]
  table(dt_sim$accecpt)
  dt_sim[,method:="abc"]
  
########################
### Compare STAN vs ABC
########################
  dt_compare <- rbind(post_stan, dt_sim, fill=T)
  
  ggplot(data=dt_compare, aes(x=beta0, y=beta1, color=accecpt)) + geom_point() + 
    facet_grid(~method) +
    geom_point(data=dt_compare[accecpt==T]) + theme_bw() +
    ylim(-300, 300) + xlim(-500, 500)
  
  
median(dt_sim[accecpt==T]$beta0)
median(dt_sim[accecpt==T]$beta1)
sd(dt_sim[accecpt==T]$beta1)


