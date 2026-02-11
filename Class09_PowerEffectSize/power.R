install.packages("effectsize")

### libraries

  library("data.table")
  library("ggplot2")
  library("foreach")
  library("effectsize")
  
##############################
### Power analysis
##############################
  
  run_experiment <- function(n, effect, sd = 1) {
    control   <- rnorm(n, 0, sd)
    treatment <- rnorm(n, effect, sd)
    test <- t.test(treatment, control)
    
    data.table(
      pval = test$p.value,
      est  = mean(treatment) - mean(control), n=n, effect_size=effect, sd=sd)
    
  }
  
  n_sims <- 2000
  n <- 20
  true_effect <- 0.5
      
  results <- foreach(i = 1:n_sims, .combine = rbind) %do% {
    run_experiment(n, true_effect)
  }
   

  results[,list(power=mean(pval<.05)), list(n, effect_size, sd)]
  
### Your turn: What sample size would you need to obtain 90% power to reject the null at alpha<.05?
### Hint, use a nested foreach loop with the inner loop iterating across a vector of sample sizes
 
  
##############################
### Regression towards the mean
##############################
    
### Let's work with a model based on quantitative genetics
### Here, phenotypic variation is determined by genetics and environment: Vp=Vg+Ve
### Let's assume that any individual has a true "genetic" value that is modified by aspects of the environment that we can control and that we cannot control
  
  true_trait <- data.table(id=c(1:1000), true_value=rnorm(n_ind, mean = 0, sd = 1))

### Let's imagine that we measure an once, and then come back some time later to measure them again. 
### Let's assume that the trait we are measuring is behavioral, i.e. very pliable. Thinking about a "fixed" trait like eye color doesn't really make sense.
### In these measurements, we add environmental noise to the true value to get our measured value
  measurement_sd <- 1
  
  true_trait[, meas1 := true_value + rnorm(.N, 0, measurement_sd)]
  true_trait[, meas2 := true_value + rnorm(.N, 0, measurement_sd)]

### To motivate Regression to the Mean, let's imagine that we focused on the individuals with the biggest values after the 1st measurement.
### Are they going to have a large value on teh second measurement?
### The canonical example of regression to the mean is baseball batting averages. (how many times can you hit the ball; typically ~0.25 and >0.3 is good. That is the full extent of my sports knowledge, btw.)
### In the baseball example, if you take early season stats maybe you want to ask if a "top" hitter is likely to stay a top hitter.

# Select individuals with extreme first measurement
  threshold <- quantile(true_trait$meas1, 0.9)
  true_trait[, extreme := meas1 >= threshold]

# Compare means
  true_trait[extreme == TRUE,
             .(mean_meas1 = mean(meas1),
               mean_meas2 = mean(meas2))]

# Visualization
  ggplot(true_trait, aes(meas1, meas2)) +
    geom_point(alpha = 0.2) +
    geom_point(data = true_trait[extreme == TRUE],
               color = "red", alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(title = "Regression to the Mean",
         subtitle = "Extreme individuals become less extreme on re-measurement",
         x = "Measurement 1",
         y = "Measurement 2") +
    theme_bw()


####################
### Winner's curse
####################

### Distribution of all estimated effects
  ggplot(results, aes(est)) +
    geom_histogram(bins = 40, fill = "gray") +
    geom_vline(xintercept = true_effect, color = "red", linewidth = 1) +
    labs(title = "Estimated Effect Sizes",
         subtitle = "Across all experiments",
         x = "Estimated effect size")

### When the test is significant (p<.05), does the estimated effect size reflect the true value?
  results[,list(estimated_effect=mean(est), true_effect=true_effect), list(significant=I(pval<.05))]
  
  results[,sig:=pval<.05]
  
  ggplot(results, aes(est, group=sig, fill=sig)) +
    geom_histogram() +
    geom_vline(xintercept = true_effect, color = "black", linewidth = 1) +
    geom_vline(xintercept=mean(results[sig==T]$est), color="orange", linewidth=1) + 
    labs(title = "Winner's Curse",
         subtitle = "Significant results tend to overestimate effect size",
         x = "Estimated effect size") +
    theme_bw()
  
### Homework:
  
  
  
########################################
### Do replicate experiments disapoint?
########################################

  results[sig==T]
    
  replication_estimates <- foreach(i = 1:nrow(results[sig==T]), .combine = c) %do% {
    run_experiment(n, true_effect)$est
  }
  
  mean(results[sig==T]$est)
  mean(replication_estimates)

##################
#### Effect size
##################
  sampleSize <- 5
  dt <- data.table(trt=rep(c("A", "B"), each=sampleSize), 
                   x=c(rnorm(sampleSize, 0, 1), rnorm(sampleSize, .5, 1)))
  cohens_d(x~trt, data=dt)
  hedges_g(x~trt, data=dt)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  