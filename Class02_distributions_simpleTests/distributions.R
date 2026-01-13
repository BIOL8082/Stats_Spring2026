### install packages
  install.packages("ggplot2")
  install.packages("foreach")
  install.packages("patchwork")
  install.packages("abd")
  
### libraries
  library(data.table)
  library(ggplot2)
  library(foreach)
  library(readxl)
  library(abd)
  library(patchwork)

### our toy-dataset
  ShrinkingSeals
  
  ### your turn: what are the dimensions and structure of this object?
    
### plotting the data. What are some noticable features of the data?
  ggplot(data=ShrinkingSeals, aes(length)) + geom_histogram()
  ggplot(data=ShrinkingSeals, aes(y=length, x=age)) + geom_point()
  
### moments
  mean(ShrinkingSeals$length) ### mean
  sd(ShrinkingSeals$length) ### standard deviation
  var(ShrinkingSeals$length) ### variance
  sqrt(var(ShrinkingSeals$length)) ### SD = sqrt(var)

### the standard error
  sd(ShrinkingSeals$length)/sqrt(length(ShrinkingSeals$length)) ### the standard error

### your turn: can you transform age in days into a new column of age in years, to match the publication?
  ShrinkingSeals <- as.data.table(ShrinkingSeals)
  ShrinkingSeals[,age_years:=age/365]
  ShrinkingSeals[,age_years_integer:=floor(age/365 - 0.5)]
  
  ggplot(data=ShrinkingSeals, aes(y=length, x=age, color=as.factor(age_years_integer))) + geom_point()

### your turn: calculate the mean, sd, variance, sample size, standard errors for each unique value of age, in years  
  ss.ag <- ShrinkingSeals[,list(mean=mean(length), 
                                sd=sd(length), 
                                var=var(length), 
                                se=sd(length)/sqrt(length(age)),
                                nSeals=length(age),
                                .N),
                          list(years=age_years_integer)]

### our turn. Let's make a plot. What do we notice?
  se.plot <- ggplot(data=ss.ag, aes(y=mean, x=years)) + 
    geom_line(color="black") +
    geom_point(size=4) +
    geom_linerange(aes(ymin=mean-2*se, ymax=mean+2*se)) +
    theme_bw() + 
    xlab("Age (years)") + ylab("Length (cm)") +
    ylim(c(100,145)) +
    ggtitle("Two Standard Errors")
  
  sd.plot <- ggplot(data=ss.ag, aes(y=mean, x=years)) + 
    geom_line(color="black") +
    geom_point(size=4) +
    geom_linerange(aes(ymin=mean-2*sd, ymax=mean+2*sd)) +
    theme_bw() + 
    xlab("Age (years)") + ylab("Length (cm)") +
    ylim(c(100,145)) +
    ggtitle("Two Standard Deviations")
  
  se.plot + sd.plot + plot_annotation(tag_levels="A")
  
### is there a statistically significant difference in the length between years 2 & 3?
  setkey(ShrinkingSeals, age_years_integer)
  ShrinkingSeals[J(c(2,3))]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
### how well do we estimate the moments at different sample sizes?
### we can make a random subsample using the `sample` function
sample(ShrinkingSeals$length, 20, replace=F)

### we can take the mean of a sub-sample
mean(sample(ShrinkingSeals$length, 20, replace=F))

### if we want to understand the full dynamics of how the moments change, we will need to perform many resamplings at different sample sizes
sampleSizes <- seq(from=2, to=dim(ShrinkingSeals)[1], by=10)

nSamples <- 20

moments.dt <- foreach(n=sampleSizes, .combine="rbind")%do%{
  foreach(samp=1:nSamples, .combine="rbind")%do%{
    tmp <- sample(ShrinkingSeals, n, replace=F)
    data.table(n=n, samp=samp, mean=mean(tmp$length), sd=sd(tmp$length), se=sd(tmp$length)/sqrt(n))
  }
}

ggplot(data=moments.dt, aes(y=mean, x=n)) + geom_point()
ggplot(data=moments.dt, aes(y=sd, x=n)) + geom_point()
ggplot(data=moments.dt, aes(y=se, x=n)) + geom_point()

### putting it all together
mean_plot <- ggplot(data=moments.dt, aes(y=mean, x=n)) + geom_point() + ggtitle("Mean")
sd_plot <- ggplot(data=moments.dt, aes(y=sd, x=n)) + geom_point() + ggtitle("SD")
se_plot <- ggplot(data=moments.dt, aes(y=se, x=n)) + geom_point() + ggtitle("SE")

mean_plot + sd_plot + se_plot + plot_annotation(tag_levels="A")







