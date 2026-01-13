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

### your turn: calculate the mean, sd, variance, sample size, standard errors for each unique value of age, in years  
 
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
  
### is there a statistically significant difference in the length between years 2 & 3? Let's use a t-test
  setkey(ShrinkingSeals, age_years_integer)
  ?t.test
  
### your turn: what two parameters do we need for the t-test, and how to we extract them from the data?
   
### 

  
  
  
  
  
  
  