### libraries
  library(data.table)
  library(ggplot2)
  library(foreach)
  library(readxl)
  library(abd)

### types of objects
  data.frames
  data.tables
  lists
  vectors
  functions

### importing data

### looking at data objects
  dim
  str
  summary

### moments
  mean(ShrinkingSeals$length) ### mean
  sd(ShrinkingSeals$length) ### standard deviation
  var(ShrinkingSeals$length) ### variance
  sqrt(var(ShrinkingSeals$length)) ### SD = sqrt(var)

  ### your turn: using the formula for the variance
    sum((ShrinkingSeals$length - mean(ShrinkingSeals$length))^2)/(length(ShrinkingSeals$length)-1) ### manual calculation of variance

  ### the standard error
    sd(ShrinkingSeals$length)/sqrt(length(ShrinkingSeals$length))

### how well do we estimate the moments at different sample sizes?
  ### we can make a random subsample using the `sample` function
    sample(ShrinkingSeals$length, 20, replace=F)

  ### we can take the mean of a sub-sample
    mean(sample(ShrinkingSeals$length, 20, replace=F))

  ### if we want to understand the full dynamics of how the moments change, we will need to perform many resamplings at different sample sizes
    sampleSizes <- seq(from=2, to=dim(ShrinkingSeals)[1], by=100)
    nSamples <- 100

    moments.dt <- foreach(n=sampleSizes, .combine="rbind")%do%{
      foreach(samp=1:nSamples, .combine="rbind")%do%{
        tmp <- sample(ShrinkingSeals, n, replace=F)
        data.table(n=n, samp=samp, mean=mean(tmp$length), sd=sd(tmp$length))
      }
    }

    ggplot(data=)









### t.test
  t.test(ShrinkingSeals$length~I(ShrinkingSeals$age>median(ShrinkingSeals$age))
