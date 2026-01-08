### libraries
  library(data.table)
  library(ggplot2)
  library(foreach)
  library(readxl)
  library(abd)
  library(patchwork)

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










#### the Seal Preservation Association is obsessed with knowing how big the average seal, and how much variance in seal size. Their last survey was a few years ago, and they want to conduct a new expidition in 2026.
### they reached out to you for help.
### Based on your preliminary data from the last survey of seals (the ShrinkingSeals), how many seals do they need to sample to


### t.test
  t.test(ShrinkingSeals$length~I(ShrinkingSeals$age>median(ShrinkingSeals$age))
