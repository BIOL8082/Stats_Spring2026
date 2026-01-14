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
  ShrinkingSeals[,age_years:=age/365 + 0.5]
  ShrinkingSeals[,age_years_integer:=floor((age/365) + 0.5)]
  setkey(ShrinkingSeals, age_years_integer)
  
  ggplot(data=ShrinkingSeals, aes(y=length, x=age, color=as.factor(age_years_integer))) + geom_point()
  ggplot(data=ShrinkingSeals, aes(y=length, x=age, color=as.factor(age_years_integer))) + geom_boxplot()
  
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
  
### is there a statistically significant difference in the length between years 2 & 3? Let's use a t-test
  setkey(ShrinkingSeals, age_years_integer)
  ?t.test
  
### your turn: what two parameters do we need for the t-test, and how to we extract them from the data?
    t.test(x=ShrinkingSeals[J(3)]$length, 
           y=ShrinkingSeals[J(4)]$length)
  
   str(t.test(x=ShrinkingSeals[J(3)]$length, 
              y=ShrinkingSeals[J(4)]$length))
    
    
### Let's image that we have no ruler to measure our seals, and instead classify them as big (>125cm) or small (<=125cm). 
  ShrinkingSeals[,size_binary:=ifelse(length>125, "big", "small")]
  
### Let's also pretend that we only know if they are young (<10 year) or old (>=10 years).
### your turn: make the binary age column
  ShrinkingSeals[,age_binary:=ifelse(age_years_integer<10, "young", "old")]
  
  
### tabulation
  ### one and two way tables
    table(ShrinkingSeals$size_binary)  ### makes counts of unique values
    table(ShrinkingSeals$age_binary)
    prop.table(table(ShrinkingSeals$age_binary)) ### divides by the sum of the table
    
    table(ShrinkingSeals$size_binary, ShrinkingSeals$age_binary)  ###two-way tables
  
  ### annoyingly, the table is transposed and "your PI" is a real stickler for the ordering of axes on the table: small->big top to bottom; young->old left to right.
    ShrinkingSeals[,size_binary:=factor(size_binary, levels=c("small", "big"))]
    ShrinkingSeals[,age_binary:=factor(age_binary, levels=c("young", "old"))]
    
  ### that's better. What do you notice?
    table(ShrinkingSeals$size_binary, ShrinkingSeals$age_binary)
  
  ### formal test using Chisq
    tab <-  table(ShrinkingSeals$size_binary, ShrinkingSeals$age_binary)
    tab
    chisq.test(tab)
  

  ### where is there excess or deficit
    chisq_enrichment <- function(x) {
      if (!all(dim(x) == c(2, 2))) {
        stop("Input must be a 2x2 matrix")
      }
      
      # Expected counts from chi-square
      expected <- outer(rowSums(x), colSums(x)) / sum(x)
      
      # Enrichment as (observed - expected)/expected
      enrichment <- (x - expected)/expected
      
      return(list(
        observed = x,
        expected = expected,
        enrichment = enrichment
      ))
    }
    chisq_enrichment(tab)$enrichment
    
    
    
    
  ### hw  
  ### residuals 
    
    ### Seals tend to give birth around July 1. The length and weight measurements that we used are of pregnant females, and their size (including length) changes postpartum. 
    ### Are seals larger, after controlling for their age in years, in June (Julian day ~150-180) compared to August (~215-245)? Use a t-test to report your results.
    ### submit a box plot comparing the sizes (controlling for age in years) between these two time periods, and the output of a t-test comparing relative size at these two time points
    ### hint, you will need to to merge the raw data "ShrinkingSeals" with the summary data "ss.ag"
    
    ss2 <- merge(ShrinkingSeals, ss.ag, by.x="age_years_integer", by.y="years")
    ss2[,resid:=length-mean]
    ss2[,jday:=(age_years-age_years_integer)*365]
    ggplot(data=ss2, aes(x=jday, y=resid)) + geom_point()
    
    t.test(
      ss2[jday>=150 & jday<180]$resid,
      ss2[jday>=215 & jday<245]$resid)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
### old
    
    x/N * y/N = (xy/N^2)*N
  
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







