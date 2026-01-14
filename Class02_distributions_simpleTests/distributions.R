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

### confidence intervals
  c(mean(ShrinkingSeals$length) - 1.96*(sd(ShrinkingSeals$length)/sqrt(length(ShrinkingSeals$length))),
    mean(ShrinkingSeals$length) + 1.96*(sd(ShrinkingSeals$length)/sqrt(length(ShrinkingSeals$length))))
  
### your turn: can you transform age in days into a new column of age in years, to match the publication?
 
  
### plots
  ggplot(data=ShrinkingSeals, aes(y=length, x=age, color=as.factor(age_years_integer))) + geom_point()
  ggplot(data=ShrinkingSeals, aes(y=length, x=age, color=as.factor(age_years_integer))) + geom_boxplot()
  
### your turn: calculate the mean, sd, variance, sample size, standard errors for each unique value of age, in years  

### our turn. Let's make a plot. What do we notice?
  se.plot <- ggplot(data=ss.ag, aes(y=mean, x=years)) + 
    geom_line(color="black") +
    geom_point(size=4) +
    geom_linerange(aes(ymin=mean-1.96*se, ymax=mean+1.96*se)) +
    theme_bw() + 
    xlab("Age (years)") + ylab("Length (cm)") +
    ylim(c(100,145)) +
    ggtitle("95% Confidence intervals")
  
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
    
    
### Let's image that we have no ruler to measure our seals, and instead classify them as big (>125cm) or small (<=125cm). 
  ShrinkingSeals[,size_binary:=ifelse(length>125, "big", "small")]
  
### Let's also pretend that we only know if they are young (<10 year) or old (>=10 years).
### your turn: make the binary age column

  
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
  
  ### where is there excess or deficit? (I asked ChatGPT to write this function using the prompt: "write me R code to generate the deviation between observation and expectation for a 2x2 table. Use the Chisq formula, but return the results as the enrichment=(observation-expectation)/expectation)
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
    
    
    
  
    
    