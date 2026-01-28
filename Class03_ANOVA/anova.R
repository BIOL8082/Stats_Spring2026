### install new packages
  install.packages("car")
  install.packages("gglm")
  
### libraries
  library(data.table)
  library(ggplot2)
  library(car)
  library(patchwork)
  library(gglm)
  
### load data
  apples <- fread("https://raw.githubusercontent.com/BIOL8082/Stats_Spring2026/refs/heads/main/Class03_ANOVA/apples.csv")  
  str(apples)
  summary(apples)
  apples[,geno:=as.factor(geno)]
  apples <- apples[,c(1:7)]
  
### take a minute to look at the data. use structure, table, and make some histograms.
  tlmm_plot <- ggplot(data=apples, aes(tlmm)) + geom_histogram()
  ovn_plot <- ggplot(data=apples, aes(ovn)) + geom_histogram()
  tlmm_plot + ovn_plot
  
### basic ANOVA - thorax length.
  aov(tlmm~food, data=apples)
  summary(aov(tlmm~food, data=apples))
  summary(aov(tlmm~food+geno, data=apples))
  summary(aov(tlmm~food*geno, data=apples))
  summary(aov(tlmm~food+geno+food:geno, data=apples))
  summary(aov(tlmm~food:geno, data=apples))
  
### your turn - set up the anova for ovariole number (ovn)
  summary(aov(ovn~food*geno, data=apples))
  
### R's base `aov` function uses Type I sum of squares, or sequential sum of squares
### That means that the sum of squared deviations is calculated for the second term, after accounting for the first term.
### Type I is best for balanced designs, which this experiment is not
### notice how the "significance" changes between the models. Which model would you choose to present?
  table(apples$food, apples$geno)
  summary(aov(ovn~food+geno, data=apples))
  summary(aov(ovn~geno+food, data=apples))
  
### Type II is the simultaneous sum of squares. You can implement Type II () using the `car` package. 
### You can read more about different types of sums of squares here. https://md.psych.bio.uni-goettingen.de/mv/unit/lm_cat/lm_cat_unbal_ss_explained.html#type-i-sequential-or-incremental-ss
  Anova(lm(ovn~food+geno, data=apples), type="II")
  Anova(lm(ovn~geno+food, data=apples), type="II")

### Our turn: Does the ovariole number model violate any ANOVA assumptions? Setting up an ANOVA using generalized linear models
  Anova(glm(ovn~food*geno, data=apples, family=poisson()))
  anova(glm(ovn~food*geno, data=apples, family=poisson()))
  
### The ANOVA analysis suggests that there is some effect of food, genotype, and their interaciton. Let's visualize it.
### Your turn: generate the mean and 95% confidence intervals for ovariole number and thorax length for each genotype & food condition.
### Use the aggregation technique from data.table. Here is a handy function to generate confidence intervals
  ci.fun <- function(x) {
    x <- x[!is.na(x)]
    mean.x=mean(x)
    sd.x=sd(x)
    n.x=length(x)
    se=sd.x/sqrt(n.x)
    
    c(mean.x-1.96*se, mean.x+1.96*se)
    
  }
  
  ### a template
  apples.ag <- apples[,list(tlmm.mean=mean(tlmm), tlmm.lci=ci.fun(tlmm)[1], tlmm.uci=ci.fun(tlmm)[2],
                             ovn.mean=mean(ovn),   ovn.lci=ci.fun(ovn)[1],  ovn.uci=ci.fun(ovn)[2]), 
                      list(food, geno)]


### example plot
  dodge.amount=0.2
  
  tlmm.ag.plot <- ggplot(data=apples.ag, aes(x=food, y=tlmm.mean, group=geno, color=geno)) + 
    geom_line(position = position_dodge(width =  dodge.amount)) + 
    geom_point(position = position_dodge(width =  dodge.amount)) + 
    geom_linerange(aes(ymin=tlmm.lci, ymax=tlmm.uci), position = position_dodge(width =  dodge.amount)) +
    theme_bw() +
    ylab("Thorax length (mm)") + xlab("Food treatment") + labs(color="Genotype") +
    scale_x_discrete(labels=c("F"="Fresh", "R"="Rotten"))
  
  ovn.ag.plot <- ggplot(data=apples.ag, aes(x=food, y=ovn.mean, group=geno, color=geno)) + 
    geom_line(position = position_dodge(width =  dodge.amount)) + 
    geom_point(position = position_dodge(width =  dodge.amount)) + 
    geom_linerange(aes(ymin=ovn.lci, ymax=ovn.uci), position = position_dodge(width =  dodge.amount)) +
    theme_bw() +
    ylab("Ovariole number") + xlab("Food treatment") + labs(color="Genotype") +
    scale_x_discrete(labels=c("F"="Fresh", "R"="Rotten"))
                
  tlmm.ag.plot + ovn.ag.plot + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
  

### checking model diagnostics
  ovn.mod <- glm(ovn~food*geno, data=apples, family=poisson())
  gglm(ovn.mod)
  
  lm.mod <- lm(tlmm~food*geno, data=apples)
  gglm(lm.mod)
  
  
### homework: When I set up the experiment, I placed 50 eggs onto each apple. 
### Does food quality or genotype affect egg-adult survival? What is the correct family to use? 
### Make a plot of egg-to-adult survival like we did above. 
### Are there density dependent effects on thorax length or ovariole number?

  
  