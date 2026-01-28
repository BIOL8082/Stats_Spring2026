install.packages("lmerTest")

### libraries
  library(data.table)
  library(ggplot2)
  library(lme4)
  library(patchwork)

### load data
  load("/Users/alanbergland/Documents/work/Projects/2005-2019/2010_Dissertation/Individual Projects/Clinal Populations/ Data/Clinal_analysis.Rdata")
  clinal <- as.data.table(clinal)
  clinal <- clinal[,c("ovn", "tlmm", "food", "vial", "block", "geno")]
  write.csv(clinal, file="/Users/alanbergland/Documents/GitHub/Stats_Spring2026/Class05_modelSelection_mixedEffects/clinal_flies.csv", quote=F, row.names=F)
  
  
### look at the data
### food is food quality measured as yeast concentration
### vial is the rearing vial
### block is the experimental block
### geno is isofemale line genotype name
  
### basic linear model
  m1 <- lm(tlmm~food*geno, data=clinal)
  summary(m1)  
  anova(m1)  
  
### basic mixed effect model
  m2 <- lmer(tlmm~food + (1|geno), data=clinal)
  summary(m2)
  anova(m2)  
  
  m3 <- lmer(tlmm~food+(food|geno), data=clinal)
  summary(m3) 
  
### What does the structure of the mixed effect model look like?
### this is an example of an S4 object class. You can see tht because it uses the "@" notation
  str(m3)
  fixef(m3) ### these are the main effects of food, on average across all of the other random effects
  ranef(m3) ### these are the specific coefficients for each level of the random effects. You can add each row to the fixed effects to get the predictions
  
  predict(m3, re.form=NULL) ### utilizes the fixed and random effects
  
### your turn: capture the output of the predict function in a new column of the original data frame "clinal".
### call that new column, "pred"
  
  
### our turn: make a plot of the model predictions. 
### Compare that to a plot of the real data, with individual linear models for each genotype.
### what is different?
  lmer_plot <- ggplot(data=clinal, aes(x=food, y=pred, group=geno, color=geno)) + 
    geom_line() + ggtitle("LMER predictions") +
    coord_cartesian(ylim = c(.9, 1.2))
  
  lm_plot <- ggplot(data=clinal, aes(x=food, y=tlmm, group=geno, color=geno)) + 
    geom_smooth(method="lm", formula=y~x, se=F) + 
    ggtitle("linear model")  +
    coord_cartesian(ylim = c(.9, 1.2))
  
  lm_plot + lmer_plot + plot_annotation(tag_levels="A") + plot_layout(guides="collect")
  
### Conduct an ANOVA between m4 and m5 to test the "significance" of food-genotype term. 
  anova(m2, m3)

### Flies are reared in vials, and vial effect can be profound. 
### How does the incorporation of the vial effect change the interpretation of our results?
  t0 <- lmer(tlmm~1+(1|vial), data=clinal)
  
  t1 <- lmer(tlmm~food+(1|vial), data=clinal)

  t2 <- lmer(tlmm~food+(1|geno)+(1|vial), data=clinal)
  summary(t2)  
  
  t3 <- lmer(tlmm~food+(food|geno)+(1|vial), data=clinal)
  summary(t3)  
  
  anova(t3, t2, t1, t0)
    
  lmerTest::ranova(t2)
  

  
    