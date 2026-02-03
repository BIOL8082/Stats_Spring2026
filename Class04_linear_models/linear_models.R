install.packages("AER")
install.packages("car")

### libraries
  library(data.table)
  library(ggplot2)
  library(AER)
  library(gglm)
  library(car)
  library(patchwork)

### load data
  data("EquationCitations", package = "AER")
  EquationCitations <- as.data.table(EquationCitations)

### look at the data
  str(EquationCitations)
  ggplot(data=EquationCitations, aes(cites)) + geom_histogram()
  ggplot(data=EquationCitations, aes(pages)) + geom_histogram()
  ggplot(data=EquationCitations, aes(equations)) + geom_histogram()
  ggplot(data=EquationCitations, aes(mainequations)) + geom_histogram()

  ggplot(data=EquationCitations, aes(log(cites))) + geom_histogram()
  ggplot(data=EquationCitations, aes((mainequations))) + geom_histogram()



### basic correlations
  cor(EquationCitations$equations, EquationCitations$mainequations) ### pearson
  str(cor.test(EquationCitations$equations, EquationCitations$mainequations)) ### pearson by default
  cor.test(EquationCitations$equations, EquationCitations$mainequations, method="spearman") ### spearman (non-parametric)

### basic linear regression
  m1 <- lm(cites~pages, data=EquationCitations)
  summary(m1)
  anova(m1)
  gglm(m1)
  ggplot(data=EquationCitations, aes(x=pages, y=cites)) + geom_point()

### Our turn - transforming the data to meet assumptions of normality
  m1 <- lm(log(cites)~pages, data=EquationCitations)
  summary(m1)
  anova(m1)
  gglm(m1)
  ggplot(data=EquationCitations, aes(x=pages, y=cites)) + geom_point()

### Your turn - multiple linear regression
### write the a model that includes `pages` & `mainequations` as additive terms (call that model m3)
### next, write a different model that includes and interaction between pages & mainequations (call that model m4)
### do we need to tranform the data?
  m3 <- lm(log(cites) ~ pages + mainequations, data=EquationCitations)
  summary(m3)
  gglm(m3)

  m4 <- lm(log(cites) ~ pages * mainequations, data=EquationCitations)
  summary(m4)

  anova(m3, m4, test="Chisq")

### Your turn: is there colinearity between the predictors?
  cor.test(EquationCitations$pages, EquationCitations$mainequations)

### variance inflation factor
  car::vif(m3)

### some example plots to show relatioships
  eqns_plot <- ggplot(data=EquationCitations, aes(x=log(mainequations+1), y=log(cites))) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw()

  pages_plot <- ggplot(data=EquationCitations, aes(x=log(pages), y=log(cites))) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw()

  pages_plot + eqns_plot + plot_annotation(tag_levels="A")

### quadratic terms
  m5 <- lm(log(cites) ~ log(pages) + I(log(pages)^2), data=EquationCitations)
  m6 <- lm(log(cites) ~ log(mainequations+1) + I(log(mainequations+1)^2), data=EquationCitations)
  m7 <- lm(log(cites) ~ log(mainequations+1) + log(mainequations+1)^2, data=EquationCitations)

  summary(m5)
  summary(m6)
  summary(m7) ### IDK why we have to wrap wrap the coefficient in I.

### ANCOVA. Does the relationship between cites, eqns, and paper length vary among journals?
  m8 <- lm(log(cites) ~ journal, data=EquationCitations)
  summary(m8)

  m8a <- lm(log(cites) ~ journal + log(pages) + log(mainequations+1), data=EquationCitations)
  summary(m8a)

  m8b <- lm(log(cites) ~ journal * log(pages) +
                         journal * log(mainequations+1), data=EquationCitations)
  summary(m8b)

  m8c <- lm(log(cites) ~ journal * log(pages) *  log(mainequations+1), data=EquationCitations)
  summary(m8c)


### your turn: run anova between these models; does the best fit model look "okay"?
  anova(m8, m8a, m8b, m8c)
  anova(m8c)

### homework: total citations are a major currency associated with papers, but they are composed of two types of citations:
### 'self-citations' versus 'citations from others'.
### Does the effects of paper length and their use of equations differ between self and other citations?
### Build two different models with self-cites and other-cites as the response models, and journal, page length and main text equations as the predictors
### Make sure that these models use the proper GLM family.
### You'll need to read the glm help page (and possibly other help pages) to determine the best family.
### what model (and interactions) do you think are most important?
### submit a single PDF/word doc with (A) A composite figure showing your result, (B) the output summary of your best model, (C) A few sentences describing your result,
  ## (D) your code
