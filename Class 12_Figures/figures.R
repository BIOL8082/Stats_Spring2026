install.packages("ggpubr")
install.packages("ggstatsplot")
install.packages("apaTables")

### libraries
  library(data.table)
  library(ggplot2)
  library(ggpubr)
  library(AER)
  library(ggstatsplot)
  library(apaTables)

### load data
  data("EquationCitations", package = "AER")
  EquationCitations <- as.data.table(EquationCitations)

### look at the data
  str(EquationCitations)         
          
### boxplot
  p <- ggboxplot(EquationCitations, x = "journal", y = "cites",
                 color = "journal", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "journal")
  p
  
  my_comparisons <- list( c("AmNat", "Evolution"), c("AmNat", "ProcB"), c("Evolution", "ProcB") )
  p + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
    stat_compare_means(label.y = 600)                   # Add global p-value

  
### Your turn: Can we use stat_compare_means with the standard geom_boxplot?
 
### ggstatsplot version
  EquationCitations[,lCites:=log10(cites+1)]
  ggbetweenstats(data=EquationCitations, x=journal, y=lCites) 
  
### Your turn: can we combine "ggbetweenstats" and "stat_compare_means?  What do we think?
 
### tables
  t1 <- lm(log10(cites)~journal, data=EquationCitations)
  apa.aov.table(t1, filename="~/tst.doc")


