install.packages("psych")
install.packages("viridis")
install.packages("reshape2")
install.packages("foreach")

### libraries
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(psych)
  library(viridis)
  library(foreach)

### pull data from Github
  pheno <- fread("https://raw.githubusercontent.com/BIOL8082/Stats_Spring2026/refs/heads/main/Class06_Multivariate_pt1/wideform.fixed.phenotable.csv")
  inv <- fread("https://raw.githubusercontent.com/BIOL8082/Stats_Spring2026/refs/heads/main/Class06_Multivariate_pt1/inversion.csv")
  
### pad missing data
  ### first, translate from wide to long
    pheno.l <- melt(data=pheno, id.vars="ral_id")
  
  ### summarize missingingness for each phenotype and strain
    missing.pheno <- pheno.l[,list(missing=mean(is.na(value))), list(variable)]
    missing.dgrp <-  pheno.l[,list(missing=mean(is.na(value))), list(ral_id)]
    hist(missing.dgrp$missing)
    hist(missing.pheno$missing)
  
  ### trim
    setkey(pheno.l, variable)
    pheno.l.mp <- pheno.l[J(missing.pheno[missing<.15]$variable)] 
    
    setkey(pheno.l.mp, ral_id)
    pheno.trim <- pheno.l.mp[J(missing.dgrp[missing<.2]$ral_id)]
    pheno.trim
    
  ### pad missing
    pheno.trim.ag <- pheno.trim[,list(mu=mean(value, na.rm=T)), list(variable)]
    pheno.trim <- merge(pheno.trim, pheno.trim.ag, by="variable")
    pheno.trim[,pad_value_pad:=value]
    pheno.trim[is.na(value), pad_value_pad:=mu]
    pheno.trim[,value:=pad_value_pad]
    
  ### long back to wide
    pheno.use <- dcast(pheno.trim, ral_id~variable, measure.var="pad_value_pad")
    dim(pheno.use)  
    summary(pheno.use)

### cormat on observed data
    cor.mat <- cor(mat)
    cor.dt <- as.data.table(reshape2::melt(cor.mat))
    cor.plot <- ggplot(data=cor.dt, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis()
    cor.plot
    

## basic PCA
    mat <- as.matrix(pheno.use[,-"ral_id"])
    rownames(mat) <- pheno.use$ral_id
    pheno.pca <- prcomp(mat, center=T, scale=T)
    summary(pheno.pca)
    str(pheno.pca)    
  
### Scree plot
    scree.dt <- data.table(PC=c(1:95), var=pheno.pca$sdev^2)
    scree.dt[,percent_variance:=var/sum(scree.dt$var)]
    
    scree.plot <- ggplot(data=scree.dt, aes(x=PC, y=percent_variance)) + geom_point(color="black", size=3) +
      theme_bw() +
      ylab("Variance")
    
    scree.plot
    
### Variable loadings
    str(pheno.pca)
    loadings.pca <- as.data.table(pheno.pca$rotation)
    loadings.pca[,trait:=rownames(pheno.pca$rotation)]
    
    
    loadings.plot <- ggplot(data=loadings.pca, aes(xend=PC1, yend=PC2, x=0, y=0)) + 
      geom_segment(arrow=arrow(length = unit(0.15, "cm"))) +
      xlab("PC1") + ylab("PC2") + theme_bw()
    loadings.plot
    
### Your turn: which phenotypes are driving the strong positive loadings onto PC1? What about PC2?
    loadings.pca[,c("trait", "PC1")][PC1 < -.2]
    
    
### Projections
    str(pheno.pca)
    proj <- as.data.table(pheno.pca$x)
    proj[,ral_id:=as.numeric(rownames(pheno.pca$x))]
    
    pc1_pc2 <- ggplot(data=proj, aes(x=PC1, y=PC2)) + geom_point() + theme_bw()
    pc3_pc4 <- ggplot(data=proj, aes(x=PC3, y=PC4)) + geom_point() + theme_bw()
    
### your turn: are the phenotypes that are driving the strong positive loading on to PC1 highly correlated?    
  summary(cor.dt[grep("DDT", Var1)][grep("DDT", Var2)]$value)
  
  
  & grep("DDT", Var2)]
    
    
### PCA megaplot
    layout <- c("
      ABEE
      CDEE
      "
    )
    scree.plot + loadings.plot + pc1_pc2 + pc3_pc4 + cor.plot + plot_layout(design=layout)
    
### factor analysis: https://library.virginia.edu/data/articles/getting-started-with-factor-analysis
   dgrp.fa <- fa(mat, nfactor=50, rotate="varimax", fm="ml", n.obs=141)
   summary(dgrp.fa)     
   str(dgrp.fa)
   dgrp.scores <- as.data.table(dgrp.fa$scores)
   dgrp.scores[,ral_id:=as.numeric(rownames(dgrp.fa$scores))]
   
### inversions
    setnames(inv, c("DGRP Line", "In(2L)t"), c("DGRP", "In2Lt"))
    inv[,ral_id:=as.numeric(tstrsplit(DGRP, "_")[[2]])]
    
    proj <- merge(proj, inv[,c("ral_id", "In2Lt")], by="ral_id")
    proj[,In2Lt:=factor(In2Lt, levels=c("ST", "INV/ST", "INV"))]
    summary(aov(PC1~In2Lt, proj))
    summary(aov(PC1~as.numeric(In2Lt), proj))

    projl <- melt(proj, id.vars=c("In2Lt", "ral_id"))
    inv.pc <- foreach(pc=unique(projl$variable), .combine="rbind")%do%{
      # pc <- "ML2"
      t1 <- lm(value~(In2Lt), data=projl[variable==pc])
      data.table(pc=pc, p=anova(t1)$Pr[1])
    }   
    inv.pc[,pa:=p.adjust(p, method="fdr")]
    inv.pc[p<.05]
    
    ggplot(data=proj, aes(x=In2Lt, y=ML4, group=In2Lt)) + geom_boxplot()  
 
   
   
          
    