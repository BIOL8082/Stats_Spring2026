install.packages("vegan")
install.packages("ggcorrplot")

### libraries
  library(vegan)
  library(MASS)
  library(data.table)     
  library(ggcorrplot)

### read in gene expression data. This is data for ~400 individual flies that were either starved or fed. Then mRNA was extracted and sequenced.
  ge <- fread("https://raw.githubusercontent.com/BIOL8082/Stats_Spring2026/refs/heads/main/Class06_Multivariate_pt2/gene_expression_simple.csv")
  str(ge)
  dim(ge)
  names(ge)[!grepl("FBgn", names(ge))]
  head(ge$variable)
  ge[,cage:=tstrsplit(variable, "\\.")[[2]]]
  
  
### LDA
  ### specify a training dataset and a testing dataset
    ### the sample function
      sample(letters, size=10)
      sample(100, size=10)
      sample(c(1,10,100), size=2)
      sample(10, size=100)
      sample(10, size=100, replace=T)
      sample(c(1,10,100), size=20, prob=c(.7, .29, .01), replace=T)
      
    ### randomly select 60% of the data as training
      ge[,train:=sample(2, size=dim(ge)[1], replace=T,  prob=c(.6, .4))]
      table(ge$train, ge$trt)
      train <- ge[train==1, -c("train", "variable")]
      
      linear <- lda(trt~., data=train[,-1543])
      str(linear)
      
    ### use the training data to generate the predicted classes. This should work well.
      p <- predict(linear, train)
      str(p)
      ldahist(data = p$x[,1], g = train$trt) ###LD1 matters

    ### is the most likely prediction from the model the same as the real value of inversion status?
      tab <- table(Predicted = p$class, Actual = train$trt)
      tab
      
      sum(diag(tab))/sum(tab) ### accuracy of classification
  
    ### so what about the testing data?
      p2 <- predict(linear, ge[train==2, -c("train", "variable")])
      tab2 <- table(Predicted = p2$class, Actual = ge[train==2, -c("train", "variable")]$trt)
      tab2
      
      sum(diag(tab2))/sum(tab2) ### accuracy of classification
      
       
  ### cluster analysis. Here, we can ask if the gene expression profile for any individual (based on ~2K genes) are correlated with other individuals
      ### first step, look at the correlation structure of the data
        mat <- as.matrix(ge[,-c("train", "variable", "trt")])
        dim(mat)
        rownames(mat) <- ge$variable
        colnames(mat) <- names(ge[,-c("train", "variable", "trt")])
        cor.mat <- cor(t(mat)) ### why do we need to transpose?
        
        cor.dt <- as.data.table(reshape2::melt(cor.mat))
        cor.plot <-ggplot(data=cor.dt, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
          theme(axis.text = element_blank()) + scale_fill_viridis()
        cor.plot
        
      ### hierarchical clustering 
        ge.dist <- dist(mat, method="euclidean")
        ge.hclust <- hclust(ge.dist, method="average")
        plot(ge.hclust)
        str(ge.hclust)      
      
      # reorder correlation plot based on HC clustering
      
        ggcorrplot(cor.mat, hc.order=T, lab=F, method="square", outline.color=NULL, tl.cex=4) +  
          theme(axis.text = element_blank()) + scale_fill_viridis()
      
    ### kmeans
        km <- kmeans(x=mat, centers=3)
        str(km)      
        
        km.dt <- data.table(km$cluster, ge$trt, ge$variable)
        table(km.dt$V1, km.dt$V2)
        
        ge.pc <- prcomp((mat), scale=T, center=T)
       
        km.dt[,PC1:=ge.pc$x[,1]]
        km.dt[,PC2:=ge.pc$x[,2]]
        
        ggplot(data=km.dt, aes(x=PC1, y=PC2, color=as.factor(V1))) + geom_point()        
        
          
### RDA
  ge.norm <- decostand(ge[,-c("variable", "trt", "cage")], "hellinger")
  
  rda_result <- rda(ge.norm[,1:2300] ~ as.numeric(as.factor(trt))+as.numeric(cage), data = ge[,c("variable", "trt", "cage")], scale = TRUE)
  summary(rda_result)
  plot(rda_result, scaling = 2)
        