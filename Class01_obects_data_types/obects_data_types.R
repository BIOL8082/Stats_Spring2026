### installing packages. You only need to run these lines once (per computer, or after you've installed the latest version of R)
  install.packages("data.table"
  install.packages("ggplot2")
  install.packages("foreach")
  install.packages("readxl")
  install.packages("abd")
  install.packages("patchwork")

### libraries. You need to run these lines at the start of every R session.
  library(data.table)
  library(ggplot2)
  library(foreach)
  library(readxl)
  library(abd)
  library(patchwork)

### types of data
### Data in can be represented (most commonly) as a number, in integer, a character, a factor.
  ## numbers
    1.1
    1
    as.numeric(1)
    is.numeric(1)

  ### integer
    1
    2
    3
    as.integer(1)
    as.integer(2.6)
    is.integer(2.4)

  ### character
    "a"
    "b"
    as.character("a")
    as.character(1)

  ### factor
    factor("a")
    factor(c("a", "b"))
    factor(c("a", "b"), levels=c("b", "a"))
    factor(c(1,10,"a"))
    factor(c(1,10,2))
    as.numeric(factor(c(1,10,2)))
    as.numeric(factor(c(1,10,2), levels=c(1,10,2)))

### class of objects.
### There are different types of objects in R. These types of object have different purposes and features
### To determine the type of an R object, use the function `class`
  ## vectors. Vectors are one-dimensional objects. They have a length, and are coerced to be the same class.
    vec <- c(1,2,3)
    vec <- c("A", "B", "C")
    vec <- c(1, "B", "C")

  ## matrices. Matrices are two-dimensional objects of the same type
    matrix(c(1:9), nrow=3, ncol=3)
    matrix(c(1:9), nrow=3)
    matrix(c(1:9), ncol=3)
    matrix(c(1:8), ncol=3)
    matrix(c(1:9), ncol=3, byrow=T)
    mat <- matrix(c(1:9), ncol=3, byrow=F)
    mat

    mat[1,]  ### this prints the first row
    mat[,1]  ### this prints the first column
    mat[1,3] ### first row, 3rd column

  ## array. Arrays are n-dimensional objects of same type
    arr <- array(c(1:27), dim=c(3,3,3))
    arr[,,1]
    arr[,1,2]
    arr[1,,2]

  ## data.frames. These are two-dimensional objects, similar to Excel sheets. Each column can have its own data type
    data.frame(fruit=c("apple", "pear", "orange"), num=c(1,2,3))
    df <- as.data.frame(mat)
    df$V2  ### dollar signs are one way that you can access the columns.
    df[,1]
    df[1,]
    summary(df)

    df2 <- data.frame(fruit=c("apple", "pear", "orange"), tastiness=c("good", "great", "awesome"))
    df2[df2$fruit=="pear",]

  ## data.tables. Data-tables are much better versions of data.frames. They are usually interchangeable, but there are additional feautes of data.tables that make them usefule, especially for lager objects
    data.table(fruit=c("apple", "pear", "orange"), num=c(1,2,3))

    data.table(fruit=rep(c("apple", "pear", "orange"), each=100),
              num=c(rnorm(n=100,mean=1,sd=2), rnorm(n=100,mean=2,sd=2), rnorm(n=100,mean=2,sd=2)))

    dt <- data.table(fruit=rep(c("apple", "pear", "orange"), each=100),
              num=c(rnorm(n=100,mean=1,sd=2), rnorm(n=100,mean=2,sd=2), rnorm(n=100,mean=2,sd=2)))
    dt
    dt[1,2] ### same as above
    dt[fruit=="apple",] ## same as above
    dt[fruit=="apple"] ## simpler notation but same result
    dt[,list(average=mean(num)), list(fruit)] ## one of the more usefule features of data.tables. You can calculate summary statistics for individual levels of columns.

    setkey(dt, fruit) ### indexing is another very useful feature of data.tables.
    dt[J("pear")] # You can do quick subsets.
    dt[fruit=="pear"] # For small-moderate size datasets, the speed gain is not so large compared to "vectorized searches". Notice also the simler notation of data.table

    dt2 <- as.data.table(df2) ### you can merge two data.tables by a common column. You can also merge by multiple columns
    setkey(dt2, fruit)
    merge(dt, dt2)
    merge(dt, dt2, by="fruit")

  ## lists. lists are collections of other objects. Each element of a list can be a different object type, or can be lists themselves
    l <- list()
    l[[1]] <- df
    l[[2]] <- dt2
    names(l) <- c("a", "b")
    l$b
    l[[1]]
    l[1]

  ## functions. Functions take input data, do something to that data and return it.
    


### importing data
  ### "flat" csv files
  ### Excel files
  ### Google sheets
  ### Rdata/RDS files




### looking at data objects
  dim
  str
  summary
