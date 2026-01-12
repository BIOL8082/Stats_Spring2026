### installing packages. You only need to run these lines once (per computer, or after you've installed the latest version of R)
  install.packages("data.table"
  install.packages("ggplot2")
  install.packages("foreach")
  install.packages("readxl")
  install.packages("abd")
  install.packages("patchwork")
  install.packages("googlesheets4")

### libraries. You need to run these lines at the start of every R session.
  library(data.table)
  library(ggplot2)
  library(foreach)
  library(readxl)
  library(abd)
  library(patchwork)
  library(googlesheets4)

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
    fun <- function(x, y) {
      x*y
    }
    fun(4978,2)

    fun2 <- function(x, y) {
      data.table(x=x, y=y, xy=x*y)
    }
    fun2(4978,2)
    fun2(x=c(1,10,100), y=2)
    fun2(x=c(1,10,100), y=c(2,4))
    fun2(x=c(1,10,100,1000), y=c(2,4))

    ## the code inside a function can access objects in the general workspace (be careful). But only one object can be retunred from a function. Usually it is the last line (if that line prints to the console).
    fun2 <- function(x, y) {
      return(data.table(x=x, y=y, xy=x*y)) ### or you can return it
    }

### importing data
  ### "flat" csv files
    read.csv("~/test_data.csv")
    fread("~/test_data.csv") ### useful funciton from data.table. Flexible, tolerant, tidy. All the things you look for in a function.
    fread("https://raw.githubusercontent.com/BIOL8082/Stats_Spring2026/refs/heads/main/Class01_obects_data_types/test_data.csv") ### it can also read directly from the internet

  ### Excel files
    read_excel("~/test_data.xlsx", sheet="Dataset 3 320-324 cont.") ### tibbles are like data.tables except that Alan does not use them.
    as.data.table(read_excel("~/test_data.xlsx", sheet="Dataset 3 320-324 cont.")) ### that is better

  ### Google sheets
    read_sheet("https://docs.google.com/spreadsheets/d/1t1OUUDtHbtmWPtp3pS32MI2awU9V8bfmoi3NBgqtDnQ/edit?usp=sharing")

  ### Rdata/RDS files
    dt <- fread("https://raw.githubusercontent.com/BIOL8082/Stats_Spring2026/refs/heads/main/Class01_obects_data_types/test_data.csv")
    save(dt, file="~/dt.Rdata") ### this sames an environment with named variables
    save(dt, fun2, l, file="~/objects.Rdata") ### you can save multiple objects
    saveRDS(dt, file="~/dt.RDS") ### saves a single R object but when you re-import you have to collec the data in a new object

    ### clear the workspace
    ls() ### these are the objects in the workspace
    rm(list=ls()) ### this removes all of the objects

    ### load data
    load(file="~/dt.Rdata")
    ls()

    ### try again
    rm(list=ls()) ### this removes all of the objects
    load(file="~/objects.Rdata")
    ls()

    ### now with RDS
    readRDS(file="~/dt.RDS")
    dt <- readRDS(file="~/dt.RDS")

### looking at data objects
  dim(dt)
  str(dt)
  summary(dt)
  str(l)
