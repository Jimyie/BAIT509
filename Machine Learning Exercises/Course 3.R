library(tidyverse)
  xgrid <- seq(-5, 4, length.out=1000)
  kNN_estimates <- map_dbl(xgrid, function(x){
    ## YOUR CODE HERE FOR kNN
    ## Note: The variable "x" here is a single value along the grid.
    ## Hint1: Extend your code for kNN from the previous exercise.
    ## Hint2: Say you store the prediction in the variable "yhat".
    ##         Then in a new line of code, write: return(yhat)
    dat$d <- abs(dat$x-x)
    dat2 <- arrange(dat,dat$d)
    pre.knn=dat2[1:10,]
    predicty.knn=mean(pre.knn$y)
    return(predicty.knn)
  })
  loess_estimates <- map_dbl(xgrid, function(x){
    ## YOUR CODE HERE FOR LOESS
    ## Note: The variable "x" here is a single value along the grid.
    ## Hint1: Extend your code for loess from the previous exercise.
    ## Hint2: Say you store the prediction in the variable "yhat".
    ##         Then in a new line of code, write: return(yhat)
    dat$d <- abs(dat$x-x)
    dat2 <- arrange(dat,dat$d)
    pre.loess=filter(dat2,dat2$d<0.9)
    predicty2.loess=mean(pre.loess$y)
    return(predicty2.loess)
  })
  est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
    gather(key="method", value="estimate", kNN, loess)
  ggplot() +
    geom_point(data=dat, mapping=aes(x,y), colour="orange") +
    geom_line(data=est, 
              mapping=aes(x,estimate, group=method, colour=method)) +
    theme_bw()