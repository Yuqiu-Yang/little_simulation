# Project 
generateWeights <- function(n, r, type = 3)
{
  w <- numeric(n)
  len <- n - 2 * r
  if(type == 1){
    w[(r + 1) : (n - r)] <- 1
  }else if(type == 2){
    w[(r + 1) : (n - r)] <- 1
    w[c(r + 1, n - r)] <- r + 1
  }else{
    h_len <- floor(len/2)
    temp <- seq(from = 1, by = 2, length.out = h_len)
    w[(r + 1) : (r + h_len)] <- 
      w[(n - r) : (n - r - h_len + 1)] <- temp
  }
  return(w)
}

estimator <- function(x, w, FUN, ...)
{
  coef_list <- ls()
  value_list <- sapply(as.list(match.call())[-1],deparse)
  missing_list <- setdiff(coef_list,names(value_list))
  temp <- c("w", "FUN") %in% missing_list
  if(all(temp)){
    stop("Please provide the weights or the funtion")
  }else if(temp[1]){
    return(FUN(x, ...))
  }else{
    return(weighted.mean(x, w, ...))
  }
}


outlier_generator <- function(n_total, n_outlier,
                              FUN, loc = 0, 
                              sig = 1, ...)
{
  x <- FUN(n_total, ...)
  if(n_outlier > 0)
  {
    temp <- x[1:n_outlier]
    x[1:n_outlier] <- sig * temp + loc
  }
  return(x)
}

# a <- outlier_generator(1000, rbinom(1, 1000, 0.1), FUN = rnorm, sig = 1, loc = 10)



