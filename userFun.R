generateWeights <- function(n, r, type = 0)
{
  # This function generates weights for weight means
  w <- numeric(n)
  if(type == 0)
  {
    r <- 0
    type <- 1
  }
  if(type == 4)
  {
    r <- ceiling(n / 2) - 1
    type <- 1
  }
  if(type == 1){
    # type 1 is the r-trimmed mean
    w[(r + 1) : (n - r)] <- 1
  }else if(type == 2){
    # type 2 is the r-Winsorized mean
    w[(r + 1) : (n - r)] <- 1
    w[c(r + 1, n - r)] <- r + 1
  }else{
    # r-linearly weighted mean
    len <- n - 2 * r
    h_len <- floor(len/2)
    temp <- seq(from = 1, by = 2, length.out = h_len)
    w[(r + 1) : (r + h_len)] <- 
      w[(n - r) : (n - r - h_len + 1)] <- temp
  }
  return(w)
}

weightedMeanGenerator <- function(w, ...)
{
  return(function(x){weighted.mean(x, w, ...)})
}

estimatorGenerator <- function(n_total, r_list, ...)
{
  funs <- vector("list", 2 + 3 * length(r_list))
  temp <- 1
  for(j in 0:4)
  {
    for(r in r_list)
    {
      funs[[temp]] <- local({
        w <- generateWeights(n_total, r, type = j)  
        weightedMeanGenerator(w, ...)
      })
      temp <- temp + 1
      if(j %in% c(0, 4)) break
    }
  }
  return(funs)
}

# a <- estimatorGenerator(10, 10*c(0.1,0.2), na.rm = T)
# lapply(a, function(f){as.list(environment(f))})


outlierGenerator <- function(n_total, n_outlier,
                              FUN, loc = 0, 
                              sig = 1, ...)
{
  # This function can generate n_total r.vs
  # that contain n_outlier outliers
  x <- FUN(n_total, ...)
  if(n_outlier > 0)
  {
    temp <- x[1:n_outlier]
    x[1:n_outlier] <- sig * temp + loc
  }
  return(x)
}

