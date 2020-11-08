source("userFun.R")
# Number of simulations
B <- 1000
# Different sample sizes
n <- c(10, 20, 30, 50)
# Ratio of r
r <- c(0.1, 0.2)
# Model parameters
model_list <- list(list(n_outlier = 0, FUN = rnorm),
                     list(n_outlier = 0.1, sig = 2, FUN = rnorm),
                     list(n_outlier = 0.2, sig = 2, FUN = rnorm),
                     list(n_outlier = 1, sig = 2, FUN = rnorm),
                     list(n_outlier = 2, sig = 2, FUN = rnorm))
# An array to store all estimates 
estimates <- array(dim = c(B,
                           2 + 3 * length(r),
                           length(model_list)))

for(size in n)
{
  # funs is a list of estimators
  funs <- estimatorGenerator(size, size*r)
  for(j in 1 : length(model_list))
  {
    args_list <- model_list[[j]]
    args_list$n_total <- size
    if(args_list$n_outlier < 1)
    {
      # if it's a ratio 
      args_list$n_outlier <- rbinom(1, size, args_list$n_outlier)
    }
    for(i in 1 : B)
    {
      # generate observations
      x <- do.call(outlierGenerator, args = args_list)
      # estimate
      estimates[i, ,j] <- sapply(funs, function(f){f(x)})
    }
  }
}






