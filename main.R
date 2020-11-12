library(rmutil)
source("userFun.R")
set.seed(42)
# Number of simulations
B <- 5
# Different sample sizes
n <- c(10, 20, 30, 50)
# Ratio of r
r <- c(0.1, 0.2)
# Model parameters
model_list <- list(list(n_outlier = 0, FUN = rnorm),
                     list(n_outlier = 0.1, sig = 2, FUN = rnorm),
                     list(n_outlier = 0.2, sig = 2, FUN = rnorm),
                     list(n_outlier = 1, sig = 2, FUN = rnorm),
                     list(n_outlier = 2, sig = 2, FUN = rnorm),
                     list(n_outlier = 0, FUN = rlogis, scale_factor = pi/sqrt(3)),
                     list(n_outlier = 0, FUN = rlaplace, scale_factor = sqrt(2)))
# An array to store all estimates 
# 
r_names <- paste0("simu_",1:B)
c_names <- paste0("est_",1:(2 + 3 * length(r)))
d_names <- paste0("model_",1:length(model_list))
for(size in n)
{
  estimates <- array(dim = c(B,
                             2 + 3 * length(r),
                             length(model_list)),
                     dimnames = list(r_names, c_names,
                                     d_names))
  # funs is a list of estimators
  funs <- estimatorGenerator(size, size*r)
  for(j in 1 : length(model_list))
  {
    print(c(size, j))
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
      x <- sort(x)
      # estimate
      estimates[i, ,j] <- sapply(funs, function(f){f(x)})
    }
  }
  save(estimates, file=paste0("n_",size,".RData"))
}






