library(rmutil)
library(Rfast)
source("userFun.R")
set.seed(42)
# Number of simulations
B <- 1000000
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
#  
# For each sample size we use an array to 
# store all estimates
# Then for each data generating model
# estimates will be stored in a B by 8 matrix 
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

####
bias <- variances <- data.frame(matrix(NA, ncol = 2 + 3 * length(r),
                          nrow = length(n) * length(model_list)))

colnames(bias) <- colnames(variances) <- c_names
rownames(bias) <- rownames(variances) <- paste(rep(d_names, each = 4), n, sep = "_")

counter <- 1
for(i in 1 : length(model_list))
{
  for(size in n)
  {
    print(size)
    load(paste0("n_",size,".RData"))
    bias[counter, ] <- colMeans(estimates[,,i])
    variances[counter, ] <- colVars(estimates[,,i])
    counter <- counter + 1
  }
}

save(bias, file = "bias.RData")
save(variances, file = "variances.RData")










