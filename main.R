source("userFun.R")

B <- 1000000
n <- c(10, 20, 30, 50)
r <- c(0.1, 0.2)
size <- n[1]
funs <- estimatorGenerator(size, size*r)




