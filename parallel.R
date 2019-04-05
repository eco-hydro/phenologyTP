library("doFuture")
registerDoFuture()

# cl <- makeClusterPSOCK(2, outfile = "log3.txt")
# registerDoFuture()
cl <- parallel::makeCluster(2, outfile = "log3.txt")

plan(cluster, workers=cl)

# library("plyr")
# x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE, FALSE, FALSE, TRUE))
# y <- llply(x, quantile, probs = (1:3) / 4, .parallel = TRUE)

mu <- 1.0
sigma <- 2.0
x <- foreach(i = 1:100000) %dopar% { 
    phenofit::runningId(i, 1000)
    rnorm(2, mean = mu, sd = sigma)
}
