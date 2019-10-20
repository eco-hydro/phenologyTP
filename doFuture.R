library("doParallel")
registerDoParallel(parallel::makeCluster(2))

mu    <- 1.0
sigma <- 2.0
x <- foreach(i = 1:3) %dopar% { rnorm(i, mean = mu, sd = sigma) }
str(x)

foo <- function() {
    foreach(i = 1:30) %dopar% { 
        print(i)
        runningId(i)
        Sys.sleep(1)
        message(i)
        rnorm(i, mean = mu, sd = sigma) 
    }
}

library("doFuture")
registerDoFuture()

killCluster()
cl <- makeCluster(3, outfile = "log2.txt")
plan(cluster, workers = cl)

foo()

