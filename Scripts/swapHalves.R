# Created by https://github.com/mm0hgw

swapHalves <- function(x) {
    lx <- length(x)
    key1 <- seq(lx%/%2)
    key2 <- seq(to = lx, length.out = lx%/%2)
    out <- x
    out[key1] <- x[key2]
    out[key2] <- x[key1]
    out
}

swapHalves2 <- function(x) {
    lx <- length(x)
    hlx <- lx%/%2
    key1 <- seq(hlx)
    key2 <- seq(to = lx, length.out = hlx)
    out <- vector(typeof(x), length(x))
    out[key1] <- x[key2]
    out[key2] <- x[key1]
    if (lx%%2 == 1) {
        out[hlx + 1] <- x[hlx + 1]
    }
    out
}

runTest <- function(testData = seq(1e+07)) {
    microbenchmark::microbenchmark(swapHalves(testData), swapHalves2(testData))
}

print(runTest(1e7))
print(runTest(1e7+1))
