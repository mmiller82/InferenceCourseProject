library(ggplot2)
lambda <- 0.2
nosim <- 1000
n <- 40
cfunc <- function(x, n)  sqrt(n) * lambda * (mean(x) - 1/lambda)
dat <- data.frame(
    x = c(apply(matrix(rexp(n, lambda), nosim), 1, cfunc, n)),
    size = factor(rep(c(n), rep(nosim, 1))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=2, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g 