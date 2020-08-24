

a <- 5
d <- 3
n <- 10000
rate <- c(2, 1, 2)
dt <- fn.gen.bincls.binb(a = a, d = d, n = n, rate = rate)

table(dt$"y")
library("rgl")
plot3d(dt[,1:3], col = dt$"y" + 1)
