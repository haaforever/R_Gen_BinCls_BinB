

a <- 5
d <- 3
n <- 10000
rate <- c(2, 1, 2)
dt <- fn.gen.bincls.binb(a = a, d = d, n = n, rate = rate)
table(dt$"y")
library("rgl")
plot3d(dt[,1:3], col = dt$"y" + 1)


a <- 5
d <- 5
n <- 500
rate <- c(10, 1, 1)
dt <- fn.gen.bincls.binb(a = a, d = d, n = n, rate = rate)
table(dt$"y")
colnames(dt)
plot(dt[,paste0("X",1:d)], col = dt$"y" + 1)

