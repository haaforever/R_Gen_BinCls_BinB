

fn.v.hypsph <- function(r, d) {
  
  
  # Volume of an n-ball
  
  
  a1 <- pi^(0.5 * d)
  a2 <- 1 / gamma((0.5 * d) + 1)
  a3 <- r^d  
  return(a1*a2*a3)
  
  
}




fn.gen.bincls.binb <- function(a, d, n, rate) {
  
    
  # Input
  # a: outer radius
  # d: dimension
  # n: number of objects to be generated
  # rate: percentage of samples in class 0, error zone, and class 1, individually
  # ex) rate = c(p, q, r)
  #     (n * p) / (p + q + r) samples in class 0
  #     (n * q) / (p + q + r) samples in error zone
  #     (n * r) / (p + q + r) samples in class 1
  # Maximum ahcievable AUC is approximately (p + r) / (p + q + r)
    
  
  # Calculate radius
  p <- rate[1]
  q <- rate[2]
  r <- rate[3]
  b <- ((q*(q + r))/(((p + q)*(q + r))-(p * r)))^(1/d) * a # Error zone radius
  c <- ((r/(r + q))^(1/d)) * b # Inner radius
  
  
  # Generate m samples in a hypercube
  m <- ceiling((n * (2 * a)^d) / fn.v.hypsph(r = a, d = d))
  z <- runif(n = d * m, min = -a, max = a)
  z <- matrix(data = z, nrow = m, ncol = d, byrow = TRUE)
  dst <- apply(X = z, MARGIN = 1, FUN = function(x) return(sqrt(sum(x^2))))
  
  
  # Select samples in a hypersphere
  # Not correctly n samples
  x <- z[dst <= a,]
  dst <- dst[dst <= a]
  
  
  # Labeling
  y <- c()
  y[dst <= c] <- 1
  y[dst > b & dst <= a] <- 0
  ix.noise <- which(dst > c & dst <= b)
  ix.noise.y0 <- sample(x = ix.noise, size = floor(0.5 * length(ix.noise)))
  ix.noise.y1 <- setdiff(x = ix.noise, y = ix.noise.y0)
  y[ix.noise.y0] <- 0
  y[ix.noise.y1] <- 1
  
  
  # Return
  print(paste("Maximum achievable AUC:", round(x = (p + r) / (p + q + r), digits = 4)))
  return(data.frame(x, y))
}

