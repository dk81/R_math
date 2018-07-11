# Brownian Motion in R

# One Dimensional Brownian Motion:

# Reference: http://www.phytools.org/eqg/Exercise_4.1/

# Generating a Single 1D BM Path:

# This assumes constant variance over time:

# time:
time <- seq(from = 0, to = 100, by = 1)

# Starting point: W0 at 0 or a different (drift) value:

W0 <- 0

# Variance of Brownian Motion:

sigma_sq <- 0.2

# Brownian Path:

w_t <- c(W0 , cumsum(rnorm(length(time) - 1, mean = 0, sd = sqrt(sigma_sq))))

# Plot Brownian Path:

plot(time, w_t, type = "l", ylim = c(-5, 5), xlab = "Time", ylab = "W(t)")


# ------------------------------------------

# Generating multiple Brownian Paths: (Note complete try later)

# Inputs:

# Time Window [0, T]:

time <- seq(from = 0, to = 100, by = 1)

# Variance:

sigma_sq <- 0.2

# Specifying the number of paths:

numPaths = 30

W_t <- matrix(0, nrow = numPaths, ncol = length(time))

for (i in 1:numPaths) {
  W_t[i, ] <- c(W0 , cumsum(rnorm(length(time) - 1, mean = 0, sd = sqrt(sigma_sq))))
  plot(time, W_t[i, ], xlab = "Time", ylab = "W(t)", type = "l", ylim = c(-15,15))
  for (i in 1:numPaths) lines(time, W_t[i, ])
}








