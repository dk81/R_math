# Inverse CDF Monte Carlo Method Examples (With ggplot2)
# Reference: Financial Mathematics: A Comprehensive Treatment By
#                Giuseppe Campolieti and Roman Makarov

library(ggplot2)

### Example One: 

# Generating Unif(a, b) Random Variables From Standard Uniform R.Vs Unif(0, 1)

set.seed(711) # Set random seed, use this to reproduce these results.


# Simulating 1 Million i.i.d Unif(a = 2, b = 4) R.Vs Case & Plot

uniforms_ab_sim <- runif(n = 10^6, min = 2, max = 4)

# Plot of simulated Unif(2, 4) random variables; Should be close to a rectangle:

ggplot(data = NULL, aes(uniforms_ab_sim)) +
  geom_histogram(binwidth = 0.1, boundary = 2) + 
  labs(x = "\n Uniform Random Variable Value", y = "Count \n", title = "Simulated Unif(2, 4) \n Random Variables Histogram \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12))

# CDF of Unif(a, b) is F(x) = (x - a)/(b - a) for x in open interval (a, b). 
# Inverse CDF is obtained by solving for x.

# The generating formula for generating Unif(a, b) ends up being: 
# F^(-1)(U)  = a + (b - a)U where U is a Unif(0, 1) random variable.
# Generating Unif(a = 2, b = 4) using Inverse CDF Method and Unif(0, 1):

unif_invCDFs <- 2 + 2*runif(n = 10^6, min = 0, max = 1)

# Plot should be similar to the plot before.

ggplot(data = NULL, aes(unif_invCDFs)) +
  geom_histogram(binwidth = 0.1, boundary = 2) + 
  labs(x = "\n Uniform Random Variable Value", y = "Count \n", title = "Simulated Uniform Random Variables \n From The Inverse CDF Method \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12))


### Example Two:

# We generate exponential random variables from uniform random variables
# through the Inverse CDF Method.

# Exponential simulation in R:

lambdaRate = 5 #lambdaRate value can be changed accordingly

# Simulating exponential random variables with a sample size of 10000.

E <- rexp(10000, lambdaRate)

# Plot of exponential random variables; Should be close to exponential decay.

ggplot(data = NULL, aes(E)) +
  geom_histogram(binwidth = 0.1, boundary = 2) + 
  labs(x = "\n Exponential Random Variable Value", y = "Count \n", title = "Simulated Exponential \n Random Variables Histogram \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12))


# Simulating Exponential Random Variables Using Unif(0,1) With Inverse CDF Method

U <- runif(n = 10000, min = 0, max = 1) # Simulate Unif(0,1) r.v's
X <- -(log(U)/lambdaRate) # Generating formula from Inverse CDF

# Plotting the simulation results:

ggplot(data = NULL, aes(X)) +
  geom_histogram(binwidth = 0.1, boundary = 2) + 
  labs(x = "\n Exponential Random Variable Value", y = "Count \n", 
       title = "Exponential Random Variables \n From The Inverse CDF Method") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12))

### Example Three: Pareto Distribution Simulations

# Resources: http://www.math.wm.edu/~leemis/chart/UDR/PDFs/Pareto.pdf
# http://isites.harvard.edu/fs/docs/icb.topic1475777.files/stat110hw5solutions.pdf

# Inverse CDF Method for simulating Pareto rvs:

pareto_invCDF_funct <- function(n = 1, lambda = 1, k = 1){
  unifs <- runif(n, min = 0, max =  1) # Simulate standard uniforms.
  invCDF_formula <- lambda/((1 -  unifs)^(1/k)) # lambda / ((1 - U))^(1/k)
  invCDF_formula <- lambda/(unifs^(1/k)) # U is a standard uniform rv; 1 - U also std. uniform
  return(invCDF_formula)
}

pareto_11 <- pareto_invCDF_funct(10000, lambda = 1, k = 1)


# Plot of simulated Pareto lambda = 1, k = 1 random variables; 

ggplot(data = NULL, aes(pareto_11)) + 
  geom_histogram(binwidth = 0.1, boundary = 1) + xlim(c(0, 25)) +
  labs(x = "\n Pareto Random Variable Value", y = "Count \n", title = "Simulated Pareto \n Random Variables Histogram \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12))

# Pareto with lambda = 2 and k = 2

pareto_22 <- pareto_invCDF_funct(10000, lambda = 2, k = 2)


# Plot of simulated Pareto lambda = 2, k = 2 random variables; 

ggplot(data = NULL, aes(pareto_22)) + 
  geom_histogram(binwidth = 0.1, boundary = 2) + xlim(c(0, 25)) +
  labs(x = "\n Pareto Random Variable Value", y = "Count \n", title = "Simulated Pareto \n Random Variables Histogram \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12))

# Testing: 

library(ggfortify)

autoplot(density(pareto_invCDF_funct(n = 10^6, lambda = 1, k = 1)), fill = "orange",
         xlim = c(0.5, 1)) + ggtitle("Plot")
