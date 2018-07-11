# Plotting Uniform Distributions In R With ggplot2

# Using the ggplot package to plot various probability distributions.

# install.packages("ggplot2")

library(ggplot2)

## Uniform Distribution - Unif(a, b) or Unif(min, max):

uniform_Plot <- function(a, b){
  xvals <- data.frame(x = c(a, b)) #Range for x-values
  
  ggplot(data.frame(x = xvals), aes(x = x)) + xlim(c(a, b)) + ylim(0, 1/(b - a)) +
    stat_function(fun = dunif, args = list(min = a, max = b), geom = "area", 
                  fill = "green", alpha = 0.35) + 
    stat_function(fun = dunif, args = list(min = a, max = b)) +
    labs(x = "\n u", y = "f(u) \n", 
         title = paste0("Uniform Distribution \n With Min = ", a, " & Max = ", b, " \n")) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_text(face="bold", colour="blue", size = 12),
          axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
    geom_vline(xintercept = a, linetype = "dashed", colour = "red") +
    geom_vline(xintercept = b, linetype = "dashed", colour = "red")
  
}

## Test Cases:

# Plot uniform distribution with min = 1, max = 2

uniform_Plot(a = 1, b = 2) 

# Plot uniform distribution with min = 4, max = 4.5:

uniform_Plot(a = 4, b = 4.5) 

# Plot uniform distribution with min = -1, max = 1:

uniform_Plot(a = -1, b = 1) 

### Standard Uniform Distribution Where it's a uniform dist. with a = 0, b = 1:

uniform_Plot(a = 0, b = 1) 

