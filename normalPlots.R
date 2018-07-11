## Plotting Normal Distribution Plots in R with ggplot2:

library(ggplot2)

## Standard normal distribution:

xvalues <- data.frame(x = c(-3, 3))

plot <- ggplot(xvalues, aes(x = xvalues))

plot + stat_function(fun = dnorm) + xlim(c(-4, 4))

## Standard Normal Distribution Labelled.

plot + stat_function(fun = dnorm) + xlim(c(-4, 4)) +
  annotate("text", x = 2.3, y = 0.3, parse = TRUE, size = 7, fontface ="bold",
           label= "frac(1, sqrt(2 * pi)) * e ^ {frac(-x^2, 2)}")

## A different normal distribution

normal2 <- function(x){
  dnorm(x, mean = 2, sd = 3)
}

plot + stat_function(fun = normal2) + xlim(c(-10, 15))

## Put both normal distributions together:

plot + stat_function(fun = dnorm, colour = "blue") + 
  stat_function(fun = normal2) + xlim(c(-10, 15)) + 
  labs(x = "\n x", y = "y \n") +
  theme(axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  geom_vline(xintercept = 2, linetype = "dashed", colour = "brown")


### Part Two:

# Shading from x = -1 to x = 1 (within one std deviation):

dnorm_one_sd <- function(x){
  norm_one_sd <- dnorm(x)
  # Have NA values outside interval x in [-1, 1]:
  norm_one_sd[x <= -1 | x >= 1] <- NA
  return(norm_one_sd)
}

area_one_sd <- round(pnorm(1) - pnorm(-1), 4)
area_one_sd

# Plot:

plot + stat_function(fun = dnorm) + 
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "yellow", alpha = 0.3) +
  geom_text(x = 0, y = 0.2, size = 4, fontface = "bold",
            label = paste0(area_one_sd * 100, "%")) +
  scale_x_continuous(breaks = c(-3:3)) + 
  labs(x = "\n z", y = "f(z) \n", title = "Standard Normal Distribution \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12))

# Shading from x = -2 to x = 2 (within two std deviations):

dnorm_two_sd <- function(x){
  norm_two_sd <- dnorm(x)
  # Have NA values outside interval x in [-2, 2]:
  norm_two_sd[x <= -2 | x >= 2] <- NA
  return(norm_two_sd)
}

# Area underneath standard normal distribution within 2 standard deviations (z-score):

area_two_sd <- round(pnorm(2) - pnorm(-2), 4)
area_two_sd

plot + stat_function(fun = dnorm) + 
  stat_function(fun = dnorm_two_sd, geom = "area", fill = "orange", alpha = 0.3) +
  geom_text(x = 0, y = 0.15, size = 4, fontface = "bold",
            label = paste0(area_two_sd * 100, "%")) +
  scale_x_continuous(breaks = c(-3:3)) + 
  labs(x = "\n z", y = "f(z) \n", title = "Standard Normal Distribution \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12))


# Shading from x = -3 to x = 3 (within three std deviations):

dnorm_three_sd <- function(x){
  norm_three_sd <- dnorm(x)
  # Have NA values outside interval x in [-3, 3]:
  norm_three_sd[x <= -3 | x >= 3] <- NA
  return(norm_three_sd)
}


# Area underneath standard normal distribution within 3 standard deviations (z-score):

area_three_sd <- round(pnorm(3) - pnorm(-3), 4)
area_three_sd

plot + stat_function(fun = dnorm) + 
  stat_function(fun = dnorm_three_sd, geom = "area", fill = "green", alpha = 0.3) +
  geom_text(x = 0, y = 0.15, size = 4, fontface = "bold",
            label = paste0(area_three_sd * 100, "%")) +
  scale_x_continuous(breaks = c(-3:3)) + 
  labs(x = "\n z", y = "f(z) \n", title = "Standard Normal Distribution \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12))

#--------------------------------------------------------

### Part Three:

# Summary plot for normal distribution (Version One)

plot + stat_function(fun = dnorm) + 
  stat_function(fun = dnorm_three_sd, geom = "area", fill = "green", alpha = 0.3) +
  stat_function(fun = dnorm_two_sd, geom = "area", fill = "orange", alpha = 0.3) +
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "red", alpha = 0.3) +
  geom_text(x = 0, y = 0.22, size = 4, fontface = "bold",
            label = paste0("<------ ", area_one_sd * 100, "%", " ------>")) +
  geom_text(x = 0, y = 0.15, size = 4, fontface = "bold",
            label = paste0("<------------ ", area_two_sd * 100, "%", " ------------>")) +
  geom_text(x = 0, y = 0.025, size = 4, fontface = "bold",
            label = paste0("<------------------------- ", area_three_sd * 100, 
                           "%", " ------------------------->")) +
  scale_x_continuous(breaks = c(-3:3)) + 
  labs(x = "\n z", y = "f(z) \n", title = "Standard Normal Distribution \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12))

# Summary plot for normal distribution (Version Two)

plot + stat_function(fun = dnorm) + 
  stat_function(fun = dnorm_three_sd, geom = "area", fill = "green", alpha = 0.3) +
  stat_function(fun = dnorm_two_sd, geom = "area", fill = "orange", alpha = 0.3) +
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "red", alpha = 0.3) +
  geom_text(x = 0, y = 0.22, size = 4, fontface = "bold",
            label = paste0("<------- ", round(area_one_sd * 100, 0), "%", " ------->")) +
  geom_text(x = 0, y = 0.15, size = 4, fontface = "bold",
            label = paste0("<------------- ", round(area_two_sd * 100, 0), "%", 
                           " ------------->")) +
  geom_text(x = 0, y = 0.025, size = 4, fontface = "bold",
            label = paste0("<-------------------------- ", round(area_three_sd * 100, 1), 
                           "%", " -------------------------->")) +
  scale_x_continuous(breaks = c(-3:3)) + 
  labs(x = "\n z", y = "f(z) \n", title = "Standard Normal Distribution \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) 

# Summary plot for normal distribution (Version Three)

plot + stat_function(fun = dnorm) + 
  stat_function(fun = dnorm_three_sd, geom = "area", fill = "green", alpha = 0.3) +
  stat_function(fun = dnorm_two_sd, geom = "area", fill = "orange", alpha = 0.3) +
  stat_function(fun = dnorm_one_sd, geom = "area", fill = "orange", alpha = 0.3) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  geom_text(x = 0.5, y = 0.2, size = 3.5, fontface = "bold",
            label = paste0(round((area_one_sd * 100)/2,2), "%")) +
  geom_text(x = -0.5, y = 0.2, size = 3.5, fontface = "bold",
            label = paste0(round((area_one_sd * 100)/2,2), "%")) +
  geom_text(x = 1.5, y = 0.05, size = 3.5, fontface = "bold",
            label = paste0(round((pnorm(2) - pnorm(1)) * 100,2), "%")) +
  geom_text(x = -1.5, y = 0.05, size = 3.5, fontface = "bold",
            label = paste0(round((pnorm(-1) - pnorm(-2)) * 100,2), "%")) +
  geom_text(x = 2.3, y = 0.01, size = 3.5, fontface = "bold",
            label = paste0(round((pnorm(3) - pnorm(2)) * 100,2), "%")) +
  geom_text(x = -2.3, y = 0.01, size = 3.5, fontface = "bold",
            label = paste0(round((pnorm(-2) - pnorm(-3)) * 100,2), "%")) +
  scale_x_continuous(breaks = c(-3:3)) + 
  labs(x = "\n z", y = "f(z) \n", title = "Standard Normal Distribution \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12))