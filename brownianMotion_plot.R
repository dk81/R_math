# Brownian Motion In R:

# Resource: http://www.phytools.org/eqg/Exercise_4.1/
# https://stackoverflow.com/questions/17150183/plot-multiple-lines-in-one-graph
# R documentation for BM() function in library(sde) or https://cran.r-project.org/web/packages/sde/sde.pdf

# Brownian Motion is a stochastic (random) process based on the normal distrubiton.
# Recall that a stochastic process is a set of random variables indexed by time.

# With a Brownian motion, we have a sequence of normal random variables dependent on time.

# If W_t is Brownian motion then W_t follows a normal distribution of a mean of zero and a 
# variance of time itself. This variance is non-constant and changes depending on time.

library(ggplot2)

brownian_motion_fn <- function(t0 = 0, T = 1, N = 10, col_line = "black"){
  if (T <= 0 | T %% 1 != 0){
    print("Please enter a whole number that is 1 or greater.")
  } 
  else if (t0 >=  T){
    stop("Inital time needs to be less than the end time.")
  }
  else if (t0 < 0){
    stop("Initial time must be at least 0.")
  }
  else {
    
    # Initialize

    dt <- (T - t0)/N  #Time increment
    
    time_vect <- seq(from = t0, to = T, by = dt)
    
    # Remember that W_0 = 0 (Brownian Motion starts at 0 when t = 0) but user may not put t0 = 0.
    # First value would be W_t0 = Norm(mean = 0, var = t0)
    
    psn_vector <- c(rnorm(n = 1, mean = 0, sd = sqrt(t0)), cumsum(rnorm(n = N, mean = 0, sd = sqrt(dt))))
    
    # For Loop Method would be:
    
    #for (i in 1:N){
    #  psn_vector[i + 1] = psn_vector[i] + rnorm(n = 1, mean = 0, sd = sqrt(dt))
    #}
    
    # Put results into a data frame:
    bm_df <- data.frame(time_vect, psn_vector)
    
    colnames(bm_df) <- c("t", "Position") #Rename columns
    
    print(head(bm_df, n = 12))
    
    # Ggplot with time on horizontal axis, and running total on vertical axis.
    
    ggplot(bm_df, aes(x = t, y = Position)) + 
      geom_line(colour = col_line) +
      labs(x = "\n Time (t)", y = "Brownian Position \n", title = paste0("Brownian Motion Plot \n")) + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.title.x = element_text(face="bold", colour="blue", size = 12),
            axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
      geom_hline(yintercept = 0, colour = "red", linetype = "dashed")
  
  }
}

brownian_motion_fn(t0 = -1, T = 10, N = 100, col_line = "darkgreen") # Error

brownian_motion_fn(t0 = 100, T = 10, N = 100, col_line = "darkgreen") # Error

brownian_motion_fn(t0 = 0, T = 10, N = 100, col_line = "darkgreen") # Works


## Brownian Motion With Drift (mu):

# Code is similar to above. We add another function argument which is the drift (mu).

# psn_vector <- c(rnorm(n = 1, mean = 0, sd = sqrt(t0)) + mu , cumsum(rnorm(n = N, mean = 0, sd = sqrt(dt))) + mu)
# geom_hline(yintercept = mu, colour = "red", linetype = "dashed")

##-------------------------------------------

### Multiple Brownian Motion Paths:

t0 <- 0
T <- 20

N <- 1000

dt <- (T - t0) / N

time_vect <- seq(from = t0, to = T, by = dt)

num_sim <- 8

# For Loop For multiple paths:
psn_vector2 <- matrix(data = 0, nrow = num_sim, ncol = length(time_vect))

for (i in 1:num_sim){
  psn_vector2[i, ] <- c(0, cumsum(rnorm(n = N, mean = 0, sd = sqrt(dt))))
}



# Transpose and add times column (Transpose = switch rows with columns and viceversa):

psn_vector2_t <- cbind(Time = time_vect, t(psn_vector2))

head(psn_vector2_t)

# Rename columns: p_t which is position at time t.

for (i in 1:num_sim){
  colnames(psn_vector2_t)[i + 1] <- paste0("Path_", i)
}

# Remove row names, have them blank

rownames(psn_vector2_t) <- NULL

# Preview again:

head(psn_vector2_t)

# Convert to data.frame for gather().

psn_vector2_t <- data.frame(psn_vector2_t)

# Wide Format To Long Format:

library(tidyr)
library(dplyr)

psn_vector2_l <- psn_vector2_t %>% gather(Path, Position, -Time)

# Check:

head(psn_vector2_l)

# ggplot2 Plot: 

# Ggplot with time on horizontal axis, and running total/position on vertical axis.

ggplot(psn_vector2_l, aes(x = Time, y = Position, group = Path)) + 
  geom_line() +
  labs(x = "\n Time (Seconds)", y = "Position \n", 
       title = paste0("Brownian Motion Paths With ", length(time_vect) - 1, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# If I want different colours for each path I would add colour = Path in aes().

ggplot(psn_vector2_l, aes(x = Time, y = Position, group = Path, colour = Path)) + 
  geom_line() +
  labs(x = "\n Time (Seconds)", y = "Position \n", 
       title = paste0("Brownian Motion Paths With ", length(time_vect) - 1, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

###------------------------------------------

# Non-For Loop Way By Using replicate().

# Replicate example: replicate(n = 10, runif(n = 8))

# Replicate function version:

library(ggplot2)

# Using the cumsum() function to generate random walk more efficently:

t0 <- 0
T <- 20

N <- 1000

dt <- (T - t0) / N

time_vect <- seq(from = t0, to = T, by = dt)

num_sim <- 8

time_length <- length(time_vect)

# Multiple paths:

psn_vector2 <- matrix(data = 0, nrow = time_length, ncol = num_sim)


# Fill rows with each random walk path realization with replicate():

psn_vector2 <- replicate(num_sim, c(cumsum(rnorm(n = time_length, mean = 0, sd = sqrt(dt)))))


psn_vector2 <- data.frame(psn_vector2)


# Check:

head(psn_vector2)

# Add times column (No need to transpose here):

psn_vector2 <- cbind(Time = time_vect, psn_vector2)

head(psn_vector2)

tail(psn_vector2)



# Rename columns: p_t which is position at time t.

for (i in 1:num_sim){
  colnames(psn_vector2)[i + 1] <- paste0("Path_", i)
}

# Remove row names, have them blank

rownames(psn_vector2) <- NULL


# Convert to data.frame for gather().

psn_vector2 <- data.frame(psn_vector2)

# Wide Format To Long Format Data:

library(tidyr)
library(dplyr)

psn_vector2 <- psn_vector2 %>% gather(Path, Position, -Time)

# Check:

head(psn_vector2)
tail(psn_vector2)

dim(psn_vector2)

# ggplot2 Plot: 

# Ggplot with time on horizontal axis, and running total/position on vertical axis.

ggplot(psn_vector2, aes(x = Time, y = Position, group = Path)) + 
  geom_line() +
  labs(x = "\n Time (Seconds)", y = "Position \n", 
       title = paste0("Brownian Motion Paths With ", length(time_vect) - 1, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# If I want different colours for each path I would add colour = Path in aes().

ggplot(psn_vector2, aes(x = Time, y = Position, group = Path, colour = Path)) + 
  geom_line() +
  labs(x = "\n Time (Seconds)", y = "Position \n", 
       title = paste0("Brownian Motion Paths With ", length(time_vect) - 1, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")