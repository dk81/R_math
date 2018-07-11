# Random Walk Plot In R With ggplot2

# Resouce: https://stackoverflow.com/questions/21991130/simulating-a-random-walk

# The random walk is a running total/sum based on a random variable
# that is +1 or -1 depending on the outcome.

# In here, we deal with a symmetric random walk where the expected value of the running total/sum
# is zero and the random variable has an equal chance of being +1 or -1.

library(ggplot2)

# Random Walk Function:
# Input: Time in seconds; Output: The sequence of sums at each time point with M_0 = 0.

random_walk <- function(t = 1){
  if (t <= 0 | t %% 1 != 0){
    print("Please enter a whole number that is 1 or greater.")
  } else {
  
  # Initialize
  time_vect <- 0:t
  sum_vect <- c(0) 
  
  # Remember that M_0 = 0 meaning that the sum at t = 0 is 0
  for (i in 1:t){
    sum_vect[i + 1] = sum_vect[i] + sample(x = c(-1, 1), size = 1, prob = c(0.5, 0.5))
  }
  
  return(sum_vect)
}
}

time = 5

rw5 <- random_walk(5)

data_rw5 <- data.frame(0:5, rw5)

data_rw5

colnames(data_rw5) <- c("t", "Position") #Rename columns

data_rw5

# Ggplot with time on horizontal axis, and running total on vertical axis.

ggplot(data_rw5, aes(x = t, y = Position)) + 
  geom_point() +
  geom_line() +
  labs(x = "\n t", y = "Position \n", title = paste0("Random Walk With ", time, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dotted")

#----------------

# Using the cumsum() function to generate random walk more efficently:

time <- 5
  
rw_psn <- c(0, cumsum(sample(x = c(-1, 1), size = time, prob = c(0.5, 0.5), replace = TRUE)))

rwalk_data5 <- data.frame(0:5, rw_psn)

rwalk_data5

colnames(rwalk_data5) <- c("t", "Position")

# Ggplot with time on horizontal axis, and running total on vertical axis.

ggplot(rwalk_data5, aes(x = t, y = Position)) + 
  geom_point() +
  geom_line() +
  labs(x = "\n t", y = "Position \n", title = paste0("Random Walk With ", time, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dotted")


#-----------------

## Making It As A Random Walk Plotting Function:
# Input: Time in seconds; Output: The sequence of sums at each time point with M_0 = 0.


random_walk_plot <- function(t = 1, col_line = "black"){
  if (t <= 0 | t %% 1 != 0){
    print("Please enter a whole number that is 1 or greater.")
  } else {
    
    # Random Walk Steps:
    psn_vector <- c(0, cumsum(sample(x = c(-1, 1), size = t, prob = c(0.5, 0.5), replace = TRUE)))
    
    # Put results into a data frame:
    rwalk_df <- data.frame(0:t, psn_vector)
    
    colnames(rwalk_df) <- c("t", "Position") #Rename columns
    
    # Ggplot with time on horizontal axis, and running total on vertical axis.
    
    ggplot(rwalk_df, aes(x = t, y = Position)) + 
      geom_point() +
      geom_line(colour = col_line) +
      labs(x = "\n t", y = "Position \n", title = paste0("Random Walk With ", t, " Steps \n")) + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.title.x = element_text(face="bold", colour="blue", size = 12),
            axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
      geom_hline(yintercept = 0, colour = "red", linetype = "dotted")
    
  }
}


# Function calls:

random_walk_plot(t = 20, col_line = "green")


#--------------------

# Multiple Random Walk Paths:

# Resource: http://www.phytools.org/eqg/Exercise_4.1/
# https://stackoverflow.com/questions/17150183/plot-multiple-lines-in-one-graph

library(ggplot2)

# Using the cumsum() function to generate random walk more efficently:

time <- 100
num_paths <- 8

rw_paths <- matrix(data = 0, nrow = num_paths, ncol = time + 1)

# Fill rows with each random walk path realization:

for (i in 1:num_paths){
   rw_paths[i, ] <- c(0, cumsum(sample(x = c(-1, 1), size = time, prob = c(0.5, 0.5), replace = TRUE)))
}

rw_paths <- data.frame(rw_paths)

# Check:

head(rw_paths)

# Transpose and add times column (Transpose = switch rows with columns and viceversa):

rw_paths_t <- cbind(Time = 0:time, t(rw_paths))

head(rw_paths_t)

# Rename columns: Each column represents the path and the values are the positions at time t:

for (i in 1:num_paths){
  colnames(rw_paths_t)[i + 1] <- paste0("Path_", i)
}

# Remove row names, have them blank

rownames(rw_paths_t) <- NULL


# Convert to data.frame for gather().

rw_paths_t <- data.frame(rw_paths_t)

# Check:

head(rw_paths_t)

# Wide Format To Long Format Data:

library(tidyr)
library(dplyr)

rw_paths2 <- rw_paths_t %>% gather(Path, Position, -Time)

# Check:

head(rw_paths2)

# Ggplot with time on horizontal axis, and running total/position on vertical axis.

ggplot(rw_paths2, aes(x = Time, y = Position, group = Path)) + 
  geom_line() +
  labs(x = "\n Time (Seconds)", y = "Position \n", title = paste0("Random Walk Paths With ", time, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# If I want different colours for each path I would add colour = Path in aes().

ggplot(rw_paths2, aes(x = Time, y = Position, group = Path, colour = Path)) + 
  geom_line() +
  labs(x = "\n Time (Seconds)", y = "Position \n", title = paste0("Random Walk Paths With ", time, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")


# This code can be put into a function as well.


#-----------------------------

# Replicate example: replicate(n = 10, runif(n = 8))

# Replicate function version:

library(ggplot2)

# Using the cumsum() function to generate random walk more efficently:

time <- 100
num_paths <- 8

rw_paths <- matrix(data = 0, nrow = time + 1, ncol = num_paths)

# Fill rows with each random walk path realization with replicate():


rw_paths <- replicate(num_paths, c(0, cumsum(sample(x = c(-1, 1), size = time, prob = c(0.5, 0.5), replace = TRUE))))


rw_paths <- data.frame(rw_paths)

# Check:

head(rw_paths)

# Add times column (No need to transpose here):

rw_paths <- cbind(Time = 0:time, rw_paths)

head(rw_paths)

# Rename columns: p_t which is position at time t.

for (i in 1:num_paths){
  colnames(rw_paths)[i + 1] <- paste0("Path_", i)
}

# Remove row names, have them blank

rownames(rw_paths) <- NULL


# Convert to data.frame for gather().

rw_paths <- data.frame(rw_paths)

# Wide Format To Long Format Data:

library(tidyr)
library(dplyr)

rw_paths2 <- rw_paths %>% gather(Path, Position, -Time)

# Check:

head(rw_paths2)
tail(rw_paths2)

dim(rw_paths2)

# Ggplot with time on horizontal axis, and running total/position on vertical axis.

ggplot(rw_paths2, aes(x = Time, y = Position, group = Path)) + 
  geom_line() +
  labs(x = "\n Time (Seconds)", y = "Position \n", title = paste0("Random Walk Paths With ", time, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# If I want different colours for each path I would add colour = Path in aes().

ggplot(rw_paths2, aes(x = Time, y = Position, group = Path, colour = Path)) + 
  geom_line() +
  labs(x = "\n Time (Seconds)", y = "Position \n", title = paste0("Random Walk Paths With ", time, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

#-----------------------------

## Two Dimensional Random Walk (Maybe Another Time):

# Not sure about this code.

# Start at origin (0, 0) i.e. X_0 = 0 and Y_0 = 0 at time t = 0.

time <- 100

# Random Walk Steps for x and y:

rw_psn_x <- c(0, cumsum(sample(x = c(-1, 1), size = time, prob = c(0.5, 0.5), replace = TRUE)))
rw_psn_y <- c(0, cumsum(sample(x = c(-1, 1), size = time, prob = c(0.5, 0.5), replace = TRUE)))

rwalk_data100 <- data.frame(0:time, rw_psn_x, rw_psn_y)

colnames(rwalk_data100) <- c("t", "Position_x", "Position_y")

rwalk_data100

# Ggplot with time on horizontal axis, and running total on vertical axis.

ggplot(rwalk_data100, aes(x = Position_x, y = Position_y)) + 
  geom_path() +
  labs(x = "\n x Co-Ordinate", y = "y Co-Ordinate \n", title = paste0("Random Walk With ", time, " Steps \n")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dotted")
