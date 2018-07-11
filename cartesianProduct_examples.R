# Cartesian Products In R

# Example One:

P <- c(1, 2)
Q <- c("a", "b", "c")

# Cartesian Products (Table Form):
# First column from the first vector, second column from second vector.

prod1 <- expand.grid(P, Q)
prod1

prod2 <- expand.grid(Q, P)
prod2

prod3 <- expand.grid(P, P)
prod3

# Number of ordered pairs or elements in the Cartesian product for product4:
nrow(prod3)

##-------------------------------------------

# Example Two:

fruits <- c("Banana", "Orange", "Mango")
animals <- c("Dog", "Cat", "Panda", "Elephant")

product1 <- expand.grid(fruits, animals)
product1

product2 <- expand.grid(animals, fruits)
product2

product3 <- expand.grid(fruits, fruits)
product3

product4 <- expand.grid(animals, animals)
product4

# Number of ordered pairs or elements in the Cartesian product for product4:
nrow(product4)

