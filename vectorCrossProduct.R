# Vector Cross Product in R:

# Reference:
# http://stackoverflow.com/questions/36798301/r-compute-cross-product-of-vectors-physics
# last comment

install.packages("pracma") 

require("pracma")
#cross(v1,v2)

# Example:

# Finding the cross product of the two vectors
# u = (2, 0, -8) and v = (0, 0, 1)

u <- c(2, 0, -8)
v <- c(0, 0, 1)

crossProd1 <- cross(u, v)
crossProd1

# Checking that the cross product is perpendicular/orthogonal to 
# vectors u and v.
# If dot product is zero, two vectors are orthogonal (90 degrees) to each other.

sum(u * crossProd1)

sum(v * crossProd1)
