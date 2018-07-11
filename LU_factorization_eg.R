### LU Factorization in R:

## Example:

## Initialize matrices and column vectors

matrix1 <- matrix(c(1, -2, 1, 3, -1, 0, 1, 4, 1), nrow = 3, ncol = 3, byrow = TRUE)

matrix1

b1 <- matrix(c(2, 1, 0), nrow = 3, ncol = 1, byrow = TRUE)

b1

# Source: http://stackoverflow.com/questions/26377199/convert-a-matrix-in-r-into-a-upper-triangular-lower-triangular-matrix-with-those

l1 <- matrix1
l1[upper.tri(l1)] <- 0

l1
  
u1 <- matrix1
u1[lower.tri(u1)] <- 0
  
u1

## ---------------------------------

# Solve the linear systems:

# Solve Ly = b then Ux = y. We have LUx = b

y1 <- solve(l1, b1)

y1

x1 <- solve(u1, y1)

x1

# Check:

l1 %*% u1 %*% x1

# Compare:

solve(matrix1, b1)

matrix1 %*% solve(matrix1, b1)


### Using the Matrix package:

library(Matrix)

lu(matrix1)