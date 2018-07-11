# Dot Product, Norm, Distances in R:

## Norms Examples:

# a)

a <- c(-3, 4)

# Two ways of computing a norm:

norm(a, type = "2")

sqrt(sum(a^2))

# b)

b <- c(5, 1, 10, -2)

norm(b, type = "2")

sqrt(sum(b^2))

#-----------------------------------
### Distance Between Two Vectors:

v <- c(10, 5, -2, -1)
w <- c(-1, 0, 2, 1)

# Three ways of computing distance between vectors in R:

sqrt(sum((v - w)^2))

dist(rbind(v, w))

norm(v - w, type = "2")

# Source: http://stackoverflow.com/questions/5559384/euclidean-distance-of-two-vectors
# Source: http://stackoverflow.com/questions/10933945/how-to-calculate-the-euclidean-norm-of-a-vector-in-r

#-----------------------------------
## Dot Product:

# Assuming equal lengths of vector the multiplicationn operator in R does
# element wise multiplication (i.e. a1 * b1).

# Example 1:

a <- c(1, -2)
b <- c(-10, -3)

sum(a * b)

# Example 2:

c <- c(1, -2, 5, 3)
d <- c(4, -3, -7, 2)

sum(c * d)

# Example 3: (e + g) * f

e <- c(3, -5)
f <- c(-1, 4)
g <- c(3, 2)

sum((e + g) * f)