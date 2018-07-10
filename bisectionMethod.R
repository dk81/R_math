### Bisection Method In R:

# References: http://www.dummies.com/programming/r/how-to-use-functions-as-arguments-in-r/
#             Tim Sauer - Numerical Analysis Second Edition


# Example One: xsquared - 7:

funct_1 <- function(x){
  return(x^2 - 7)
}


# Example Two: xsquared - 5:

funct_2 <- function(x){
  return(x^2 - 5)
}

# Example Three: exp(x) - 2:

funct_3 <- function(x){
  return(exp(x) - 2)
}

### Creating a bisection method function in R:

# Inputs: x, a and b in interval [a, b] and a function f.

bisection_method <- function(a, b, tol, f){
  if (f(a)*f(b) > 0){
    print("No root found.")
  } else
      while ((b - a)/2.0 > tol){
        midpt= (a + b)/2.0
        if (f(midpt) == 0){
          return(midpt) #The midpoint is the x-intercept/root.
        } else if (f(a)*f(midpt) < 0){ 
            b = midpt
        } else
            a = midpt
      }
      return(midpt)
}

# Function calls:
 
bisection_method(a = -1, b = 3, tol = 0.0001, funct_1) # approx sqrt(7)

bisection_method(a = -1, b = 3, tol = 0.0001, funct_2) # aprrox sqrt(5)

bisection_method(a = -1, b = 3, tol = 0.0001, funct_3) #ln(2) which is about 0.693