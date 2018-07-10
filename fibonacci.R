# Fibonacci Sequence in R

# First two numbers are 1 and 1 (some have 0 and 1)
# Next number is adding the previous two numbers together.
# 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, and so on. 

# A Function Which ouptuts the n-th Fibonacci number (n >= 1 & n is natural number):

fibonacci_funct <- function(n) {
  
  if (n < 1){
    print("Please choose a whole number that is 1 or greater")
  } else if (n == 1) {
    return(print("The first Fibonacci number is 1."))
  } else if (n == 2) {
    return(print("The second Fibonacci number is 2."))
  } else if ((n >= 3) && (n %% 1 == 0)){
    
    fibonacci_vect = c() #Initalize empty vector
    
    # Initalize first two numbers in Fibonacci sequence:
    
    fibonacci_vect[1] <- 1 
    
    fibonacci_vect[2] <- 1
    
    for (i in 3:n){
      fibonacci_vect[i] =  fibonacci_vect[i - 2] + fibonacci_vect[i - 1]
    }
    
    # Create output message with paste0() concatenation:
    
    msg <- paste0("Position: ", n, ", Fibonacci Number: ", fibonacci_vect[n])
    
    return(msg) # Output n-th Fibonacci number as a message
  } else # Non-whole number case.
    print("Invalid number. Please enter in a whole number that is 1 or greater.")
}

## Test Cases With Function Calls:

fibonacci_funct(2)

fibonacci_funct(-1)

fibonacci_funct(1)

fibonacci_funct(0)

fibonacci_funct(2.4)

fibonacci_funct(3)

fibonacci_funct(6)

fibonacci_funct(10)


