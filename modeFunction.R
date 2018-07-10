# Computing mode in R:

# Found online 
# Link: https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode

names(sort(-table(x))[1])

## Function:

getMode <- function(x) {
  names(sort(table(x), decreasing = TRUE)[1])
}

## -----------------
# Test Cases :

x <- c(1, 3, 3, 5, 9, 11, 2, 4)

foods <- c("pizza", "salad", "pasta", "pasta", "sushi", "KFC", "pasta")

samplePoissons <- rpois(100, lambda = 1)

getMode(x); getMode(foods); getMode(samplePoissons)
