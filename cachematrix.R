## Put comments here that give an overall description of what your
## functions do
# Calculating the inverse of a matrix could be computationally
# intensive potentially requiring large amounts of memory. 
# These set of functions provide one main purpose. Assuming
# that the same matrix will be used in a loop requiring
# the inverse to be calculated, this R script will allow the
# programmer to cache the inverse calculation instead of having
# to recalculated each time. It is almost like it  is creating 
# a global envirnoment variable whose value can be used 
# throughout the script until a new matrix inverse calculation
# is made by calling the two function in oder from this script again.

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix
# The function takes in a matrix as its input
# Initializes a new variable m assigning it a null value

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
	     setsolve = setsolve,
         getsolve = getsolve)
}

## Write a short comment describing this function
# The following function calculates the inverse of the special 
# "matirx" created with the above function. However, it first 
# checks to see if the inverse has already been calculated. If so, 
# it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the input matirx and sets 
# the value of the inverse matrix in the cache via the setsolve 
# function. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}