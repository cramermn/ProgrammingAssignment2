# Functions that cache the inverse of a matrix
# Usage example:
#
# source('cachematrix.R')
# m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))

# cacheSolve(m)
# [,1] [,2]
# [1,]  0.5  0.0
# [2,]  0.0  0.5

# Create a special "matrix", which is a list containing a function to
#   - set the value of the matrix
#   - get the value of the matrix
#   - set the value of the inverse matrix
#   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
                            # This function creates a special "matrix" object 
                            # that can cache its inverse.

                            # Purpose: Define function to set the value of the 
                            # matrix
    
    m <- NULL               # clears the cache
        set <- function(y) {
        x <<- y             # Set the value
        m <<- NULL          # Clear the cache
        }
    get <- function() x     # Define function to set the inverse
                            # This is only used by getinverse() when 
                            # there is no cached inverse
    
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
                            # Define function to get the inverse
    
    list(set = set, get = get, # Return a list with the above four functions
         setInverse = setInverse,
         getInverse = getInverse)
    }

# Calculate the inverse of the special "matrix" created with the above
# function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
                            # This function computes the inverse of the special 
                            # "matrix" returned by makeCacheMatrix above. If 
                            # the inverse has already been calculated (and the 
                            # matrix has not changed), then the cachesolve 
                            # should retrieve the inverse from the cache. 
    
    m <- x$getInverse()     # This fetches the cached value for the inverse
    if(!is.null(m)) {       # If the cache was not empty, we can just return it

        message("getting cached data")
        return(m)
        }
    data <- x$get()         # iF The cache was empty. We need to calculate it, 
                            # cache it, and then return it.
                            # Get value of matrix
    
    m <- solve(data)        # Calculate inverse
    x$setInverse(m)         # Cache the result
    m                       # Return the inverse
    }

#---------------------------------------------------------------------------
# Example: Caching the Mean of a Vector  |
#---------------------------------------------------------------------------

# In this example we introduce the <<- operator which can be used to assign
# a value to an object in an environment that is different from the current
# environment. Below are two functions that are used to create a special 
# object that stores a numeric vector and caches its mean.

# The first function, makeVector creates a special "vector", which is 
# really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

# makeVector <- function(x = numeric()) {
#   m <- NULL
#     set <- function(y) {
#         x <<- y
#         m <<- NULL
#     }
#     get <- function() x
#     setmean <- function(mean) m <<- mean
#     getmean <- function() m
#     list(set = set, get = get,
#          setmean = setmean,
#          getmean = getmean)
# }
# The following function calculates the mean of the special "vector" created 
# with the above function. However, it first checks to see if the mean has 
# already been calculated. If so, it gets the mean from the cache and skips 
# the computation. Otherwise, it calculates the mean of the data and sets 
# the value of the mean in the cache via the setmean function.
# 
# cachemean <- function(x, ...) {
# m <- x$getmean()
# if(!is.null(m)) {
#     message("getting cached data")
#    return(m)
# }

# data <- x$get()
# m <- mean(data, ...)
# x$setmean(m)
# m
# } 
