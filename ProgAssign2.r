## THE PROMPT - THE ASSIGNMENT WORK IS BELOW
# Next two functions are examples provided in the class.
# In this example we introduce the <<- operator which can be used to assign a value 
# to an object in an environment that is different from the current environment. 
# Below are two functions that are used to create a special object that stores a 
# numeric vector and cache's its mean.

# The first function, makeVector creates a special "vector", which is really a 
# list containing a function to:

# - set the value of the vector
# - get the value of the vector
# - set the value of the mean
# - get the value of the mean

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

# The following function calculates the mean of the special "vector" created 
# with the above function. However, it first checks to see if the mean has 
# already been calculated. If so, it gets the mean from the cache and skips 
# the computation. Otherwise, it calculates the mean of the data and sets the 
# value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## THE ASSIGNMENT
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## The assignment is to write a pair of functions that cache the inverse of a matrix.

# The first function, makeCacheMatrix creates a random 2x square matrix, which:
#  - sets the value of the random matrix
#  - gets the value of the random matrix
#  - sets the value of the inverse matrix
#  - gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        set_invMat <- function(inverse) invMat <<- inverse
        get_invMat <- function() invMat
        list(set = set, 
             get = get,
             set_invMat = set_invMat,
             get_invMat = get_invMat)
}

## The following function calculates the inverse matrix created with the above function. 
## However, it first checks to see if the matrix has already been inverted. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix and sets the value of the matrix in the cache 
## via the set_invMat function.

cacheSolve <- function(x, ...) {
        invMat <- x$get_invMat()
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        mat <- x$get()
        invMat <- solve(mat, ...)
        x$set_invMat(invMat)
        invMat
}
