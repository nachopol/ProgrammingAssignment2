## The following pair of functions allow to cache the inverse of a square matrix

## makeCacheMatrix create a special 'matrix' capable to set and retrieve
## both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    # intialization of inverse variable
    i <- NULL
    # "set function" which updates the matrix and initilizes its inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # "get function" which retrieves the matrix
    get <- function() x
    # "setinverse function" which sets the inverse with the data provided
    setinverse <- function(inverse) i <<- inverse
    # "getinverse function" which returns the inverse of the matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of the x matrix when created with makeCacheMatrix.
## If the inverse was already calculated and the matrix data has not change it
## returns the cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    ## If it's not null, it was previously calculated so it's retrieved from "cache"
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## If it was null, the inverse is :
    ## 1 - calculated from the matrix data
    ## 2 - cached in the x object
    ## 3 - returned
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
