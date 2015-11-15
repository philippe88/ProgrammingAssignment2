## Implement a way to cache the inverse of a matrix.
##      makeCacheMatrix: contains a matrix and its inverse.
##           cacheSolve: calculates the inverse of a CacheMatrix, 
##                       retrieving it from cache if available.


# This function creates a CacheMatrix object (a list). 
# It contains a matrix, and its inverse (NULL during initial creation)
# It returns functions to set/get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    # hold the inverse of x, always set to NULL at creation
    # use 'setinverse' function to set, from cacheSolve
    i <- NULL
    
    # replace the matrix with a new one
    # use '<<-' i because x and i are in the parent environment
    set <- function(y) {
        x <<- y
        # set the inverse to NULL at creation 
        i <<- NULL
    }
    # return the matrix
    get <- function() x
    
    # set the inverse of the matix, meant to be called from cacheSolve
    setinverse <- function(inv) i <<- inv
    
    # return the inverse of the matrix
    getinverse <- function() i
    
    # return the final object with the 4 accessor functions as a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## This function calculates the inverse of a CacheMatrix 
## Retrieve the inverse of matrix 'x' from cache if available.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
       
    # check if the inverse is already available
    # if it is, retuns that value
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # if the inverse is not available,
    # fetch the matrix
    data <- x$get()
    # calculate the inverse,
    i <- solve(data, ...)
    # Store it in the CacheMarix
    x$setinverse(i)
    # and retun the value
    i
    
}
