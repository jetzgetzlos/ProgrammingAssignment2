## Caching the Inverse of a Matrix, March 2017

## The makeCacheMatrix function creates a special "matrix", with inverse caching 
## 'matrix' : initial matrix, source of the special matrix
## return : a special matrix built from the inital matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        ## x is changed : we must erase the cached inverse
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## This function cacheSolve computes the inverse of a special "matrix" created by 
## makeCacheMatrix. If the inverse has not been computed yet, the inverse is
## computed and cached. If the inverse has already been computed (and if the matrix
## is the same, without changes), the inverse is not calculed but retrieved from the
## cache
## 'x' : a special matrix created by makeCacheMatrix
## Return a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        # Inverse is already computed and cached
        message("getting cached data")
        return(inverse)
    }
    x_matrix <- x$get()
    #compute the inverse
    inverse <- solve(x_matrix)
    x$setInverse(inverse)
    inverse
}
