## The two following functions can be used together to compute the inverse of an 
## invertible matrix, caching the value of inverse so that when we need it again,
## it can be looked up in the cache rather than recomputed.

## This function takes a matrix as input and returns an object (a list of 4 
## functions) tailor-made for the input, that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # The input, x, is an invertible matrix.
        # We will assume x is indeed invertible and we will not test it here.
        inv <- NULL
        # The 1st function stores the input matrix in the external environment 
        # for future uses:
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # The 2nd function retrieves the value of the matrix we want to invert:
        get <- function() x
        # The 3rd function stores the inverse in the external environment 
        # for future uses:
        setinverse <- function(solve) inv <<- solve
        # The 4th function retrieves the value of the inverse:
        getinverse <- function() inv
        # Return a list of the 4 functions above:
        list(set = set, get = get,
                                setinverse = setinverse,
                                getinverse = getinverse)
        
}


## This function computes the inverse of the special object returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        # The input variable, x, here means a list of 4 functions: 
        # set(y), get(), setmean(y), getmean().
        
        # If there exists a precalculated inverse, it will return that.
        # x$getinverse() returns NULL the first time we call cacheSolve,
        # but the next time we call cacheSolve, it will return the inverse that
        # was previously calculated.
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # Let's retrieve the matrix that has to be inverted and store it in "data":
        data <- x$get()
        # Now we will calculate its inverse:
        inv <- solve(data, ...)
        # And we will store the inverse in the external environment for future uses:
        x$setinverse(inv)
        # Last, return the matrix that is the inverse of 'x' (= the original
        # matrix we used as input for the makeCacheMatrix function)
        inv
        
}
   

