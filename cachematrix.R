## These functions are meant to provide the ability for a 
##     matrix to cache an inverse of itself.

## makeCacheMatrix is a wrapper around the native matrix which
##     caches its own inverse.
## There are four functions on the cacheMatrix,
##     1. get the value of the matrix
##     2. set the value of the matrix
##     3. get the value of the inverse of the matrix
##     4. set the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inv_m) inv <<- inv_m
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This method computeds the inverse of the "cache Matrix" created
##     First it checks if the matrix inverse is already calculated,
##     and that the matrix hasn't changed (If the matrix is changed,
##     then the inv is set to NULL.) If the inverse was calculated,
##     return it. Otherwise compute it.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    # Check that the inverse exists on the "cache Matrix"
    if(!is.null(inv)){
        print("getting cached data")
        return(inv) 
    }
    # Inverse was not created, so get the matrix, invert it, and
    #     populate the cache. Then return the inverted matrix.
    mx <- x$get()
    inv <- solve(mx)
    x$setInv(inv)
    inv
}
