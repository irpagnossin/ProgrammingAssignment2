## Functions below enhances dealing with the inverse of large matrices:
## it wraps a given square matrix and its inverse, lazily evaluated, in
## a structure composed of two getters and two setters:
## Example:
##
## x <- matrix(rnorm(100), 10, 10)
## structured_x <- makeCacheMatrix(x)
## structured_x$getinv()

## It wrapps a given matrix x in a structure
## which caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # TODO: check if x is square
    
    inv_x <- NULL
    
    # Getters
    get <- function() x
    getinv <- function() inv_x
    
    # Setters
    set <- function(new_x) {
        x <<- new_x
        inv_x <<- NULL
    }
    setinv <- function(inv) inv_x <<- inv
    
    # Exposed elements of this structure
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}

## Return the inverse of structured matrix x,
## either cached or determined on demand.
cacheSolve <- function(x, ...) {

    # It retrieves cached inverse
    inv_x <- x$getinv()
    
    # If no cached inverse exists, calculates it from x and cache it
    if(is.null(inv_x)) {
        inv_x <- solve(x$get())
        x$setinv(inv_x)
    }
    
    inv_x
}