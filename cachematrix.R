## Functions below enhance dealing with inverse of large matrices:
## they wrap a given square matrix and its inverse, lazily evaluated, in
## a structure composed of two getters and two setters.
##
## Example:
## --------
## x <- matrix(rnorm(100), 10, 10)
## struct_x <- makeCacheMatrix(x)
## struct_x$getinv()    # Evaluates to NULL because inverse is not readly determined
## cacheSolve(struct_x) # Determines inverse of x and cache it into struct_x
## struct_x$getinv()    # Prints inverse of x
## struct_x$get()       # Prints x itself

## It wraps a given matrix x in a structure
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

## Returns the inverse of a strucuted x.
## If it had been calculated already, uses the cached value;
## if not, determines the inverse and caches it for future use.
cacheSolve <- function(x, ...) {

    # It retrieves cached inverse
    inv_x <- x$getinv()
    
    # If no cached inverse exists, calculates it from x and caches it
    if(is.null(inv_x)) {
        inv_x <- solve(x$get())
        x$setinv(inv_x)
    }
    
    inv_x
}