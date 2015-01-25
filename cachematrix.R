## Matrix inversion functions with caching.

#1234567890123456789012345678901234567890123456789012345678901234567890123456789
## This function creates a CacheMatrix object from a matrix. The CacheMatrix 
## object is iverted using the cacheSolve() function. The inversion result is 
## cached inside the object when it is first computed. The cached result is 
## returned on subsequent inversion.
## Usage:
## x <- matrix(rnorm(5*5),5,5)
## cx <- makeCacheMatrix(x)
## print(cx$get())

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # access the underlying matrix object
    get <- function() {
        x
    }
    
    setinv <- function(newinv) {
        inv <<- newinv
    }
    
    getinv <- function() {
        inv
    }
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}
#1234567890123456789012345678901234567890123456789012345678901234567890123456789

## This function computes the inverse of a CacheMatrix object. The inverse is 
## stored inside the object and is retreived on subsequent calls to the same
## object.
## Usage:
## xinv = cacheSolve(cx)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
