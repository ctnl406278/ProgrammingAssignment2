## The overall function of these two functions is to save time and resources when calculating
## the inverse of a matrix (assuming it is invertable). It does so by retrieving it from 
## memory or calculating it from scratch. 

## The makeCacheMatrix function receives a variable as a matrix. It contains 4 functions 
## that either stores (set, setInverse) or returns (get, getInverse) the matrix variable 
## and its inverse.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    setInverse <- function(inverse)
        m <<- inverse
    getInverse <- function()
        m
    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## The cacheSolve function returns the inverse of the matrix by first verifying its existance. 
## It is either retreived from memory or calculated from scratch.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
