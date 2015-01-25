## makeCacheMatrix is a function to bundle a matrix with its calcuated inverse
## CacheSolve is a function to return the inverse of a matrix

## makeCacheMatrix creates a special list of a matrix and its inverse, which is a function to
## set the value of the list, get the value of the list, set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function to return the inverse of the matrix x

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
}
