## makeCacheMatrix is a function that generates a list of functions that
## cache the inverse of a matrix.
## In order to cache the inverse of a matrix: 
##      1.  set the matrix
##      2.  get the matrix
##      3.  set the inverse of the matrix
##      4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
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


## cacheSolve function returns the inverse of the matrix 
## created by makeCacheMatrix(). 
## In case it has already been calculated, 
## it retrieves it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}