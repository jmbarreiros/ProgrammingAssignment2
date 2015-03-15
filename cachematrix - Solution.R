# The objective of these functions is to cache the inverse of a matrix in order to avoid
# computing the inverse for the same matrix repeatedly


## The function makeCacheMatrix creates a a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## The function cacheSolve checks if the inverse has already been computed and if so 
## it prints a message saying the requested data is cached and returns the inverse. If the inverse 
## for that matrix hasn't been computed before, it computes the inverse and saves that matrix
## in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
