## Caching the Inverse of a Matrix:
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This process helps a lot in saving time.


## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        inv1 <- NULL
        set <- function(y) {
                x <<- y
                inv1 <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv1 <<- inverse
        getInverse <- function() inv1
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created by 
## makeCacheMatrix above. To match the required conditions specified in the questionIf the inverse has 
## already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv1 <- x$getInverse()
        if (!is.null(inv1)) {
                message("getting cached data")
                return(inv1)
        }
        mat <- x$get()
        inv1 <- solve(mat, ...)
        x$setInverse(inv1)
        inv1
}