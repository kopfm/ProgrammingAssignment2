## This pair of functions can cache the inverse of a matrix.

## makeCacheMatrix creates a special matrix object that can cache its inverse.
## If a new matrix is inserted with, the cache is resetted.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL ## initializes inverse with NULL
        get <- function() x ## get the original matrix
        set <- function(y) { ## insert a new matrix
                x <<- y ## assign new matrix to x
                inverse <<- NULL ## assign NULL to inverse -> resets the cache!
        }
        setInverse <- function(inv) inverse <<- inv ## cache inversed matrix
        getInverse <- function() inverse ## return value of inverse
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse) ## return special matrix
}

## chacheSolve returns the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse was already computed 
## (and the matrix has not changed), cacheSolve retrieves the inverse from
## the cache.

cacheSolve <- function(x) {
        m <- x$getInverse() ## get value of inverse
        if(!is.null(m)) { ## inverted matrix is already in cache
                message("getting cached data")
                return(m) ## cached inverted matrix is returned
        }
        data <- x$get() ## get original matrix
        m <- solve(data) ## invert matrix
        x$setInverse(m) ## cache inverted matrix
        m ## return inverted matrix
}