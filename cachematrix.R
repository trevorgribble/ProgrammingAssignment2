## makeCacheMatrix & cacheSolve work in tandem to calculate the inverse of a matrix
## and store it in the cache so that when we need it again, it can be looked up 
## in the cache rather than recomputed

## makeCacheMatrix accepts a square invertible matrix x as it's argument
## and creates a list of 4 functions as its output
## set - sets the value of the original input matrix
## get - returns the original input matrix
## setmatrixinverse - stores the value of the inverse of the matrix
## getmatrixinverse - returns the inverse of the matrix if it is already in cache
##                    (or NULL if it has yet to be calculated)

makeCacheMatrix <- function(x = matrix()) {
        matrixinverse <- NULL
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        get <- function() x
        setmatrixinverse <- function(inverse) matrixinverse <<- inverse
        getmatrixinverse <- function() matrixinverse
        list(set = set, get = get,
             setmatrixinverse = setmatrixinverse,
             getmatrixinverse = getmatrixinverse)
}


## makeCacheMatrix accepts the list created from makeCacheMatrix as its argument
## it first checks to see if the matrix inverse has already been calculated and stored in cache
## if it has been stored already, it reports that it is getting cached data and then
## returns that inverted matrix from cache
## if it has yet to be stored, it gets the original input matrix, calculates the inverse
## and sets the inverse into cache, returning the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixinverse <- x$getmatrixinverse()
        if(!is.null(matrixinverse)) {
                message("getting cached data")
                return(matrixinverse)
        }
        data <- x$get()
        matrixinverse <- solve(data, ...)
        x$setmatrixinverse(matrixinverse)
        matrixinverse
}
