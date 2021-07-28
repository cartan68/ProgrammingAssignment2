## Function makeCacheMatrix provides the ability to cache a computed matrix.
## If the matrix is unlikely to change, the computation can be done once and
## the results can be saved for future use, saving the time required to
## recompute the matrix.
##
## Function cacheSolve takes a matrix and returns the inverse of the matrix.
## If the matrix inverse has been cached, the cached inverse is returned.
## Otherwise, the matrix inverse is computed, cached, and returned


## makeCacheMatrix
##
## This function is used to cache a matrix and its inverse.
##
## Arguments:
##      x: A matrix to be stored. The default is an empty matrix.
##
## Return:
##      A list of functions:
##          - get: a function that returns the stored data matrix
##          - set: a function that stores the data matrix
##          - getmean: a function that returns the cached mean of
##                  the stored data matrix
##          - setmean: a function that stores in cache the mean of
##                  the stored data matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(matrixinverse) m <<- matrixinverse
    getinverse <- function() m

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve
##
## This function returns the inverse of matrix 'x'. If the inverse already
## exists, the cached inverse matrix is returned. If the inverse has not yet
## been calculated, the inverse is calculated, cached, and returned.
##
## Arguments:
##      x: A matrix for which an inverse is requested
##      ...: Additional arguments to be sent to the solve function
##
## Return:
##      A matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getinverse()

    # If a cached inverse does not exist, compute it
    if (is.null(m)) {
        message("computing inverse")
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
    }
    return(m)
}
