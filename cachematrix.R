## Set of functions show how to use <<- for setting the value of an object
## that is not in current environment. Here its used as a caching mechanism

## This function returns the list of getter and setter functions
## of the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(solvedMatrixInverse) matrixInverse <<- solvedMatrixInverse
    getMatrixInverse <- function() matrixInverse
    
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## Checks if the the inverse exists in the cache. If yes returns cached value
## If not, calculates the inverse, set the value and then returns the same

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrixInverse <- x$getMatrixInverse()
    if(is.null(matrixInverse)){
        #message("calculating the inverse");
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setMatrixInverse(inverse)
        return(inverse)
    }
    return(matrixInverse)
}
