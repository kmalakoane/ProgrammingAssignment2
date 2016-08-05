## A pair of functions that cache the inverse of a matrix.

## Creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize variable to store inversed matrix
    m <- NULL

    ## Method to set the matrix
    set <- function(y) {
            x <<- y
            m <<- NULL
    }

    ## Method to get the matrix
    get <- function() x

    ## Mehod to set the inverse of a matrix
    setInverse <- function(inverse) m <<- inverse

    ## Mehod to get the inverse of a matrix
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Calculates the inverse of the matrix created with the "makeCacheMatrix" function above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if it's already set
    if(!is.null(m)) {
        message("getting cached inverse of the matrix")
        return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data)

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the inverse of the original matrix
    m
}
