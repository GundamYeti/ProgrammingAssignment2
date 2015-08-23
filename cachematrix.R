## Put comments here that give an overall description of what your
## functions do
## Per the assignment instructions makeCacheMatrix creates a blank matrix that can cache it's inverse and cacheSolve either
## computes the inverse or simply retrives the already solved function from the cache.

## Write a short comment describing this function
## "this function creates a special "matrix" object that can cache its inverse."
## First it sets the value of the matrix then it get the value of the matrix then it
## sets the value of the inverse of the matrix then it gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
## Per the assignment, cacheSolve "computes the inverse of the special "matrix" returned by makeCacheMatrix above."
##If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. It checks if
## the inverse is already there. If yes, it gets the result
## If not, it find the inverse and sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
