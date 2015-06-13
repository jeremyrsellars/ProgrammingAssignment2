# CacheMatrix

## This library calculates the inverse of a matrix,
## caching the result so that the result is only
## calculated once.  The object can be safely re-used
## with other matrices, by using the set method.

## Example usage:
### m <- matrix(c(1,7,11,18,2,3,4,29,47,13,243,24,332,23,34,1), nrow=4, ncol=4)
### cm <- makeCacheMatrix(the_matrix)
### # These are the same.
### solve(m)
### cacheSolve(cm)
### m2 <- matrix(c(1,2,2,3), nrow=2, ncol=2)
### cm$set(m2)
### # These are the same.
### solve(m2)
### cacheSolve(cm)

# makeCacheMatrix
## Constructs a CacheMatrix object using the specified matrix.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setinverse <- function(solved) m <<- solved
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


# cacheSolve
## Return a matrix that is the inverse of 'x',
## caching the result so future calls return quickly.

cacheSolve <- function(x, ...) {
   m <- x$getinverse()
   if(!is.null(m)) {
           message("using cached solution")
           return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
