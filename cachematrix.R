##  Catching the Inverse of the Matrix

## The first function, makeCacheMatrix creates a special "matrix",
## list containing a function to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

     invrs <- NULL

     #1
     set <- function(matrix){
          m <<- matrix
          invrs <<- NULL
     }

     #2
     get <- function(){
          m
     }

     #3
     setinverse <- function(inverse){
          invrs <<- inverse
     }

     #4
     getinverse <- function(){
          invrs
     }

     list(set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


## This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

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
}
