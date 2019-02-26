## The function makeCacheMatrix creates a list of 4 functions:

## 1. set: sets the value of the matrix
## 2. get: gets the value of the matrix
## 3. setinverse: sets the value of the inverse
## 4. getinverse: gets the value of the inverse

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


## The function cacheSolve computes the inverse of the special "matrix" 
## created with the function makeCacheMatrix. It first checks to see if 
## the inverse has already been computed. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the setinverse 
## function.

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
