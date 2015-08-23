## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
set <- function(y) {
      x <<- y
      m <<- NULL
}
get <- function() x
setinv <- function(solve) m <<- solve
getinv <- function() m
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cachesolve <- function(x, ...) { 
    i<- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m<- solve(data, ...)
    x$setinv(m)
    m

}

