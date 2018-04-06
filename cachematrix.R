## These two functions compute and cache the inverse of a matrix.
## If a matrix' inverse has already been calculated, it will fetch its inverse from the cache
## instead of computing it again.

## This first function creates a list of functions to: 
## 1) set the value of the matrix, 2) get the value of the matrix
## 3) set the inverse, 4) get the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function first checks if the matrix' inverse has already been computed, if it has
## it gets it from the cache. If not, it computes the inverse of the matrix

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
