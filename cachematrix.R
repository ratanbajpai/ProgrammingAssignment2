## This set of functions use the scoping rules in R
## to cache potentialy time consuming computations.
## These functions compute and cache the inverse of an
## invertible matrix. The cached value of inverted matrix
## is re-used if the data has not changed.
## This set of functions is very useful if the matrix
## to be inverted is large and the inverse has to be
## computed repeatedly in a loop.

## The function makeCacheMatrix creates a special "vector",
## which is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function calculates the inverse of the
## special "matrix" created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been
## calculated. If yes, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setsolve
## function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

