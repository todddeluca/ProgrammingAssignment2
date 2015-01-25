## Put comments here that give an overall description of what your
## functions do

# Since inverting a matrix can be an time-consuming operation, it can be useful to cache the
# result of a matrix inversion.  This module contains two functions to support this aim:
#
# - makeCacheMatrix: creates a new matrix and wraps it in a list containing functions to get and set
#   the underlying matrix, and to get and set the cached inverted matrix associated with that matrix.
# - cacheSolve: Given a "cacheMatrix" list, this function will return a cached inverse of the matrix
#   if it exists.  Otherwise it will compute and cache the inverse before returning it.


# This function creates a special "matrix" object that can cache its inverse.
# param x: an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    # y shoud be an invertible matrix
    x <<- y
    # clear any cached inverse
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
    # cache the inverse
    inv <<- inverse
  }
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.
# param x: a "cacheMatrix" list returned by makeCacheMatrix
# param ...: parameters passed through to the solve function
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


manuallyTestCacheMatrix <- function() {
  # test matrices from https://www.khanacademy.org/math/precalculus/precalc-matrices/inverting_matrices/e/matrix_inverse_3x3
  m1 = matrix(c(2, 0, 0, 0, 1, 0, 2, 2, 2), 3, 3)
  m1
  
  # test using a regular matrix with cacheSolve
  i1 = cacheSolve(m1) # should fail when given a normal matrix
  
  cm1 = makeCacheMatrix(m1)
  # test that cache is initially empty
  is.null(cm1$getinverse())
  
  # test inverting a matrix
  i1 = cacheSolve(cm1)
  i1 # manully checked result on Khan Academy website.
  
  # test that cache is set
  !is.null(cm1$getinverse())
  
  m2 = matrix(c(1, 1, 1, 2, 1, 1, 2, 2, 0), 3, 3)
  m2
  
  # set matrix to a new matrix
  cm1$set(m2)
  
  # test that cache is cleared
  is.null(cm1$getinverse())
  
  i2 = cacheSolve(cm1)
  i2 # manully checked result on Khan Academy website.
  
  # test that cache is set
  !is.null(cm1$getinverse())
}
