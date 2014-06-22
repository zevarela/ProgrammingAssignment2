## This is an example of R code to preserve state in a different environment, that can be used for caching data.
## The functions bellow will create a "matrix extension" that can cache it's inverse, and a sover function
## that uses the cached version if available. 
## It was my first attempt at this assigment, and my first use of GitHub! Thanks for checking it out.


## This function was created as my first attempt at an R extension of the matrix object that caches it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(new.matrix) {
    if (new.matrix!=x) # only update the matrix when needed
    {
      x <<- new.matrix
      inverse <<- NULL
    }
  }
  get <- function() x
  set.inverse <- function(solved.inverse) inverse <<- solved.inverse
  get.inverse <- function() inverse
  list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


## This function receives a makeCacheMatrix object and returns the inverse of it's matrix.
## returns the cached inverse it the matrix has already been inverted before
cacheSolve <- function(x, ...) {
  inv <- x$get.inverse()
  if(!is.null(inv)) {
    message("Inverse was previously computed, returning cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set.inverse(inv)
  inv
}
