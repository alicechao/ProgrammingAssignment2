####################################
## Assignment:    2               ##
## Author:        Alice Chao      ##
## Last Updated:  22th June, 2014 ##
####################################

# Summary: A pair of functions that is able to cache the inverse of a matrix. 
# We assume that the matrix supplied is always invertible.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # Set inverse to null
  i <- NULL
  
  # Subfunction to set the matrix outside of the current function 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Subfunction to get the submitted matrix 
  get <- function() x
  
  # Subfunction to set the inverse of the submitted matrix 
  set_inverse <- function(inverse) i <<- inverse
  
  # Subfunction to get the inverse of the submitted matrix 
  get_inverse <- function() i
  
  
  list(set = set, 
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cacheSolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  i <- x$get_inverse()
  
  # If inverse existed, then include a "getting cached data" message first
  # to inform the user that cached data is being returned 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Otherwise, the inverse hasn't been calculated yet, so get the matrix and 
  # calculate and return its inverse 
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}