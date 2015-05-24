# The first function, `makeCacheMatrix` is a structure that holds a matrix, its inverse 
# and two pairs of getters/setters that can be used to control them.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # setter for matrix 
  set <- function(y) {
    x <<- y
    # whenever new matrix is set value of inverse is nulled
    m <<- NULL
  }
  
  # getter for matrix
  get <- function() x
  
  # getter and setter for inverse 
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  # returning list of functions defined above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function returns inverse of input matrix.
# It calculates new inverse only if it was not calculated first.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # getting inverse of matrix x
  m <- x$getInverse()
  
  # if the value is not NULL we return it instead of calculating new inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # otherwise we get matrix
  data <- x$get()
  
  # and calculate inverse using solve function
  m <- solve(data, ...)
  
  # and we set it as an inverse of x
  x$setInverse(m)
  
  # and return the inverse
  m
}
