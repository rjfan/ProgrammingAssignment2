## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The basic steps of the algorithm
### Create a function of functions names 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ### Set the values of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ### Get the values of the matrix
  get <- function() x
  ### Cache the inverse
  setinv <- function(minv) inv <<- minv
  ### Retrieve the inverse
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ### Get the cached inverse
  inv <- x$getinv()
  
  ### Check if it already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ### Find the inverse otherwise
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
