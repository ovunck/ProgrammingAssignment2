## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Defining methods for:
## - setting and getting input matrix x and
## - setting and getting inverse of input matrix x
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## Function to check if input matrix x has a pre-computed/cached inverse
## If inverse of x is already calculated return the cached data
## Else compute the inverse of matrix x and cache it for future use
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
  m
}
