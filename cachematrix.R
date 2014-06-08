## makeCacheMatrix and cacheSolve are used in tandem to create matrices
## that can store their own inverse, thus eliminating repeating the
## same inversion calculation over and over again.

## This function, makeCacheMatrix, creates a special "matrix" that
## can cache its inverse; it is really a list containing functions
## that manipulate it.
## set, sets the value of the matrix
## get, gets the value of the matrix
## setinverse, sets the value of the inverse matrix
## getinverse, gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The following function, cacheSolve, calculates the inverse of the 
## special "matrix" created with the above function, if the "matrix"
## doesn't already have its inverse in its cache,i. It then returns
## the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
