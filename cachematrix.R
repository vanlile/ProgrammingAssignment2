

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## get the value of the inverse
## set the value of the inverse
## Uses the setinverse and getinverse function
## i is the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
## solve (x): Computing the inverse of a square matrix can be done with the solve function.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
}
