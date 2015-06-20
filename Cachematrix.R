## Computing the inverse of a square matrix, then solve(X)returns its reverse
## functions do

## makeCacheMatrix creates a specialmatrix object that can cache its
inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n<<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse)n  <<- inverse
  getinverse <- function() n
  list(set = set, get = get,
       setinverse = setinverse, 
       sgetinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##it first checks to see the inverse has already been computed
##If so it gets the inverse from the cache and skip computation
## If not it calculates the inverse of the data

cacheSolve <- function(x, ...) {
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
