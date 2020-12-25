## Two functions that cache the inverse of a matrix
## Thanks for reviewing and wish you success in the course

## Creates a matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {j <<- inverse}
  getInverse <- function() {j} 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Function that returns a matrix that is inverse of x 

cacheSolve <- function(x, ...) {
 
  j <- x$getInverse()
  if(!is.null(j)){
    message("retrieving cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

