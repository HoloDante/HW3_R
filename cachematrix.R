## Put comments here that give an overall description of what your
## functions do

## This function has four uses, get matrix x, set it to something, and get or set the inverse of x

makeCacheMatrix<-function(x=matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gives the inverse of a matrix, assuming it is invertible. If the inverse is calculated the first time, it is stored by means o
## makeCacheMatrix, such that solve is only used once, for example if looped over by some other function. 

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
