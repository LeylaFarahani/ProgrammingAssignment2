## makeCacheMatrix creats a special "matrix" objecthat can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) a <<- solve
  getsolve <- function() a
  list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
}

## the function "cacheSolve" computes the inverse of the special "matrix" that retured by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  a <- x$getsolve()
  if(!is.null(a)) {
    message("getting inversed matrix")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setsolve(a)
  a
}