## Author: ama4tf
## The goal is to create an object that can cache the solution to a complex operation on a matrix (in this case the inverse)
##The second function is the process to check this cache and if not populated, populate it

## makeCaceMatrix creates a list object that can store a 
## cached version of a matrix and it's inverse in the parent environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Given the cached variable created above, looks for if getInv is set
## If yes returns it, if not, it solves thematrix and stores it as m using setInv

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}