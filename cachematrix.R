makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  
  set <- function( matrix ) {
    m <<- matrix
    inv <<- NULL
  }
  get <- function() m
  
  setInv <- function(inverse) inv <<- inverse
  
  getInv <- function() inv
  
  list(set = set, get = get,
    setInv = setInv,
    getInv = getInv)
}

cacheSolve <- function(m, ...) {
  inv <- m$getInv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- m$get()
  
  inv <- solve(data)
  
  m$setInv(inv)
  
  return(inv)
}