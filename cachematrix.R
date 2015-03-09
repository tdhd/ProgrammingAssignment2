## Example of how to cache the inverse of a matrix

## creates a matrix frmo the passed argument where the inverse may be cached

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## calculates the inverse of the argument x
## in the case it has been calculated before it returns the cached value
## otherwise the inverse is calculated and stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

m = makeCacheMatrix(diag(3))
# calculate inverse
print(cacheSolve(m))
# prints 'getting cached data', because the inverse was calculated before
print(cacheSolve(m))

