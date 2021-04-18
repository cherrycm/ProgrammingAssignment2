
#the program has the makeCacheMatrix command

makeCacheMatrix <- function(x = matrix()) {
  che <- NULL
  set <- function(y) {
    x <<- y
    che <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {che <<- inverse}
  getinverse <- function() {inv}
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#the next program is used to identify cache data

cacheSolve <- function(x, ...) {
  che <- x$getinverse()
  if (!is.null(che)) {
    message("getting cached data")
    return(che)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(che)
  che
}