
#the program has the makeCacheMatrix command

makeCacheMatrix <- function(x = matrix()) {
  cherrycm <- NULL
  set <- function(y) {
    x <<- y
    cherrycm <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {cherrycm <<- inverse}
  getinverse <- function() {cherrycm}
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#the next program is used to identify cache data

cacheSolve <- function(x, ...) {
  cherrycm <- x$getinverse()
  if (!is.null(cherrycm)) {
    message("getting cached data")
    return(cherrycm)
  }
  data <- x$get()
  cherrycm <- solve(data, ...)
  x$setinverse(cherrycm)
  cherrycm
}