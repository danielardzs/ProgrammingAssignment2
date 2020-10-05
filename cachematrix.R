## This function takes a matrix and returns its inverse

## This function stores the inverse of a matrix

makeCacheMatrix <- function(x=matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i<<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This functions returns the cached data of the 
##inverse of a matrix previously calculated

cacheSolve <- function(x,...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}