## The inverse of Matrix to be calculate.
##if the inverse calculated before or had not change then it returns the vale from the cache instead of re-calculating it again.

## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){ 
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(solve)m<<-solve 
  getInverse <- function()m
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}
       
  

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and there has no change in Matrix,then cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){ 
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setInverse(m)
  m
}