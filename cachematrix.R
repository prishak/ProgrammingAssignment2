## The inverse of Matrix to be calculate.
##if the inverse calculated before or had not change then it returns the vale from the cache instead of re-calculating it again.

## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y){ 
    x <<- y
    p <<- NULL
  }
  get <- function()x
  setInverse <- function(solve)p<<-solve 
  getInverse <- function()p
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}
       
  

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and there has no change in Matrix,then cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  p <- x$getInverse()
  if(!is.null(P)){ 
    message("getting cached data")
    return(p)
  }
  data<-x$get()
  p<-solve(data,...)
  x$setInverse(p)
  p
        ## Return a matrix that is the inverse of 'x'
}
