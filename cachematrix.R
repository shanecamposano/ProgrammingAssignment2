## Matrix inversion sometimes is a time-consuming computation
## so caching the inverse of a matrix instead of a repeated computation
## can be considered.

## makeCacheMatrix creates a list containing function that:
## -set value of matrix
## -get value of matrix
## -set value of inverse of matrix
## -get value of inverse of mtrix

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  set <- function(y){
        x <<- y 
        inv <- NULL
  } 
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function()inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}

##cacheSolve computes the inverse of the matrix returned by makeCacheMatrix function
##considering first if the inverse has already been calculated.
##If it's the case,it "get"s the result and skip the computation.
## Otherwise, it computes the inverse of the matrix, sets the value in the cache using "SetInverse" function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
if (!is.null(inv)) {
  message("getting cached data")
  return(inv)}

  matdata <- x$get()
  inv <- solve(matdata, ...)
  x$setInverse(inv)
  inv
}
