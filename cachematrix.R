## Assignment No.1: 

## This function creates a special "matrix" object that can cache its inverse.
## In the first function, I've set the solved value "sv" as null.

makeCacheMatrix <- function(x = matrix()) {
  sv <- NULL
  set <- function(y){
    x <<- y
    sv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) sv <<- solveMatrix
  getInverse <- function() sv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Assignment No. 2
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

## I've used "sv" in the first function below to be sure that 
## this will be linked properly to the function created above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  sv <- x$getInverse()
  if(!is.null(sv)){
    message("getting cached data")
    return(sv)
  }
  data <- x$get()
  sv <- solve(data)
  x$setInverse(sv)
  sv
}
