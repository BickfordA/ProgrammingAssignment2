## Put comments here that give an overall description of what your
## functions do

## This function creates a special list type which allows for getting and
## setting the base value of the matrix as well as getting and setting the
## computed value of the matrix

makeCacheMatrix <- function(data = matrix()) {
  inverse <- NULL
  x <- data

  set <- function(y){
    x <<- y
    inverse <<- NULL
  }

  get <- function() x
  
  setInverse <- function( solve ) inverse <<- solve
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## given a cache matric object as created in makeCacheMatrix
## this funciton will check to see if thesolve object already has 
## the inverse value computed, if it does it will return it. If
## not it will compute it and set the corresponding value in the
## input object

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached inverse")
    return(inv)
  }
  #otherwise compute the inverse
  message("Calculating inverse")
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
