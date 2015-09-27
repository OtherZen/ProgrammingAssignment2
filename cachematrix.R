## A pair of functions that: (1) create a matrix with getter/setter functions on both the matrix data and it's inverse (if computed),
## (2) return the inverse of a matrix (cached or computed)

## "makeCacheMatrix": takes in a square matrix "x", initializes the cached inverse variable to null, 
## and define four getter/setter functions for the matrix data and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL 
  set <- function(y){
    x <<-y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) invMatrix <<- solved
  getinverse <- function () invMatrix
  list(set=set,get=get,
       setinverse = setinverse, getinverse = getinverse)
  }


## "cacheSolve": takes the resulting matrix from "makeCacheMatrix()" as "x", returns the cached inverse variable associated with 
## x if it exists, otherwise it computes, caches and returns the inverse of x

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
  }
