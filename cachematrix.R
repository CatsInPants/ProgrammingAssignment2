## The function makeCacheMatrix creates a special vector
## that works saving the inverse of a matrix in a cache (this func).

makeCacheMatrix <- function(x = matrix()) {
  
  ## Here we are creating a "inv" value that it's undefined by now.
  ## 
  inv <- NULL
  
  ## 1.  set the value of the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## 2.  get the value of the matrix
  get <- function() {x}

  ## 3.  set the value of the inverse (of the matrix)
  setInverse <- function(inverse) {inv <<- inverse}
  
  ## 4.  get the value of the inverse (of the matrix)
  getInverse <- function() {inv}
  
  ## Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The second part of the function, cacheSolve shows the result
## (inversed matrix) from the cache saved in makeCacheMatrix
## function

cacheSolve <- function(x, ...) {
  
  ## Here we says that inv contains the inverse matrix of x
  inv <- x$getInverse()
  
  ## When exist a value in inv (previously saved or used) 
  ## the function shows a message
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## To show the matrix from our object
  mat <- x$get()
  
  ## Now, solve the inverse using the inv.
  inv <- solve(mat, ...)
  
  ##Set the inverse to the object
  x$setInverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}

## TO USE.
## first you can source the .R file ("cachematrix.R" in this case)
## You must create a matrix (or use any stored matrix), in this 
## case, we use "probe"

## > probe <- makeCachematrix(matrix(1:4,2,2))
## > probe$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## if we use probe$getInverse(), the result is NULL
## Now, to resolve the inverse of matrix probe:

## > cacheSolve(Probe)

## Now the cache is stored and we can use 
## > probe$getInverse()

## what if we want to probe with a 4*4 matrix:

## > probe$set(matrix(c(22,15,10,9,5,12,5,21,1,10,11,9,8,13,2,7),4,4))
## > probe$get()
##       [,1] [,2] [,3] [,4]
## [1,]   22    5    1    8
## [2,]   15   12   10   13
## [3,]   10    5   11    2
## [4,]    9   21    9    7

## now probe$getInverse() give us NULL
## cacheSolve(probe)  give us the inverse.