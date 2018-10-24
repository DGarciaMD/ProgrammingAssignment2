## I am creating for an assignment two functions to
## creates a special "matrix" and cache's its inverse 
## Thanks for looking around 
## By Daniel García Morante

## The first function, makeCacheMatrix creates the special "Matrix",
##containing diverses functions to:
##set the value of the Matrix
##get the value of the Matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL     
  setMatrix <- function(y) {    # set the value of the matrix
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x     # get the value of the matrix
  setInverse <- function(solve) inverse <<- solve   #set the value of the inverse
  getInverse <- function() inverse                  #get the value of the inverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## this second function calculates the mean of the special "matrix"
##created with the makeCacheMatrix() fuction. first it checks to see
##if the inverse Matrix has already been calculated. If so, it gets the
##inverse from the cache and skips the computation. Otherwise, it calculates
##the inverse matrix of the data and sets the value of the inverse in the 
##cache via the setmean function.


> cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()    #get the value of the inverse matrix from the previous function
  if(!is.null(inverse)) {     # conditional if the inverse is not NULL then show that message and return the inverse matrix
    message("getting cached data")
    return(inverse)
  }
  data <- x$getMatrix()        #otherwise get the original MAtrix Data
  inverse <- solve(data, ...)  #compute the inverse
  x$setInverse(inverse)        #set the invertible Matrix
  return(inverse)              #return a Matrix that is the inverse of "x"
}

