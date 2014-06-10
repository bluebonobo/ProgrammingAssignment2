## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##  This function takes a standard Matrix as an input and returns a list of get/set/getInverse/setInverse methods
##  It initializes the cache variable inv to NULL
##  The set method writes the matrix x in cache using the operator <<-
##  The get method reads the matrix x from cache
##  The setInverse method writes the inverse of x (inv matrix) in cache using the operator <<-
##  The getInverse method reads the inv matrix from cache

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##  Write a short comment describing this function

##  This function takes a special Matrix created by makeCacheMatrix as an input and other argument if needed
##  It returns the inverse of matrix given as input
##  It retrieves the inverse of matrix x from the cache using getInverse
##  If the inverse of x already exist in cache, it returns the cached inverse matrix (inv)
##  If the inverse of x doesn't exist in cache, it computes it (using solve) and saves it to cache (setInverse) and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()

  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv  
  
}


## How to test
## 
## > source(cachematrix.R)
## > testm<-matrix(c(4,0,0,4),nrow=2,ncol=2,byrow = TRUE)
## > specm<-makeCacheMatrix(testm)
## > cacheSolve(specm)
##      [,1] [,2]
## [1,] 0.25 0.00
## [2,] 0.00 0.25
## > cacheSolve(specm)
## getting cached data
##      [,1] [,2]
## [1,] 0.25 0.00
## [2,] 0.00 0.25


