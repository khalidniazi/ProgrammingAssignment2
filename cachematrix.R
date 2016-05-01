
## Function "makeCacheMatrix" creates a matrix object that can caches its inverse
## Computes inverse of the matrix if the inverse is not already cached; else it 
## returns the cached inverse of the matrix.

## generates an object whose inverse can be cached
makeCacheMatrix <- function(x = matrix()) { 
  inverse <- NULL                     
  set <- function(y) {                      
    x <<- y
    inverse <<- NULL              
  }
  get <- function() x                           
  setinv <- function(solve) inverse <<- solve 
  getinv <- function() inverse        
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## computes and returns the inverse or returns the already cached inverse
cacheSolve<- function(x, ...) {                 
  inverse <- x$getin()
  if(!is.null(inverse)) {                 
    message("Yahoooo!!!! we got the cached data :-)")
    return(inverse)
  }
  data <- x$get()                               
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}
