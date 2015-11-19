## makeCacheMatrix creates a matrix object with functions 
## for retrieving its inverse from cache
## cacheSolve uses this object to retrieve its inverse from cache or
## else calculate the inverse and cache it


## makeCacheMatrix contains functions to set and get a matrix
## also contains functions to set and get its inverse
## returns list containing these functions so that 
## they can be used elsewhere

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inverse <<- inverse
  
  getinverse <- function() inverse
  
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)

}


## tries to get inverse from cache. If it exists (not null)
## then it is returned

## if the inverse in cache is null, it solves the inverse
## then stores the result in cache and returns the inverse

cacheSolve <- function(x, ...) {
  
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
