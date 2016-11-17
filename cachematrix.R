## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # assign inverse variable to NULL
  i <- NULL
  
  # set the special matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #get the special matrix
  get <- function() x
  
  # set the inverse matrix using the solve function
  setinverse <- function(solve) i <<- solve
  
  # get the inverse matrix
  getinverse <- function() i
  
  list( set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## get the inverse of the matrix
  i <- x$getinverse()
  
  ## check if the inverse is already calculated. 
  ## If exists, return the inverse
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  data <- x$get()
  
  ## Return a matrix that is the inverse of 'x'       
  i <- solve(data, ...)
  
  x$setinverse(i)
  
  i
  
}
