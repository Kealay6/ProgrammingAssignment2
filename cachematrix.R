## function that caches a special matrix where the argument will be a matrix
makeCacheMatrix <- function(x=matrix()) {
  
  ## define a variable called "matrixInverse" and set it to be nothing (placeholder)
  matrixInverse <- NULL
  
  ## function that sets y to x (original matrix)
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  
  ## function that gets the value of x (original matrix)
  get <- function() x 
  
  ## function that sets the value of the inverted matrix
  setInverse <- function(inverse) matrixInverse <<- inverse
  
  ## function that gets the value of the inverted matrix
  getInverse <- function() matrixInverse
  
  ## combine them all in a list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## function that computes the inverse of the "special" matrix returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  
  #define a variable to get the inverse into
  inv <- x$getInverse()
  if(!is.null(inv)) {
    #if inv is NOT NULL, then this code executes
    
    #output a message to say it's getting the already calculated inverse
    message("getting cached data")
    
    #return the answer (no need to calculate again)
    return(inv)
  }
  
  ## pull the original matrix and apply to the data variable
  data <- x$get()
  
  ## calculate the inverse using the original matrix
  inv <- solve(data,...)
  
  ## set the inverted matrix so that it can be cached for next time
  x$setInverse(inv)
  
  ## return the answer
  return(inv)
  
}