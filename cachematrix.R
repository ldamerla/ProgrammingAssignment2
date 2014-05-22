 #These set of functions take a matrix that is passed to it and sends the inverse of the matrix.  
 #When the inverse is calculated,the next time the matrix is sent, the inverse is returned from the Cached inverse previously calculated
 
 

## makeCacheMatrix creates a set of functions to set and get a matrix that is passed to it and set inverse and get inverse of that matrix.


makeCacheMatrix <- function(m =matrix()) {
  
 inv <- NULL
  
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  
  get <- function() m
  
  setInverse <- function(Solvedinv)  inv <<- Solvedinv
  
 
  
  getInverse <- function() inv
  
  list( 
       set = set,
       get = get,
       setInv = setInverse,
       getInv = getInverse)
}







 
        ## Return a matrix that is the inverse of 'm' the matrix passed to this function
cacheSolve <- function(m,...) {
  inv <- m$getInv()
  
  if (!is.null(inv)){
    message("getting cache data")
    return (inv)
  }
  
  inv <- solve(m$get(),...)
  m$setInv(inv)
  inv
}

