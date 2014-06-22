## The two functions together provides the ability to cache the inverse of a matrix
## This is useful to avoid repetitive caclulation

## The first function makeCacheMatrix returns a list containing the functions to be used
## by the second method
## It takes a matrix as input and creates 4 methods and returns them as a list
## The methods are described inside the function

makeCacheMatrix <- function(x = matrix()) {
  ## On first run, it sets the inverse to be NULL
  inverse <- NULL
  
  ## The set method modifies the original matrix and also sets inverse to be NULL
  Set <- function(y){
    x <<- y
    inverse <<- NULL
    
  }
  
  ## The setinverse method takes an *already INVERSED* matrix and sets it as the inverse
  
  SetInverse <- function(i){
    inverse <<- i
  }
  
  ## The get method retrieves the original matrix
  
  Get <- function(){
    x
  }
  
  ## The getinverse method retrieves the inversed matrix
  
  GetInverse <- function(){
    inverse
  }
  
  ## Finally, we return the list of all the 4 methods above
  
  list(set = Set,
       get = Get,
       setinverse = SetInverse,
       getinverse = GetInverse)
  
}


## The second function returns the cached inverse making use of the object of the first function
## if no such inverse exists, it calculates one and caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## The first step is to see if we already have calculated the matrix, and so we retrieve it using
  ## get inverse method of the object
  i <- x$getinverse()
  
  ## If this is NOT NULL i.e. it contains an inverse, we return it.
  
  if (!is.null(i)){
    ## We inform the user that we are getting data from cache and return the inverse
    message("getting cached data")
    return(i)
  }
  
  ## If the inverse was NULL, this part of code executes
  
  ## We get the original matrix using the get method of the object
  matrix <- x$get()
  
  ## We solve the matrix i.e. calculate its inverse
  i <- solve(matrix,...)
  
  ## We set the inverse into the cache using the setinverse method
  x$setinverse(i)
  
  ## Finally, we return the inverse
  i
  
}
