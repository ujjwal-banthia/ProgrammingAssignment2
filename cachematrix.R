## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m  <- NULL
  #generating a null variable for later use
  #set function to setup a special matrix
  set  <- function(y){
    x <<- y
    m <<- NULL 
  }
  #get function to retrieve the value from cache
  get  <- function() x
  #setinverse function to set new calculated value into cache
  setinverse  <- function(inverse) m  <<- inverse
  #getinverse function to extract the inverse value from the cache
  getinverse  <- function() m
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cach

cacheSolve <- function(x, ...) {
  m  <- x$getinverse()
  #checking if data is already present in cache or not
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #if data is not present in cache then compute and save it into
  #cache using the code below
  data  <- x$get()
  m  <- solve(data, ...)
  x$setinverse(m)
  m
}
