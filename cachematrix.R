## This function will store the matrix as well as its
## inverse. Allows for matrix to be get & set as well
## as getInverse and setInverse for the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(inputMatrix)
  {
    #message("setMatrix, makeCachMatrix")
    x <<- inputMatrix
    cachedInverse <- NULL
  }
  get <- function()
  {
    #message("getMatrix, makeCacheMatrix")
    x
  }
  setinverse <- function(computedInverse)
  {
    #message('setInverse, makeCacheMatrix')
    cachedInverse <<- computedInverse
  }
  getinverse <- function()
  {
    #message('getInverse, makeCacheMatrix')
    cachedInverse
  }
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  cachedInverse <- x$getinverse()
  #Check if there is a cached inverse and return it if there is
  if (!is.null(cachedInverse))
  {
    message("cached matrix found:")
    #There is a cached inverse matrix. 
    return (cachedInverse)
  }
  #If there is no cached inverse yet
  cachedMatrix <- x$get()
  calculatedInverse <- solve(cachedMatrix, ...)
  x$setinverse(calculatedInverse)
  message('no cached inverse found. here is the new inverse:')
  return (calculatedInverse)
}
