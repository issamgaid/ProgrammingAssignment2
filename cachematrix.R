## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
  {
  matInverse <- NULL
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(inverse) matInverse <<- inverse
  getMatrixInverse <- function() matInverse

  list(set = set, 
        get = get,
        setMatrixInverse = setMatrixInverse,
        getMatrixInverse = getMatrixInverse
      )
}






##This function computes the inverse of the special "matrix" using if possible the cache
cacheSolve <- function(x, ...) 
  {
  ##This function computes the inverse of the special "matrix" 
  ##returned by makeCacheMatrix above. If the inverse has 
  ##already been calculated (and the matrix has not changed), 
  ##then the cachesolve should retrieve the inverse from the cache.
  ## Return a matrix that is the inverse of 'x'
  ##mat <-x$getMatrix()
  m <- x$getMatrixInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  newInverse <- solve(data, ...)
  x$setMatrixInverse(newInverse)
  newInverse
}

##How to test these é functions
##> source("cachematrix.R")
##> mat <- matrix(c(0,2,5,4),2,2)
##> mat2 <- makeCacheMatrix(mat)
##> cacheSolve(mat2) #inverse returned after computation
##     [,1] [,2]
##[1,] -0.4  0.5
##[2,]  0.2  0.0
