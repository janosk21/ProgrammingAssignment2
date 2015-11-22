## the first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the invertedMatrix
## get the value of the invertedMatrix

makeCacheMatrix <- function(x = matrix()) {
  # stores the cached value
  # initialize to NULL
  cache <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store in cache
  setMatrix <- function(inverse) cache <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() cache
  
  # return the created functions to environment
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache, is created in the working environment and it's inverted value, is stored in cache
## otherwise returns the inverted cahedMatrix

cacheSolve <- function(x, ...) {
  ## attempt to get the inverse of the matrix stored in cache
  cache <- x$getInverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(cache)) {
    message("get cached inverted matrix")
    return(cache)
  }
  
  # create matrix since it does not exist
  matrix <- x$get()
  
  # make sure matrix is square and invertible
  # if not, handle exception cleanly
  tryCatch( {
    # set and return inverse of matrix
    cache <- solve(matrix, ...)
  },
  error = function(e) {
    message(e)
    return(NA)
  },
  finally = {
    # set inverted matrix in cache
    message('1st time inverted and stored into the cache')
    x$setMatrix(cache)
  } )
  
  # display matrix in console
  return (cache)
}

#load R program
source("cachematrix.R")                 
# create the function
x <- makeCacheMatrix()
#set the matrix in the working env
x$set(matrix(c(1,0,0, 0,1,0, 0,0,1), nrow=3, ncol=3))
# check get
x$get()
# invoke inverse to the matrix, in case can't inverse return NA
# First run... should display 
cacheSolve(x)              
