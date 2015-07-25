# This 2 functions creates a square invertible matrix(1 - makeCacheMatrix)
# and make the inverse of the matrix , which stored in a cache(2 - cacheSolve)
#
# Sample  included results
# > source("cachematrix.r")    load  program
# > m <- makeCacheMatrix()     create matrix m by function
# > m$set(rbind(c(1, -0.25,1), c(-0.25, 1,-0.25),c(-0.25, 0,1))  )  create & fill matrix 
# > cacheSolve(m)              1st run returns inverted matrix
#                              from working environment
#        [,1]     [,2]     [,3]
#[1,] 0.8533333 0.21333333 -0.8
#[2,] 0.2666667 1.06666667  0.0
#[3,] 0.2133333 0.05333333  0.8
#
# > cacheSolve(m)             2nd and subsequent runs
#                             returns inverted matrix from cache
# getting cached data          
#        [,1]     [,2]     [,3]
#[1,] 0.8533333 0.21333333 -0.8
#[2,] 0.2666667 1.06666667  0.0
#[3,] 0.2133333 0.05333333  0.8

## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  # stores the cached value, initialize to NULL
  cachematrix <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    cachematrix <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store in cache
  setMatrix <- function(inverse) cachematrix <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() cachematrix
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


#cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
#If the inverted matrix does not exist in cache,
#it it created in the working environment and it's inverted value
#is stored in cache
cacheSolve <- function(x, ...) {
  ## attempt to get the inverse of the matrix stored in cache
  cachematrix <- x$getInverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(cachematrix)) {
    message("getting cached matrix")
    
    # display matrix in console
    return(cachematrix)
  }
  
  # create matrix since it does not exist
  matrix <- x$get()
  
  # make sure matrix is square and invertible
  tryCatch( {
    cachematrix <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error!!!:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    # set inverted matrix in cache
    x$setMatrix(cachematrix)
  } )
  
  # display matrix in console
  return (cachematrix)
}

#source("cachematrix.r")
#m <- makeCacheMatrix()
#m$set(rbind(c(1, -0.25,1), c(-0.25, 1,-0.25),c(-0.25, 0,1))  )

#cacheSolve(m) 
