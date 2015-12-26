## These two functions allow you to cache and return the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
  # Creates four functions that allow you to store and retrieve the matrix and the 
  # inverse of the matrix.
  #
  # Args:
  #   X: a matrix that you want to calculate the inverse of
  # 
  # Returns:
  #   A list of the four functions: get, set, getinverse, setinverse
  
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))

}


cacheSolve <- function(x, ...) {
  # Look to see if the inverse of this matrix has already been calculated and return 
  # that cached value if so. If it has not been, calculate the inverse of the matrix 
  # and return.
  # 
  # Args:
  #   x: the list of functions returned by the makeCacheMatrix() function
  #
  # return: 
  #   The inverse of the matrix
        
  i <- x$getinverse()
  if(!is.null(i)){
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  
}

