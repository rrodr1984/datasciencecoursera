# The two functions below are used to create a special "matrix" object that can cache its inverse matrix
# and to retrieve that inverse matrix from the cache

###############################################################################################
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
      #initialize variable
      invm <- NULL
  
      #set the value of the matrix
      set <- function(y) {
            x <<- y
            invm <<- NULL
      }
  
      #get the value of the matrix
      get <- function() x
  
      #set the value of inverse of the matrix
      setinverse <- function(inverse) invm <<- inverse
  
      #get the value of inverse of the matrix
      getinverse <- function() invm
  
      ## Return a list with the above four functions
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

###############################################################################################
## This function return a matrix that is the inverse of matrix 'x'
## It computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
      # This retrieves the cached value for the inverse
      invm <- x$getinverse()
  
      # Check if the cache is empty and in case it?s not empty, we can recover the inverse matrix from the cache
      if(!is.null(invm)) {
            message("getting cached data.")
            return(invm)
      }
  
      # If the cache was empty. We need to calculate the inverse of matrix 'x', cache it, and finally return its value.
      # Get value of matrix
      m <- x$get()
  
      #Computing the inverse of a square matrix using the solve function
      #It?s assume that the matrix supplied is always invertible, 
      #so if data is a square invertible matrix, then solve(data) returns its inverse
      invm <- solve(m)
  
      # Cache the result
      x$setinverse(invm)
  
      ## Return a matrix that is the inverse of 'x'
      invm
        
}
