## Put comments here that give an overall description of what your
## functions do

## This function created a list containing a function to 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set part
  set <- set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get part
  get <- function() x
  
  # Function that calculates the inverse 
  # of the input matrix using the built-in 
  # function "solve"
  setinv <- function(solve) inv <<- solve
  
  # Function that returns the inverse if
  # it is stored in the cache
  getinv <- function() inv
  
  # makeCacheMatrix returns a list containing the 
  # functions desribed in lines 4-8.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The following function calculates the inverse 
## of the matrix that was used as an input to the 
## above function. However, it first checks to see 
## if the inverse has already been calculated. If 
## so, it gets the mean from the cache and skips 
## the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the 
## inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Using the getinv function, of the makeCacheMatrix
  # list of functions, that returns the inverse of the 
  # input of makeCacheMatrix, if it has been previously
  # calculated. If it has not been calculated it returns
  # NULL.
  inv <- x$getinv()
  
  # Checking if the inverse received above is null. If 
  # it is not, this function prints "getting cached data"
  # and returns the inverse from the cache.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse retreived above is null, it means that it
  # has not been previously calculated and so, we retrieve
  # the inintial matrix using the get function of makeCacheMatrix
  data <- x$get()
  
  # Then we compute the inverse using the built-in function "solve"
  inv <- solve(data, ...)
  
  # We save the inverse matrix in the cache using the setinv function
  # of makeCacheMatrix
  x$setinv(inv)
  
  # cacheSolve ruturns the inverse of the matrix
  inv
}
