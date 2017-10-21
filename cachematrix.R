## Put comments here that give an overall description of what your
## functions do

## Once a matrix has been created to equal makeCacheMatrix, the following functions first find the inverse of the matrix and prints it, 
## then if the inverse of the matrix has already been performed, it is cached and R pulls that result, otherwise, it performs the 
## inverse function on the new matrix.  



## Write a short comment describing this function

## makeCacheMatrix is a function that takes a pre-specified matrix and finds its inverse and puts the inverse in a list.  

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {	   
    x <<- y  	    
    m <<- NULL      	  }	  
  get <- function() x    	  
  setinverse <- function(solve) m <<- solve  	  
  getinverse <- function() m    	  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	}





## Write a short comment describing this function

## cacheSolve takes the pre-specified matrix and looks for the solution in the list generated in the makeCacheMatrix function,
## if it is not in the list, it performs the inverse function (solve) and delivers the solution.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
} 



## Return a matrix that is the inverse of 'x'


