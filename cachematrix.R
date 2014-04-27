## Below are two functions that are used to create a special 
## object that stores a matrix and cache's its inverse

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## assign the inverse "s"
  s <- NULL
  ## set the value of the matrix
  set <- function(y) {
    ##assign the value in the current environment to prevent overloading
    x <<- y        
    s <<- NULL
    }
  
  #get the value of the matrix
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the invers of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates 
## the invers of the data and sets the value of the inverse in the cache 
## via the setinverse function.

  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)) {
      message("Getting cached data")  
      return(s)
      }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
  }
