## Below are two functions that are used to create a special object 
## that stores a numeric matrix and cache's its inverse.



## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ivrs <- NULL
  set <- function(y) {
    x <<- y
    ivrs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ivrs <<- inverse
  getinverse <- function() ivrs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function calculates the inverse of the special "matrix" 
## created with the first function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ivrs <- x$getinverse()
  if(!is.null(ivrs)) {
    message("getting cached data")
    return(ivrs)
  }
  data <- x$get()
  ivrs <- inverse(data, ...)
  x$setinverse(ivrs)
  ivrs
}
