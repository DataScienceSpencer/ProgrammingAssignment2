## The following functions will allow you to enter a matrix
## and then cache its inverse to avoid calculating that inverse multiple
## times. 


## makeCacheMademotrix will create an empty framework into which you can
## create the matrix you want to calculate the inverse of. 

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y){  
    # x$set will allow you to set the values of your matrix
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve will check to see if your matrix is empty, and if so
## it will calculate the inverse of x, apply it to getinverse
## and return it. If your matrix is not empty (that is, the matrix 
## is already cached there), it will return the cached data. 

cacheSolve <- function (x, ...) {
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

## The following tests the above functions.

test <- makeCacheMatrix()
test$set(matrix(runif(9),3))
test$get()

cacheSolve(test)
cacheSolve(test)

