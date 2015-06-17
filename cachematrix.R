## Functions below provide the inverse of a square matrix. A cached solution will be 
## returned if available. Otherwise, the matrix inverse is computed using solve(). 
##
## Note: The code provided in the Readme.md for functions "makeVector" and  
##       "cachemean" were modified to work with the problem of matrix inverse. 


## Function makeCacheMAtrix creates a list containing a function to 
## 1) set the value of the matrix, 
## 2) get the value of the matrix, 
## 3) set the inverse of the matrix, and;
## 4) get the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Return a matrix that is the inverse of 'x'. Returns the cached inverse if available 
## (and the skips computation). Otherwise, inverse is computed using solve().

cacheSolve <- function(x, ...) { 
  
  m <- x$getinverse()

  # check if inverse is already in cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If not in cache, calculate inverse using solve()
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
