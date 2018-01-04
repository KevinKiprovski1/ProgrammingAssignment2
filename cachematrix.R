## This function creates an object that is able to store a matrix and the inverse of the matrix
## This will allow for the quick retreval of data from the object using only one computation
## Rather than having to perform the computation each time it is needed. 

## makeCacheMatrix produces an object with data in it
## cacheSolved is used to access the data in the makeCacheData object

makeCacheMatrix <- function(x = matrix()) {
  inve = NULL 
  set <- function(y){
    x <<- y 
    inv = NULL
  }
  get <- function() x 
  
  setinverse <- (inverse) inve <<- inverse
  
  getinverse <- function() inve
  
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 inv <- x$getinverse()
 if (!is.null(inve)){
   message("Getting cached data")
   return(inve)
 }
 data <- x$get()
 inve <- solve(data)
 x$setinverse(inve)
 inve
}