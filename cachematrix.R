## These functions solve for the inverse of a matrix and store it in the cache and then return it if possible.
## makeCacheMatrix takes a matrix stores it into the cache.
## cacheSolve takes the stored cache checks to make sure it is not null. 
## If it is not null then it solves for the inverse and returns it.



## This function takes a matrix and stores it in cache

makeCacheMatrix <- function(x = matrix()){
  inver <- NULL
  
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inver <<- inverse}
  getInverse <- function() {inver}
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


## cacheSolve function checks to see if the value of the matrix from makeCacheMatrix is null
## if it is not null it takes the cached matrix and solves for the inverse and returns it


cacheSolve <- function(x, ...){
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message ("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setInverse(inver)
  inver
  
  
}