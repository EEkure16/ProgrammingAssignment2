## This function cache's the inverse of a special "matrix'.


makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <-NULL
    
  }
  get <- function(x)
    
    setInverse <- function(inverse) mat_inv <<- inverse
  getInverse <- function() mat_inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  

}


## This function computes the inverse of a special"matrix" as given by the preceding function
## (makeCacheMatrix) or retrieves a pre-calculated inverse matrix from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getinverse()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setinverse(mat_inv)
  mat_inv
  
}
