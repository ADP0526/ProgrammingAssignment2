## Inverse of a Matrix:
## Cashes the iverse of a matrix instead of constatnly computing it.

## This function creates a matric object and cashes its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  Get <- function() x
  serInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Take makeCashMatrix function ans computed the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  in <- x$getInverse()
  if (!is.null(inv)) {
    message("Receieving Cashed Information")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
