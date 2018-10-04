##Matrix inversion can be a costly computation and there is some benefit to caching the inverse of a matrix rather than
##compute it repeatedly.  These two functions cache the inverse of a matrix to be used in computations/

##makeCacheMatrix will create a special matrix object that can cache it's inverse. 

makeCacheMatrix <- function(x = matrix()) {  #sets an empty matrix
  v <- NULL  ##initialized
  set <- function(y) {
    x <<- y
    v <<- NULL  ##clears any other v values that are cached
  }
  get <- function() x
  setInverse <- function(inverse)  v <<- inverse
  getInverse <- function() v
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#cacheSolve will compute the inverse of the special matrix returned by makeCacheMatrix. If the inverse has already been
#calcuated (and the matrix has not changed), cacheSolve will retrieve the inverse from the cache.  This function assumes
#that the matrix is always invertible (determinant != 0)

cacheSolve <- function(x, ...) {
  v <- x$getInverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data)
  x$setInverse(v)
  v
}