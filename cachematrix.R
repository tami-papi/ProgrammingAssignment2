# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix. If the inverse has already 
# been calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
m <- matrix(c(1, 2, 3, 4), 2, 2)
cm <- makeCacheMatrix(m)
i <- cacheSolve
i <- cacheSolve(cm)
cached_i <- cacheSolve(cm)
cached_i
cached_i
m1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
c1 <- makeCacheMatri(m1)
c1 <- makeCacheMatrix(m1)
i1 <- cacheSolve(cm1)
i1 <- cacheSolve(cm1)
i1 <- cacheSolve(cl)
i1 <- cacheSolve(c1)
i1 <- cacheSolve(cl)
cm1 <- makeCacheMatrix(m1)
i1 <- cacheSolve(cm1)
# Create a 3x3 matrix
m1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
# Create a special "matrix" object that can cache its inverse
cm1 <- makeCacheMatrix(m1)
# Compute and cache the inverse
i1 <- cacheSolve(cm1)
# Print the cached inverse
print(i1)
# Create a 2x2 matrix
m2 <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)
# Create a special "matrix" object that can cache its inverse
cm2 <- makeCacheMatrix(m2)
# Compute and cache the inverse
i2 <- cacheSolve(cm2)
# Print the cached inverse
print(i2)
# Create a 4x4 matrix
m3 <- matrix(c(1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 16), nrow = 4, ncol = 4)
# Create a special "matrix" object that can cache its inverse
cm3 <- makeCacheMatrix(m3)
# Compute and cache the inverse
i3 <- cacheSolve(cm3)
# Print the cached inverse
print(i3)
q()
m4 <- matrix(c(5, 10, 10, 45, 23, 56, 12, 9, 56, 14, 15, 17, 15, 17, 18, 20), nrow = 4, ncol = 4)
cm <- makeCacheMatrix(m4)
i <- cacheSolve(cm)
i
cm
m4
cached_i <- cachedSolved(cm)
cached_i <- cachedSolve(cm)
cached_i <- cacheSolve(cm)
cached_i
m4
q()

