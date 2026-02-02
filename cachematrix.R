###############################################################################
# Matrix Inverse Caching (Closure-Based "Object")
#
# Goal:
#   Computing a matrix inverse can be expensive. These functions create a
#   special matrix object that:
#     1) stores a matrix, and
#     2) caches (remembers) its inverse once computed
#
# Benefit:
#   If you ask for the inverse multiple times without changing the matrix,
#   we return the cached inverse instead of recomputing it.
###############################################################################


###############################################################################
# makeCacheMatrix()
#
# Creates a special "matrix object" implemented via closures.
#
# What it returns:
#   A list of functions (methods) that share access to two private variables:
#     - x   : the stored matrix
#     - inv : the cached inverse of x (NULL until computed)
#
# Methods:
#   - set(y)         : replace the stored matrix with y, and clear cached inverse
#   - get()          : retrieve the stored matrix
#   - setinverse(i)  : store a computed inverse in the cache
#   - getinverse()   : retrieve the cached inverse (or NULL if not available)
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
  
  # Cached inverse (NULL means "not computed yet" or "invalidated")
  inv <- NULL
  
  # Replace the stored matrix; invalidate cached inverse because it's no longer valid
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  
  # Return the current stored matrix
  get <- function() x
  
  # Store an inverse in the cache
  setinverse <- function(inverse) inv <<- inverse
  
  # Retrieve the cached inverse (NULL if none)
  getinverse <- function() inv
  
  # Expose the "methods" as a list (a simple object interface)
  list(
    set        = set,
    get        = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


###############################################################################
# cacheSolve()
#
# Computes (or retrieves) the inverse of a cached matrix object created by
# makeCacheMatrix().
#
# Behavior:
#   1) Check if an inverse is already cached.
#      - If yes: return it immediately.
#   2) Otherwise: compute inverse using solve(), cache it, then return it.
#
# Notes:
#   - The "..." lets you pass additional arguments through to solve().
###############################################################################
cacheSolve <- function(x, ...) {
  
  # Attempt to retrieve cached inverse first
  inv <- x$getinverse()
  
  # If we have a cached inverse, use it (no recomputation)
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute inverse from the stored matrix
  data <- x$get()
  inv  <- solve(data, ...)
  
  # Cache the computed inverse for next time
  x$setinverse(inv)
  
  # Return the newly computed inverse
  inv
}
