# This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv_mat
  list(set=set, get=get, setinv=setinv getinv=getinv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_mat <- x$getinv()
  if(!is.null(inv_mat)) {
    message("Getting cached data, instead of recalculating.")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data)
  x$setinv(inv_mat)
  inv_mat
}

#  RUNNING EXAMPLE
# > x = matrix(
#   + c(1,0,5,2,1,6,3,4,0)
#   + ,
#   + nrow=3,
#   + ncol=3
#   + )
# > x
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
# > cacheSolve(m)
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

#  getting the cached value  when tried to re-calculate the same thing

# n <- cacheSolve(m)
# getting cached data.
# > n
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

# making sure the inverse is correct by multiplying matrix with its inverse to get identity matrix
# > m$get() %*% n
# [,1]          [,2]          [,3]
# [1,]    1 -1.776357e-15 -4.440892e-16
# [2,]    0  1.000000e+00  0.000000e+00
# [3,]    0  0.000000e+00  1.000000e+00
