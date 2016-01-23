## The functions below calculate and cache the inverse of inputted matrix. Hence if 
## the same matrix is inputted, the code would not do the calculation again. 

## This function sets and gets a inputted matrix and inverse of that matrix. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    set_inv_mat <- function(inv_mat) m <<- inv_mat
    get_inv_mat <- function() m 

  list(set = set, get = get,
       set_inv_mat = set_inv_mat,
       get_inv_mat = get_inv_mat)
}


## Calculate the inverse of matrix and validate if it is already availabe. If so,
## just simply print out the stored data without re-calculating 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv_mat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv_mat(m)
  m
}
