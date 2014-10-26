## These functions should set a matrix, compute and cache its inverse

## makeCacheMatrix will set a matrix, compute and cache its inverse
## when cacheSolve is called.

makeCacheMatrix <- function(x = matrix(x)) {
  InvMatrix <- NULL
  set_x <- function(y) { #allows reset of matrix x
    x <<- y
    InvMatrix <<- NULL
  }
  get_x <- function() {x}   #returns matrix x
  set_inverse <- function(inverse) {   #inverse matrix set by cacheSolve
    InvMatrix <<- inverse
  }
  get_inverse <- function() {InvMatrix} #returns InvMatrix
  list(set_matrix = set_x, get_matrix = get_x,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve will calculate the inverse of x and set it as
## InvMatrix for later calculations

cacheSolve <- function(m, ...) {
  InvMatrix <- m$get_inverse()    #Looks for cached inverse matrix
  if (!is.null(InvMatrix)) {      #Returns InvMatrix if cached
    message("Getting cached inverse.")    #and prints message.
    return(InvMatrix)
  }
  matrix <- m$get_matrix()    #retrieves matrix x
  InvMatrix <- solve(matrix)   #calculates inverse of x
  m$set_inverse(InvMatrix)   #caches InvMatrix using makeCacheMatrix
  InvMatrix        ## Return a matrix that is the inverse of 'x'
}
