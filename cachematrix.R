## Creates vector that is a list of functions used for manipulating matrices
## Stores inverse of a matrix in a cache
makeCacheMatrix <- function(x = matrix()) {

      ## Initializes inverse storage variable as NULL
      inv <- NULL

      ## Edits matrix, and re-sets inverse cache variable to NULL
      ## Now inverse needs to be re-calculated
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ## Gets matrix
      get <- function() x
      
      ## Stores inverse of matrix in cache
      set_inverse <- function(solve) inv <<- solve
      
      ## Fetches contents of cache
      get_inverse <- function() inv
      
      ## Return final list of functions
      list (set = set, get = get, 
            set_inverse = setinverse, get_inverse = get_invese)
}


## Fetches the inverse of the matrix from the cache, if it has been calculated
## Else calculates the inverse of the matrix
cacheSolve <- function(x, ...) {
      ## Puts contents of cache into 'inv'            
      inv <- x$get_inverse
        
      ## Returns inverse if it is stored already
      if (!is.null(inv)) {
            message("Getting cached data")
            return(inv)
      }
      
      ## Otherwise, it calculates inverse
      else {
      ## Gets matrix using get function, from createMatrix
      m <- x$get()
      
      ## Calculates inverse
      inv <- solve(m, ...)
      
      ## Stores inverse to cache
      x$set_inverse(inv)
      
      ##Returns inverse
      inv
      }
}
