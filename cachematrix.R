## this function creates a the inverse of a matrix in the cache. It also allows us to retrieve it. 
## Also, it creates an object that contains the inverse of this matrix.


makeCacheMatrix <- function(x = matrix()){          # This will set m to NULL and set x to argument y
      m <- NULL      
      set <- function(y) {    
            x <<- y
            m <<- NULL
      }
      
## The code below will define the vector of functions that allow us to return m (get), 
## create the inverse of a matrix and assign to m (setinverted), and get the inverted matrix (getinverted)
      
      get <- function () x
      setinverted <- function(solve) m <<- solve 
      getinverted <- function() m 
      list(set = set, get = get, setinverted = setinverted, getinverted = getinverted) 
}

## This function calculates the inverse of a matrix "x". If the
## it has already been calculated and stored, 
## the function returns the stored inverted matrix.


cacheSolve <- function(x, ...){ # get the inverted matrix, considering if theres data in the cache or not.
      m <- x$getinverted()
      if(!is.null(m)) {     # if there is data in the cache, return m.
            message("getting data from cache")
            return(m)
      }    # if there is no data in the cache, create the inverted matrix and use setinverted to chache it. 
      data <- x$get()
      m <- solve(data, ...)    
      x$setinverted(m)     
      m   # return solved matrix
}

