##Keith Buonanno
##Cousera - R Programming - Assignment#2

##Function creates a list that: 
##Sets the value of a matrix, gets the value of a matrix 
##Sets the value of matrix inverse, gets the value of matrix inverse 

makeCacheMatrix <- function(x = matrix()) {

cachedinv <- NULL
  set <- function(y) {
    x <<- y
    cachedinv <<- NULL
    }
  
  get <- function() x
  setinv <- function(solve) cachedinv <<- solve
  getinv <- function() cachedinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


##Function calculates the inverse of the matrix created with function "makeCacheMatrix.R"

cacheSolve <- function(x, ...) {

##Checks if mean of matrix has already been calculated   
  inv <- x$getinv()

##If the matrix inverse has previously been calculated, the function returns its cached value
  if(!is.null(inv)) {
    message("getting cached data...")
    return(inv)
    }

#If the matrix inverse has not been calculated, the solve() function calculates the inverse saves its value to cache  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv

}