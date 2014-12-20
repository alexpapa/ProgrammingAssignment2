## This function optimizes the computation of the inverse of a matrix
## that is assummed to be invertible by caching the inverse of that
## matrix so it does not have to be calculated again unless the
## matrix has changed

## This function creates an object containing the matrix to be inverted
## as well as its inverse, and all the methods that make it possible
## to get and set values for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
         mm<-NULL
         set <- function(y) {
                 x <<- y
                 mm <<- NULL
         }
         get <- function() x
         setinv <- function(solve) mm <<- solve
         getinv <- function() mm
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
 }


## This function calls the objects cached by makeCacheMatrix and 
## tests whether a new computation of an inverse is needed, in which
## case it computes the inverse using the relevant method defined
## within makeCacheMatrix, else it just returns the inverse matrix
## that is in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mm <- x$getinv()
        if(!is.null(mm)) {
                message("getting cached data")
                return(mm)
        }
        data <- x$get()
        mm <- solve(data)
        x$setinv(mm)
        mm
}
