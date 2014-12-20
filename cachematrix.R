## This function optimizes the computation of the inverse of a matrix
## that is assummed to be invertible by caching the inverse of that
## matrix so it does not have to be calculated again unless the
## matrix has changed

## This function creates an object containing the matrix to be inverted
## as well as its inverse, and all the methods that make it possible
## to get and set values for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## setting the "container" of the inverse to value NULL each time the 
        ## code updating the matrix is run
        mm<-NULL   

        ## defining the method for setting/updating the matrix to be inverted
        ## nb: sets the mm variable to null so next time cacheSolve is called
        ## it knows the computation of matrix inverse needs to run again
        set <- function(y) {
                 x <<- y
                 mm <<- NULL
         }

        ## defining functions for getting the value of the original matrix,
        ## computing its inverse and getting its inverse from cache
        get <- function() x
        setinv <- function(solve) mm <<- solve
        getinv <- function() mm

        ## listing all methods that can be accessed by a calling function
        ## (in this case cacheSolve)
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
        ## Get from cache the value of the inverse matrix
        mm <- x$getinv()
        
        ## if that value is **not** NULL, it means the original matrix
        ## has remained the same, so all we have to do is use the 
        ## value we just assigned to the mm variable by using the
        ## method of makeCacheMatrix that allows this function to get the
        ## inverse matrix from cache
        if(!is.null(mm)) {
                message("getting cached data")
                return(mm) ## stops execution and returns the cached inverse
        }
        
        ## The following lines are executed only if the cached inverse was
        ## found to be NULL, so we need to s compute the inverse of the original
        ## matrix by calling from cache the original matrix and using the method
        ## of makeCacheMatrix that allows us to store the inverse in cache
        
        data <- x$get()
        mm <- solve(data)
        x$setinv(mm)
        message("inverse matrix just computed for you")
        mm #this returns the inverse of the original matrix
}
