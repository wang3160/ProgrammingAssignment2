## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special "Matrix", which is really 
##a list containing a function to

##set the value of the Matrix
##get the value of the Matrix
##set the value of the Inverse Matrix
##get the value of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inverse_x <- NULL
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_x <<- inverse
        getinverse <- function() inverse_x
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
##The following function calculates the inverse matrix of the special "Matrix" created with 
##the above function. However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, 
##it calculates the inverse matrix of the "x" the value of the inverse matrix in the cache 
## via the setinverse function.



cacheSolve <- function(x, ...) {
        inverse_x <- x$getinverse()
        if(!is.null(inverse_x)) {
                message("getting cached data.")
                return(inverse_x)
        }
        data <- x$get()
        inverse_x <- solve(data)
        x$setinverse(inverse_x)
        inverse_x
        ## Return a matrix that is the inverse of x
}
