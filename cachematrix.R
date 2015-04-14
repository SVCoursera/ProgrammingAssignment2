
########################################################
##
## makeCacheMatrix
## ---------------
##
## Function to cache the inverse of a matrix (x)
## returns a vector to the get and set functions:
## f_set
## f_get
## f_setinverse
## f_getinverse
##
## inputs: x - a valid NxN matirix
## 
########################################################

makeCacheMatrix <- function(x = matrix()) {
	    # reset inv
        inv <- NULL
        # define the set function
        f_set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # define the f_get function to 
        # return the matrix 
        f_get <- function() x
        # define the f_setinverse function to 
        # cache the inverse matrix 
        f_setinverse <- function(inverse) inv <<- inverse
        # define the f_getinverse function to
        # return the cached inverse matrix (inv)
        f_getinverse <- function() inv
         
        # return a vector to the functions defined above
        # maps set to the function f_set, get to f_get etc...
        list(
        	 set = f_set, 
        	 get = f_get,
             setinverse = f_setinverse,
             getinverse = f_getinverse
             )
        
        # End of function definition
}

########################################################
##
## CacheSolve
## ----------
##
## Function to calculate and return  the inverse
## of a matrix
##
## inputs: x - a valid NxN matirix
##
########################################################

cacheSolve <- function(x = matrix()) {
	    # First check to see if the matrix has been cached already
        inv <- x$getinverse()
        # if inv is not null then use the cached value returned
        if(!is.null(inv)) {
        	    # Message to inform the user that the cache is being used
                message("getting cached matrix")
                # return the cached inverse and quit 
                return(inv)
        }
        # Message to inform the user that the inverse is being calculated
        message("Calculating inverse matrix")
        # get the matrix to be inversed
        data <- x$get()
        # calculate the inverse of the matrix
        inv <- solve(data)
        # send the inverse to be cached
        x$setinverse(inv)
        # return the inverse 
        inv

        # End of function definition
}
