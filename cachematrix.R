## Put comments here that give an overall description of what your
## functions do
## ******************************************************************************
## makeCacheMatrix creates a matrix given an argument and sets its inverse to NULL.
## The functions getinverse(matrix) and setinverse(matrix) get and set the inverse 
## of the matrix that its initialized by.
##
## cacheSolve will take a matrix as an argument and will write its inverse to cache
## wherefrom it can then be accessed. 
##
## As an example : 
## 
##
##
##
##

## Write a short comment describing this function
## The function can be used to create a matrix like so:
## mat<-makeCacheMatrix(matrix(c(5,6,7,8), ncol=2, nrow=2))
## You can check to see its been created by doing 
## mat$get() and the 2x2 matrix with 5 6 7 8 as entries will be printed
## It's inverse will be set to NULL as is checked by
## mat$getinverse()
## 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## given a matrix as an argument this function will invert it
## and cache the resulting matrix
## You can test this with the matrix created in the comments above the previous example
## after you check mat$getinverse() do 
##  cacheSolve(mat)
## You'll see the inverse matrix printed out 
## and then do
## mat$getinverse()
## and instead of the previous NULL the inverse matrix will appear.
## The name of the inverse within cacheSolve is called "inverted" to avoid conflicts with the
## name "inverse" in makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverted <- x$getinverse()
        if(!is.null(inverted)) {
                message("getting cached data")
                return(inverted)
        }
        data <- x$get()
        inverted <- solve(data, ...)
        x$setinverse(inverted)
        inverted


}
