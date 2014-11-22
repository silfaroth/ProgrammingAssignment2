## Put comments here that give an overall description of what your
## functions do

## Calculating the inverse of a matrix is computationally expensive and can really slow a program down if
## it has to be done repeated for the same matrix (for example in a loop). It makes sense to cache the result
## so that you only have to do it once and can then look up the result the next time you need it. 
##
## The makeCacheMatrix and cacheSolve functions allow you to do this.
## They are used to create a special "matrix" that stores an assumed invertable matrix and caches its 
## inverse matrix along with the functions to access these matrices.


## Write a short comment describing this function

## The makeCacheMatrix function creates a special "matrix" to hold the matrix to be inversed and a cached 
## copy of the result to save having to recalculate it. It also has a list containing functions to get and
## set the matrix and it's inverse.
##
## The function contains: 
##      x a variable for the matrix to be inverted
##      inversematrix a variable for the cached copy of the inverted matrix which is initial empty
##      get function to return the inital matrix
##      set function to store the inital matrix and reset the cached inverse to NULL
##      getinverse function to return the inverse matrix
##      setinverse function to calculate the inverse matrix and store it

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inversematrix <<- solve
        getinverse <- function() inversematrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## The cacheSolve function calculate the inverse of the special "matrix" created by the makeCacheMatrix
## function above. It first checks to see if the inverse has already be calculated. If so it prints the 
## message "getting cached data" and returns the cached inverse skipping the calculation step. Otherwise
## it gets the matrix, stores it in data and calculates the inverse matrix. This is then saved to the 
## special "matrix" cache before being returned by this function.

## Note: you can test the fuction by feeding the inverse matrix into the solve function to get back to
##       the starting matrix using:
##                              A <- matrix(c(2, 4, 3, 1, 5, 7, 7, 4, 2), nrow=3, ncol=3)
##                              specialMatrix <- makeCacheMatrix()
##                              specialMatrix$set(A)
##                              solve(cacheSolve(specialMatrix))

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)) {
                message("getting cached data")
                return(inversematrix)
        }
        data <- x$get()
        inversematrix <- solve(data, ...)
        x$setinverse(inversematrix)
        inversematrix
}
