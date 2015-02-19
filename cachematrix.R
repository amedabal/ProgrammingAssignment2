#The function, makeCacheMatrix creates a special "matrix", which is really a list containing the following functions
#function to set the value of the matrix
#funtion get the value of the matrix
#funtion to set the value of the inverse of the matrix
#funtion to get the value of the inverse of thhe matrix
# to get a inverse of a non-square matrix include the below library
library(MASS)

makeCacheMatrix <- function(x = matricx()) {
        #setting the invrs to NULL as a placeholder for a future value
        invrs <- NULL
        #defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, invrs, to NULL
        
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        # returns the matrix x
        get <- function() x
        # sets the invrs to inverse 
        setinverse <- function(inverse) invrs <<- inverse
        
        # returns the inverse
        getinverse <- function() invrs
        #returns all the functions defined above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
        invr<-x$getinverse()
        if(!is.null(invr)){
                message("getting cached data")
                return(invr)
        }
        # get the matrix into Z        
        data<-x$get()
        #using solve if it is a square matrix
        if   (det(data) != 0) {invrs <- solve(data, ...)}
        else {invrs <- ginv(data)}
        
        x$setinverse(invrs)
        invrs
}
