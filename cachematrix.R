## Those two functions will calculate the inverse of given matrix nd will store the 
## functions do



## This function returns the list of functions to set the value of matrix, get the value of 
## matrix, set the value of inverse of given matrix by 
## using function solve() and get the value of previously calculated inverse of matrix. 
makeCacheMatrix <- function(x = matrix()) {
       inverse <- NULL
       set <- function(y) {
              x <<- y
              inverse <<- NULL
       }
       get <- function() x
       setinverse <- function(solve) inverse <<- solve
       getinverse <- function() inverse
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}



## Function checks if the value of inverse in global space is equal to "NULL". 
## If it is not and the inverse for given matrix has been already found it will not re-calculate the 
## inverse of matrix rather the cached result stored is retrieved and returned to user.
## If the inverse equals "NULL" this means the inverse for given matrix has not been calculated. 
## In this case the inverse of matrix will be calculated by using solve function and global variable inverse 
## value will be set
## from NULL to the calculated inverse value of matrix using function setinverse. 

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
                }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}
