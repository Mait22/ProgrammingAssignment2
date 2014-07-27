## Those two functions are used to check if the inverse of given matrix has been already calculated and 
## been cached in global variable called “inverse”. 
## If the inverse of given matrix as already been stored in cache the inverse of matrix will not be re-calculated. 
## Instead the cached value of inverse is returned to user. 
## If the inverse has not yet been calculated the inverse will be calculated 
## and the globally cached value of inverse will be updated with newly calculated value of given matrix.
## Inversibility of input matrix is assumed. 


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



## Function checks if the value of inverse in global space is equal to NULL. 
## If the value of variable inverse does not equal NULL the inverse for given matrix has been already found it will not be –recalculated. Rather the cached result stored is retrieved and returned to user.
## If the inverse equals NULL the inverse for given matrix has not been calculated. 
## If the inverse of given matrix has not been calculated and cached the inverse of matrix will be 
## calculated by using solve function and stored in global variable inverse. 


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
