##If we need to use an inverse of a matrix repeatedly, we should cache it
##instead of calculating it again and again.

##Create a a list of 4 functions. They are set, get, setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
    inverse_mat <- NULL
    ## Usually we don't need to use set, but if we need to change the matrix,
    ## we can achieve this by makeCacheMatrix$set(new_matrix)
    set <- function(y){
        x <<- y
        inverse_mat <<- NULL
    }#set
    get <- function() x
    setinverse <- function(inverse_matrix) inverse_mat <<- inverse_matrix
    getinverse <- function() inverse_mat
    ## Important! Give each function a name so that I can use '$' to call it.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## If the inverse has already been calculated, return the inverse directly.
## If not, calculate the inverse.

cacheSolve <- function(x, ...) {
    inverse_mat <- x$getinverse()
    if (!is.null(inverse_mat)){
        message("getting cached data")
        return(inverse_mat)
    }#if
    data <- x$get()
    inverse_mat <- solve(data, ...)
    x$setinverse(inverse_mat)
    inverse_mat
}