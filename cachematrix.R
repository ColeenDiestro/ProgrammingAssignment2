## This assignment is about writing a function that cache the inverse of a matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        in <- NULL
        set <- function(m){
                x<<-m
                in<<-NULL
        }
        get <-function()x
        setinverse <- function(inverse) in <<- inverse
        getinverse <- function() in
        list (set = set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        in <- x$getinverse()
        if (!is.null(in)){
                message("getting cached data")
                return(in)
        }
        matrix <- x$get()
        in <- solve(matrix, ...)
        x$setinverse(in)
        in
}
