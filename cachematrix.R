## Function makeCacheMatrix define a list of helpfull functions. Function cacheSolve uses this list to compute an inverse of matrix.


## This function defines a list of function - set, get, setinverse and getinverse. It helps us compute inverse of a matrix x.
makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse 
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



##This function computes inverse of a matrix M. The right argument is x = makeCacheMatrix(M). However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I

}

