## With the combination of the following functions
## users should be able to create a special type
## of matrix; store it to cache; and subsequently
## calculate the matrix's inverse. NOTE: The 
## functions assume the matrix is square, and can
## be inverted. 

## makeCacheMatrix will create a special matrix using
## a matrix already stored in your environment as a 
## value. e.g., if you created a matrix using 
## 'a <- matrix(1:4, 2, 2)', you could use 
## 'b <- makeCacheMatrix(a)' to store 'a' as a cached
## value, in this case a matrix named 'b'.

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) xinv <<- solve
    getinv <- function() xinv
    list(set=set, get=get,
         setinv=setinv,
         getinv=getinv)
}


## cacheSolve will calculate the inverse of a matrix
## stored as a specific value. The function also checks
## to see if the inverse was already calculated and
## cached, and if so will skip the processing time,
## and simply return the cached value, saving you
## time.

cacheSolve <- function(x = matrix(), ...) {
    xinv <- x$getinv()
    if(!is.null(xinv)){
        message("getting cached inverse")
        return(xinv)
    }
    matrix <- x$get()
    xinv <- solve(matrix, ...)
    x$setinv(xinv)
    xinv
}
