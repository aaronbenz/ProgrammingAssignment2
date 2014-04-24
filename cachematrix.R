## Put comments here that give an overall description of what your
## functions do

## Initializes matrix type makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
    matrixINV <- NULL #sets as Null
    set <- function(y) { #sets value of matrix to y and set matrixINV to NULL
        x <<- y
    }
    get <- function() x #returns matrix x
    setinverse <- function(inverse) matrixINV <<- inverse #sets the inverse of matrix x
    getinverse <- function() matrixINV #returns the inverse of matrix x
    list(set = set, get = get,
         setinverse = setinverse, #returns list of all functions
         getinverse = getinverse)
}


## returns inv of matrix if exists, if not, solves it and caches inv of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrixINV <- x$getinverse()
    if(!is.null(matrixINV)) { #checking if cached index exists
        message("getting cached data")
        return(matrixINV) #if it does, returns it
    }
    data <- x$get() #stores the matrix as data
    matrixINV <- solve(data, ...) #calculates the inverse of the matrix
    x$setinverse(matrixINV)#caches the inverse of the matrix into x
    matrixINV #returns the inverse of the matrix
}
