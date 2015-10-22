## These two functios are used in order to cache an inverse matrix that has already been calculated in memory, since
## calculating again and again the inverse of a matrix is a time consuming process and should be avoided when
## the inverse has already been calculated.

## The first function (makeCacheMatrix) is a function that creates a list that contains 4 functions, each one doing the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This is the function that actually contains the calculation of the inverse of the matrix. If the inverse of the matrix has already been calculated
## the calculation does not have to be repeated and the result is taken from the cache memory (getinverse() if not NULL)
## If the inverse of the matrix is not in the cache, then the inverse is calculated with the solve function and cached with the setinverse() function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
