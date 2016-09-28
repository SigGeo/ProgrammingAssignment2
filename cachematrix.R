## Coursera, Data Science, R Programming, Week 3.
## Programming Assignment 2: Lexical Scoping.
## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## I'm creating a new variable called inversion.It holds the value of matrix inverse.
    inversion <-NULL
    
    set <-function(y) {
        x <<- y
        inversion <<- NULL
    }
    get <-function() x
    
    setinverse <-function(inverse) inversion <<- inverse
    getinverse <-function() inversion
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inversion <- x$getinverse()
    
    if(!is.null(inversion)) {
        message("getting cached data")
        return(inversion)
    }
    
    data <- x$get()
    inversion <- solve(data, ...)
    x$setinverse(inversion)
    inversion
}

