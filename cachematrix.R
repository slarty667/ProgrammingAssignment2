# cachematrix.R: matrices with cachable inverses for quick computing
#
# Usage:
# a <- makeCacheMatrix()        # initialize
# a$set(x)                      # set the matrix
# a$get()                       # get the matrix
# cacheSolve(a)                 # calculate the inverse matrix

# makeCacheMatrix - instantiate the cached matrix object
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL               # empty the cache
        
        # store the matrix in the object's variable and empty cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x     # return the stored matrix
        
        # store the inverse matrix in cache
        setinverse <- function(inverse) m <<- inverse
        
        # return the inverse matrix from cache
        getinverse <- function() m
        
        # function list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve - return the inverse matrix, either cached or freshly computed
cacheSolve <- function(x, ...) {
        m <- x$getinverse()     # fetch cached matrix
        
        # check if the cached matrix is a valid object
        if(!is.null(m)) {
                message("getting cached data") # notify user
                return(m)       # return the cached matrix
        }
        
        # cache didn't contain a valid object, so let's invert and cache
        data <- x$get()         # get the original matrix
        m <- solve(data, ...)   # inverse the matrix, store it to local variable
        x$setinverse(m)         # store the inversed matrix in object's cache
        m                       # return the inverse matrix
}
