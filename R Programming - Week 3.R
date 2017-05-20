#
# Caching the inverse of a Matrix
#
# Two functions that are used to create a special object that stores a numeric
# matrix and cache's its inverse.
#


#
# makeCacheMatrix
#
# Function creates a special "matrix", which is really containing a function to
#
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean
#
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function()
                x
        setinverse <- function(inverse)
                i <<- inverse
        getinverse <- function()
                i
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}

#
# cacheSolve
#
# The following function calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the inverse has
# already been calculated.
# If so, it gets the mean from the cache and skips the computation.
# Otherwise, it calculates the mean of the data and sets the value
# of the inverse in the cache via the setinverse function.
#
# Sample Test :
# x <- matrix(c(1, 2, 3, 1, 0,-1, 2, 3, 0), nrow = 3, ncol = 3)
# m <- makeCacheMatrix(x)
# s <- cacheSolve(m)
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        i
}
