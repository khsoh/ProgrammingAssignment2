## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#
# Create a cacheable matrix object that:
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of the inverse of the matrix
# 4. gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

    # Assigns NULL to variable that stores the cached inverse 
    invx <- NULL

    # define the set function to save the value of the matrix
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }

    # define the get function to get the value of the matrix
    get <- function() x

    # define the setinverse function to set and cache the value
    #  of the inverse of the matrix
    setinverse <- function(inverse_mt) invx <<- inverse_mt

    # define the getinverse function to get the cached value
    # of the inverse of the matrix
    getinverse <- function() invx

    # Returns the list of functions for accessing the matrix and its inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # Get the inverse matrix object
    invx <- x$getinverse()
    if (!is.null(invx)) {
        # Non-null return - so we can use the cached object
        return(invx)
    }

    ## Cached inverse is not present - need to compute the inverse
    ## using solve()
    data <- x$get()
    invx <- solve(data, ...)

    ## Cache the inverse 
    x$setinverse(invx)


    ## Return the inverse
    invx
}
