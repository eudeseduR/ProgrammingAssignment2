## Caching the inverse of a matrix

## makeCacheMatrix function creates a matrix object to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        ## Repository for caching values.
        mInv <- NULL 
        
        ## Create the matrix in the working environment.
        set <- function(y) { 
                x <<- y
                mInv <<- NULL
        }
        
        ## Return a list containing functions to
        ## 1. set the matrix;
        ## 2. get the matrix;
        ## 3. set the inverse;
        ## 4. get the inverse.
        ## This list is used as the input to cacheSolve().
        get <- function() x
        setinv <- function(inv) mInv <<- inv
        getinv <- function() mInv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {

        ## Get the inverse of the matrix stored.
        mInv <- x$getinv()
        
        # Return the inverse of matrix from cache.
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        
        # Calculate the inverse of matrix if it exist. 
        data <- x$get()
        mInv <- solve(data, ...)
        x$setinv(mInv)
        mInv
}

## Testing the function.

## Load matrix values.
> testMatrix <- matrix(runif(9,15,150),3,3)

## Generate the makeCacheMatrix object with testMatrix.
> testMatrixCached <- makeCacheMatrix(testMatrix)

## Retrieve the inverse of matrix calculated using the cacheSolve function.
> testMatrixInv <- cacheSolve(testMatrixCached)
> testMatrixInv
            [,1]         [,2]         [,3]
[1,] -0.01083678  0.019503353 -0.001919216
[2,]  0.03090845 -0.018212666 -0.002748595
[3,] -0.03328351  0.007479378  0.015823126
