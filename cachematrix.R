## makeCacheMatrix and cacheSolve work together to calculate a matrix inverse and store it for repeated access.

## Passing a given invertible matrix into makeCacheMatrix sets up a 4 item list which handles setting and
## retrieving values for the cacheSolve function.  The <<- operator allows use of the set and setinverse
## functions from cacheSolve (or other functions) to retain the original scope and affect the list object.

## cacheSolve then interacts with the list object to restrieve the matrix, check for the existence of an
## inverse (by means of the NULL assignment in the set function), and then either calculate the inverse
## and store it to the list, or return the inverse already stored in the list.



## makeCacheMatrix creates a list of 4 functions, built around the passed X which allow the user to:
## set and get an inputted matrix (set is not used in this configuration)
## set and get the inverse of that matrix
## when first executing, the function sets the inverse to NULL, which is then used as a check to
## see if an inverse has been calcuated.  The inv is also set to NULL when a new matrix is passed to the
## set function since that would invalidate any existing inverse cache

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        ## sets a new martix, clears the current cached inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## returns the matrix
        get <- function() x
        
        ## sets the inverse of the matrix, the <<- assignment allows it to be used from other scopes
        setinverse <- function(solve) inv <<- solve
        
        ## returns the inverse of the matrix (or NULL if not yet set)
        getinverse <- function() inv
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## CacheSolve returns the inverse of the matrix used to make the list in the makeCacheMatrix
## First checking for the inverse of the matrix stored in the created list
## if the inverse has been calculated before, then it will have a value, if not it will be the original NULL
## When inv is not NULL, the function returns that value
## if inv is NULL, then it gets the original matrix, uses the solve function to get the inverse,
## stores that inverse to the original list, and then returns the calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        ## if inv exists (not NULL), returns that value and notes that it was cached
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if inv doesnt yet exist, then get matrix, solve and store the inverse to the list
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        ## return the newly calculated inverse
        inv
}
