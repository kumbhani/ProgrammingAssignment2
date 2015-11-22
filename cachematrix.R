# Coursera - R Programming
# Week 3: Programming Assignment #2
# Romesh Kumbhani
# November 22, 2015

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # initialize inverse to null
        inv <- NULL
        
        # set method: sets the current matrix
        set <- function(newmatrix) {
                x <<- newmatrix
                inv <<- NULL
        }
        
        # get method: gets the current matrix
        get <- function() x
        
        # setinverse method: sets the inverse value (does not compute the inverse!)
        setinverse <- function(newinverse) inv <<- newinverse
        
        # getinverse method: gets the inverse value
        getinverse <- function() inv
        
        # return list of methods
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by 
# the `makeCacheMatrix` above. If the inverse has already been calculated 
# (and the matrix has not changed), then `cacheSolve` should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get the current inverse value
        inv <- x$getinverse()
        
        # if the current inverse wasn't null, it means it was cached. Immediately
        # return the cached value.
        if(!is.null(inv)) {
                message("Retrieving cached inverse.")
                return(inv)
        }
        
        # Since the inverse wasn't cached, let's compute it.
        # Get the current matrix
        curmatrix <- x$get()
        # solve its inverse
        inv <- solve(curmatrix, ...)
        # set the inverse as the new cached inverse.
        x$setinverse(inv)
        # return the newly computed inverse
        inv
}