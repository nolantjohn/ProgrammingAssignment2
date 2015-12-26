## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
        
        ## Instantiate a variable to store the matrix inverse
        m <- NULL
        
        ## Sets a new value for the underlying matrix (x)
        ## This invalidates the cached inversed matrix (m)
        ## <<- operator sets the value of x and m in the enclosing environment (created when
        ## makeCacheMatrix was first called and x / m were created), not in the environment local to set()
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Gets the underlying matrix (x)
        get <- function() x
        
        ## Caches the matrix inverse calculated in cacheSolve
        setinverse <- function(inverse) m <<- inverse
        
        ## Gets the cached matrix inverse
        getinverse <- function() m

        ## Return value of the makeCacheMatrix function
        ## List of functions that we want to expose as public and accessible
        ## Any variables declared above, but are not exported here, are private and inaccessible
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...){
        
        ## Gets the value stored for this matrix, either NULL or the inverse matrix
        m <- x$getinverse()
        
        ## If m is not NULL, then this returns the m, the cached inverse matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Assigns the underlying matrix (x) to the variable data
        data <- x$get()
        
        ## Inverts the matrix and stores the value in m
        m <- solve(data, ...)
        
        ## Calls the function that caches the inverse matrix for x, which is stored in m
        x$setinverse(m)
        
        ## Returns the inverse matrix (m)
        m
}