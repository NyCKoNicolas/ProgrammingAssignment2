## The first function takes a matrix and converts it to cache and define the use of solve to make the inverse
## The second function applys the first function in order to invert a matrix

## Convertion of a matrix to cache and definition of methods that use solve

makeCacheMatrix <- function(x = matrix()) {
        # Sets the value of m to null, by default
        m <- NULL
        
        # Set the value of the matrix
        setmatrix <- function(y){
                # Caches the matrix that assign to use cacheSolve
                x <<- y
                # Sets m to null for cacheSolve used
                m <<- NULL
        }
        # Definition of the functions
        # Get the original matrix and return of the same
        getmatrix <- function() x
        
        # Override of the matrix and return of the inverse
        setinverse <- function(solve) m <<- solve
        
        # Get the inverse matrix and return
        getinverse <- function() m
        
        # Transformation by the ccreation of the list of the four functions
        list(setmatrix = setmatrix
             , getmatrix = getmatrix
             , setinverse = setinverse
             , getinverse = getinverse)
}


## Function that use a variable to solve the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Comparison with the ancient matrix
        m <- x$getinverse() # Verification of previous inversed
        # Check for cacheSolve if has been run before
        if(!is.null(m)){
                # Message of the task 
                message("Getting cache data.")
                # Return the original matrix
                return(m)
        }
        # That the case of m is NULL then...
        # Message of the task of calculating
        message("Newly calculating data")
        # Asignation to data of the original matrix
        data <- x$getmatrix()
        # Asignation to m of the matrix that is in data
        m <- solve(data, ...)
        # Use of the method setting the invers matrix of the m matrix
        x$setinverse(m)
        # Return it
        return(m)
}
