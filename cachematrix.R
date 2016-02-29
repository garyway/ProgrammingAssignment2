## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The following two functions have the objective of caching the 
## inverse of a matrix in order facilitate this benefit.

## The function 'makeCacheMatrix' creates a special matrix object that can
## cache its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize cache of inverse matrix
        
        cache <- NULL
        
        ## Method to set matrix
        
        set_Matrix <- function(matrix) {
                x <<- matrix
                cache <<- NULL
        }
        
        ## Method to get matrix
        
        get_Matrix <- function() {x}
        
        ## Method to inverse matrix
        
        set_Inverse_Matrix <- function(Inverse_Matrix) {
                
                cache <<- Inverse_Matrix
        }
        
        ## Method to get inverse matrix
        
        get_Inverse_Matrix <- function() {cache}
        
        ## Return method list
        
        list(set_Matrix = set_Matrix, get_Matrix = get_Matrix,
             set_Inverse_Matrix = set_Inverse_Matrix,
             get_Inverse_Matrix = get_Inverse_Matrix)
}


## The function 'cacheSolve' computes the inverse matrix of the special "matrix" 
## returned by makeCacheMatrix' above. If the inverse matrix has already been 
## calculated (and the matrix has not changed), then the 'cacheSolve' should 
## should retrieve the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return inverse matrix
        
        cache <- x$get_Inverse_Matrix()
        
        ## Return inverse matrix if it already exists
        
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        
        ## Get matrix
        
        data <- x$get_Matrix()
        
        ## Compute inverse matrix
        
        cache <- solve(data, ...) 
        
        ## Set inverse matrix
        
        x$set_Inverse_Matrix(cache)
        
        ## Return inverse matrix
        
        cache
        
}
