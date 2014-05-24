## This script is comprised of two functions: makeCacheMatrix and cacheSolve.  The overall
## purpose of this script is to calculate an inverse matrix when given a matrix and to store the
## resulting inverse matrix in a cache that can be recalled the next time the inverse matrix is 
## needed. The function makeCacheMatrix is used to create a special matrix object that can be used 
## in cacheSolve.  The function cacheSolve tests to see if an inverse matrix has been cached, and 
## either returns the inverse matrix in the cache or calculates the inverse matrix, stores it in
## the cache, and returns it.  When the input matrix is changed, using either the makeCacheMatrix() 
## or $setMatrix() commands (see below in Usage), the cache is cleared and cacheSolve will 
## calculate and return a newly calculated inverse matrix.  If the inverse matrix is in the 
## cache and the input matrix has not changed, then cacheSolve will return the cached inverse
## matrix rather than recalculating it. 

## Usage:
## After loading the script's two functions below, run the following test commands:
# >example = makeCacheMatrix(matrix(c(1,2,1,2,3,4,3,4,1), nrow=3, ncol=3))  #initializes functions 
# comprised in makeCacheMatrix for the input matrix 
# >example$getMatrix()          # Returns original matrix
# >cacheSolve(example)          # Computes, caches, and returns matrix inverse
# >example$getInverseMatrix()   # Returns matrix inverse
# >cacheSolve(example)          # Returns cached matrix inverse using previously 
#computed matrix inverse

# >example$setMatrix(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modifies the input matrix
# >example$getMatrix()          # Returns new input matrix
# >cacheSolve(example)          # Computes, caches, and returns new matrix inverse instead of 
# returning previously cached inverse matrix from above
# >example$getInverseMatrix()   # Returns newly calculated matrix inverse
# >cacheSolve(example)          # Returns the most recently cached matrix inverse

################################################################################################


## The makeCacheMatrix function initializes the storage variable (inverse.cache) and returns 
## a list of 4 functions: setMatrix, getMatrix, setInverseMatrix, and getInverseMatrix

makeCacheMatrix <- function(x = matrix()) {
    inverse.cache <- NULL       ## initialize the storage variable
    
    setMatrix <- function(y) {  ## Function 1 will overwrite the existing 
        x <<- y                     ## matrix with new matrix
        inverse.cache <<- NULL  ## assignment of new matrix clears the cache
    }
    getMatrix <- function() x   ## Function 2 retrieves the input matrix x 
    
    setInverseMatrix <- function(inverse) inverse.cache <<- inverse  ##Function 3
                                    ## assigns the input inverse matrix to the 
                                    ## inverse.cache storage variable
    getInverseMatrix <- function() inverse.cache  ##Function 4
                                    ## retrieves the inverse matrix cache
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)  ##Returns a list of the 4 functions   
}


## The cacheSolve function pulls in the value stored in inverse.cache from the above function 
## (either NULL if the inverse has not yet been calculated or the calcalated inverse matrix).
## The function tests if the value of inverse.cache is "NULL".  If not, it returns the inverse
## matrix stored in inverse.cache.  If the value of inverse.cache is "NULL", then the function 
## pulls in the original matrix using the $getMatrix function from the above function.  It will 
## then calculate the inverse matrix using the solve() function, store the inverse in the cache
## by using the $setInverseMatrix function from the makeCacheMatrix function, and then return the
## inverse matrix.


cacheSolve <- function(x, ...) {
    
    inverse.cache <- x$getInverseMatrix()  ## pull in the inverse.cache value 
     
    if(!is.null(inverse.cache)) {       ## test if a calculated matrix is in the cache 
        message("getting cached data")  ## if so, prints a message to console
        return(inverse.cache)           ## return inverse matrix value
        
    } 
    X <- x$getMatrix()          ## retrieve the input matrix 
    inverse <- solve(X)         ## calculate the inverse matrix for input matrix
    x$setInverseMatrix(inverse) ## cache the inverse matrix
    inverse                     ## Returns a matrix that is the inverse of 'x'   
}

