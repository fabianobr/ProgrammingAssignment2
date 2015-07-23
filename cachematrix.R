## The "makeCacheMatrix" function creates a special "matrix" which is really a list containing a set of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix  

makeCacheMatrix <- function(inputMatrix = matrix()) {
        cachedInvertedMatrix <- NULL
        set <- function(y) {
                inputMatrix <<- y
                cachedInvertedMatrix <<- NULL
        }
        get <- function() inputMatrix
        setCachedInvertedMatrix <- function(inverse) cachedInvertedMatrix <<- inverse
        getCachedInvertedMatrix <- function() cachedInvertedMatrix
        list(set = set, get = get,
             setCachedInvertedMatrix = setCachedInvertedMatrix,
             getCachedInvertedMatrix = getCachedInvertedMatrix)
}


## The "cacheSolve" function calculates the "inverted matrix" (or solve function) of the special "matrix" created with the "makeCacheMatrix" function.
## It first checks to see if the inverted matrix has already been calculated.
## If so, it gets the inverted matrix from the cache and skips the computation.
## Otherwise, it calculates the inverted matrix (solve function) of the data and sets the value of the inverted matrix in the cache via setCachedInvertedMatrix function. 

cacheSolve <- function(x, ...) {
        invertedMatrix = x$getCachedInvertedMatrix()
        
        if (!is.null(invertedMatrix)) {
                message("getting cached inverted matrix data")
                return(invertedMatrix)
        }
        
        input.data <- x$get()
        invertedMatrix = solve(input.data, ...)
        
        x$setCachedInvertedMatrix(invertedMatrix)
        
        return(invertedMatrix)
}
