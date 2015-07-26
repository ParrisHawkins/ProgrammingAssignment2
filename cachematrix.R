## Function to compute and cache a matrix inverse
## First call matrixCache <- makeCacheMatrix(matrix) to cache the input matrix
## Then call matrixInverse <- cacheSolve(matrixCache) to compute and subsequently return the cached inverse
## Note if matrix changes you must call matrixCache(matrix) before calling cacheSolve for the first time

## Create the functions needed to cache the inverse Matrix
makeCacheMatrix <- function(matrix = matrix()) {
        inverseMatrix <- NULL
        set <- function(matrix) {
                matrix <<- matrix
                inverseMatrix <<- NULL
        }
        get <- function() matrix
        setInverse <- function(inverseMatrix) inverseMatrix <<- inverseMatrix
        getInverse <- function() inverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Create the function to compute the inverse and subsequently return the cached value
cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        ##  If the inverseMatrix is null, then it has not yet been computed
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        matrix <- x$get()
        inverseMatrix <- solve(matrix,...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
