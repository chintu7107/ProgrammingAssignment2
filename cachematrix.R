makeCacheMatrix<- function(x = matrix()) {            ##creates a special matrix
        inofx<- NULL
        set <- function(y) {                          ##sets the value of the matrix
                x <<- y
                inofx<<- NULL
        }
        get <- function() x                           ##gets the value of the matrix
        setinv <- function(inverse) inofx <<- inverse ##sets the inverse of the matrix
        getinv <- function() inofx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


cacheSolve <- function(x, ...) {
        inofx <- x$getinv()
        if(!is.null(inofx)) {                         ##checks if the inverse had already been calculated
                message("getting cached data")
                return(inofx)
        } 
        data <- x$get()
        inofx <- solve(data, ...)                     ##solve function is used to find the inverse of the matrix
        x$setinv(inofx)
        inofx
}