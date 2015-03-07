## function to create and manage a matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    seti <- function(mss) m <<- mss
    geti <- function() m
    list(set = set, get = get,
         set_inverse = seti,
         get_inverse = geti)
}


## calculate the inverse matrix or getting cached data
cacheSolve <- function(x, ...) {
    m <- x$get_inverse()
    if(!is.null(m)) {
        message("getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
}
