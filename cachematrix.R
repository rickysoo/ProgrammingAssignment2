## Functions to calculate and cache the inverse of a matrix

## Returns a a list of functions used to get and set a matrix object

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(inverse) inv <<- inverse
    
    get_inverse <- function() inv
    
    list(
        set = set,
        get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse
    )
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()

    if(!is.null(inv)) {
        message('Getting cached data...')
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}

# Example code

# First create a matrix m
m <- matrix(1:4, nrow = 2)

# Use m to initialize a matrix object f
f <- makeCacheMatrix(m)

# Check that the inverse is empty
f$get()
f$get_inverse()

# Calculate and store the inverse of matrix m in object f
cacheSolve(f)

# Check that the inverse is now calculated and cached
f$get()
f$get_inverse()

# Compare the output with that of solve()
solve(m)
