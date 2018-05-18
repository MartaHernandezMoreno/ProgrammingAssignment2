
# Matrix inversion is usually a costly computation. Because of that, it is a
# good idea write functions to cache the inverse of a matrix.
# The following functions calculate the inverse of a matrix and cache it.
# (to write them, I use makeVector and cachemean functions as the basis)


# makeCacheMatrix function creates a special "matrix", which is a list
# containing a function to:
#    1. set the value of the matrix
#    2. get the value of the matrix
#    3. set the value of the inverse
#    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. It first checks to see if the inverse has already been
# calculated:
#    If so, it gets the inverse from the cache and skips the computation.
#    If not, it calculates the inverse of the data using solve(X) function and
#    sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


# Two examples to check that the functions work correctly:

# 2x2 MATRIX EXAMPLE:

## > x1<-rbind(c(1,-1),c(1,1))
## > m1 = makeCacheMatrix(x1)
## > m1$get()
##      [,1] [,2]
## [1,]    1   -1
## [2,]    1    1

# First run (not cached)

## > cacheSolve(m1)
##      [,1] [,2]
## [1,]  0.5  0.5
## [2,] -0.5  0.5

# Second run (cached)

## > cacheSolve(m1)
## getting cached data
##      [,1] [,2]
## [1,]  0.5  0.5
## [2,] -0.5  0.5



# 3x3 MATRIX EXAMPLE:

## > x2<-rbind(c(1,1,0),c(1,0,1),c(0,1,0))
## > m2 = makeCacheMatrix(x2)
## > m2$get()
##      [,1] [,2] [,3]
## [1,]    1    1    0
## [2,]    1    0    1
## [3,]    0    1    0

# First run (not cached)

## > cacheSolve(m2)
##      [,1] [,2] [,3]
## [1,]    1    0   -1
## [2,]    0    0    1
## [3,]   -1    1    1

# Second run (cached)

## > cacheSolve(m2)
## getting cached data
##      [,1] [,2] [,3]
## [1,]    1    0   -1
## [2,]    0    0    1
## [3,]   -1    1    1

