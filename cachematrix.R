##makeCacheMatrix creates a special "matrix" object that can cache its inverse.
	## creates a list containing a function to set and get the value of the matrix
	## and to set and get the value of inversing the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
	## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## test:

x = rbind(c(1, -2/5), c(-2/5, 1))
m = makeCacheMatrix(x)
m$get()

##       [,1]  [,2]
## [1,]  1.0 -0.4
## [2,] -0.4  1.0

## 1st run = no cache

cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.1904762 0.4761905
## [2,] 0.4761905 1.1904762

## 2nd run = retrieve from cache

cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.1904762 0.4761905
## [2,] 0.4761905 1.1904762