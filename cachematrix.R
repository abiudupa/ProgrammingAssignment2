## Caching Inverse of a matrix

## Matrix inversion is a costly computation and it benefits to cache the inverse rather than compute it repeatedly

## Creates a special matrix object that can cache its inverse
## It returns a list of getter-setter functions for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        #Variable to store inversed matrix
        i <- NULL
        
        #Matrix setter
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #Matrix getter
        get <- function() x
        
        #Inversed matrix setter
        setinverse<- function(inverse) i <<- inverse
        
        #Inversed matrix getter
        getinverse <- function() i
        
        #Return list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() returns the inverse of the matrix. 
## It first checks if the inverse has already been computed. 
## If yes, gets result and skips computation. 
## If no, computes inverse, sets cache value through setinverse function.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        #If inverse already computed, return inverse
        if (!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        
        #If inverse is not computed, compute it
        data <- x$get()
        i <- solve(data, ...)
        
        #Cache the inverse
        x$setinverse(i)
        
        #Return cached inverse
        i
}

## Example:
## > m <- matrix(...)         //Create matrix
## > cm <- makeCacheMatrix(x) //Create cached matrix
## > cacheSolve(cm)           //Return inverse 
## > cacheSolve(cm)           //Return cached inverse