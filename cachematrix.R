## The functions below will create a special matrix object and
## then calculate the inverse of the special matrix. 


## makeCacheMatrix creates a special matrix object than can 
## cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  #will store cached inverse matrix      
        set <- function(y) {  #1.set matrix value
                x <<- y
                inv <<- NULL
        }
        get <- function() x  #2.get matrix value
        setinverse <- function(inverse) inv <<- inverse  #3.set inverse value
        getinverse <- function() inv  #4.get inverse value        
        list(set = set, get = get,  #return list of functions
             setinverse = setinverse,
             getinverse = getinverse)
}



## cachesolve computes or retrieves the inverse of the special 
## matrix that was created by makeCacheMatrix. If the inverse 
## has been computed (and the matrix has not changed), then 
## cachesolve will retrieve it from the cache. If not already
## computed, cachesolve will compute the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Store the inverse from the special matrix in inv
        inv <- x$getinverse() 

        #If inverse already computed, return it
        if(!is.null(inv)) {  
                message("getting cached inverse matrix")
                return(inv)
        }
        
        #If inverse not compute it, this computes it
        matrix <- x$get()
        inv <- solve(matrix, ...)
       
        x$setinverse  #cache inverse
        inv  #return inverse
}
