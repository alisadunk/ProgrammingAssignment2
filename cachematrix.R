## takes a matrix and stores cache of its inverse in $getinverse
makeCacheMatrix <- function(x = matrix()) {
        y <- solve(x) #variable to hold inverse of the matrix  
        get <- function() {x}  
        set <- function(x_new) {    
             x <<- x_new     
             y <<- solve(x_new)  
         }  
        getinverse <- function() {y}  
        list(get=get, set=set, getinverse=getinverse)
}


## takes function makeCacheMatrix as input and returns inverse of its
## member $get
## if matrix passed already has an inverse  in the cache object## returns cached value 
## if matrix doesn't have cache produces new inversion
cacheSolve <- function(x, ...) {
        r <- x$getinverse()  
        if(!is.null(r)) { 
                message("getting cached inverse of matrix") 
                return(r)  
        }  
        message("caching the inverstion of the matrix")  
        x$set(x)  
        cacheSolve(x)
}

