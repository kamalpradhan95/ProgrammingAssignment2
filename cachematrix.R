#The first function, This function creates a special "matrix" object that can cache its inverse.
#makeCacheMatrix creates a special "vector", which is really a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the MatrixInverse
# get the value of the MatrixInverse
makeCacheMatrix <- function(x=matrix()){
        inv_mat <- NULL
        
        set <- function(y){
                x <<- y
                inv_mat <<- NULL
        }
        
        get <- function() x
        setInverse <- function(solMat) inv_mat <<- solMat
        getInverse <- function() inv_mat
        list(set=set,get=get,setInverse = setInverse, getInverse = getInverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
#retrieve the inverse from the cache.
cacheSolve <- function(x,...){
        m<-x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
        
}
