#The first function, This function creates a special "matrix" object that can cache its inverse.
#makeCacheMatrix creates a special "vector", which is really a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the MatrixInverse
# get the value of the MatrixInverse
makeCacheMatrix <- function(x=matrix()){
        inv_mat <- NULL
        
        #setting inverse of matrix to  null initially
        set <- function(y){
                x <<- y
                inv_mat <<- NULL
        }
        
        #function to get the value of matrix
        get <- function() x
        
        #function to set the value of inverse
        setInverse <- function(solMat) inv_mat <<- solMat
        
        #function to set the value of inverse
        getInverse <- function() inv_mat
                
        #creating the special vector containing list of functions
        list(set=set,get=get,setInverse = setInverse, getInverse = getInverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
#retrieve the inverse from the cache.
cacheSolve <- function(x,...){
        m<-x$getInverse()
        
        #checking for the cached of the matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #getting matrix data
        data <- x$get()
        #getting the inverse of matrix
        m <- solve(data,...)
        
        #setting the inverse of the matrix
        x$setInverse(m)
        m
        
}
