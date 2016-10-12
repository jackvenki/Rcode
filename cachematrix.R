## makeCacheMatrix creates a matrix. The matrix can also be set using the set function in the makeCacheMatrix
## The inverse of the matrix is set to NULL and the inverse can be directly passed throught setinv function

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y){
         x<<-y
         inv <<- NULL
       }
       get <- function() {
         x
       }
       setinv <- function(inver){
         inv <<- inver
       }
       getinv <- function() {
         inv
       }
       list(set = set, get=get, setinv = setinv , getinv = getinv)
      

}


## This function calculates the inverse of the matrix created by 
## makeCacheMatrix above. If the inverse has already been calculated and the 
## matrix has not changed, then it should retrieve the inverse from the cache

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
       inv <- x$getinv()
       
       if(!is.null(inv)){
         message("getting catched data")
         return(inv)
       }
       
       data <- x$get()
       print(data)
       inv <- solve(data)
       x$setinv(inv)
       inv
}
