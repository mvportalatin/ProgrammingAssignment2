## Function to create a matrix to cache the inverse of a matrix

## this function will create a list of functions to
## get and set the value of the data and the inverse

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
       x <<- y
       m_inv <<- NULL
   }                                               #set value of matrix
   get <- function() x                             #get value of matrix
   setinverse <- function(solve) m_inv <<- solve   #set value of the inverse
   getinverse <- function() m_inv                  #get value of the inverse
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)                   #create list of functions
}


## This function takes the matrix from above and returns its
## value if cached, if not compute it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinverse()                     #get value of inverse
        if(!is.null(m_inv)) {
            message("getting cached data")
            return(m_inv)                           #if set return it
        }
        data <- x$get()                     #otherwise set value of matrix
        m_inv <- solve(data, ...)           #compute inverse of data
        x$setinverse(m_inv)                 #set inverse value
        m_inv                               #return inverse
}
