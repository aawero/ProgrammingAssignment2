## The following two functions are functions that cache and compute
## the inverse of a matrix

## The first function creates a matrix that computes and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                    m<- NULL
                    set<- function(y){
                          x<<-y
                          m<<- NULL
                    }
                    get<-function() x
                    setinv<-function(solve) m<<- solve
                    getinv<- function() m
                    list(set=set, get=get, setinv=setinv, getinv=getinv)
                    
}


## The following function calculates the inverse of the matrix
## created with the above function, but it first checks if the
## inverse of the matrix already exists, in that case the function
## 'cachesolve' will retrieve the inverse of the matrix
## from the cache.

cacheSolve <- function(x, ...) {
              m<-x$getinv()
              if(!is.null(m)) {
                message("getting cached data")
                return(m)
              }
              data<-x$get()
              m<-solve(data, ...)
              x$setinv(m)
              return(m)
        ## Return a matrix that is the inverse of 'x'
}
