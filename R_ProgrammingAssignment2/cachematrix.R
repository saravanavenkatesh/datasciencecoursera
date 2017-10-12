## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  transpose<-NULL
  set<-function(y){
    x<<-y
    transpose<<-NULL
  }
  get<-function(){x}
  settranspose<-function(trans=matrix()){transpose<<-trans}
  gettranspose<-function(){transpose}
  return(list(set=set,get=get,settranspose=settranspose,gettranspose=gettranspose))

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mattrans<-x$gettranspose()
  if(!is.null(mattrans)){
    print("Value obtained from the cache")
    return(mattrans)
  }
  else{
    matns<-solve(x$get())
    x$settranspose(matns)
    return(x$gettranspose())
    
  }
}
