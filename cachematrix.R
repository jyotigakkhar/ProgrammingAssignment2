## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x)==nrow(x) && det(x)!=0) {
    m<-NULL
    set<-function(y){
      x<<-y
      m<<-NULL
    }
    get<-function() x
    setinverse <- function() m <<- inv(x)
    getinverse<-function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
  }

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m<-x$getinverse
  if (!is.null(m)) {
    return(m)
  }
  data<-x$get
  m <- inv(data, ...)
  x$setinverse(m)
  m
}
