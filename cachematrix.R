## cachematrix calculates and caches a matrix's inverse

## makeCacheMatrix(x) returns a list of functions for inspecting and editing x (a matrix)
makeCacheMatrix <- function(x = matrix()) {
  ## inv is the cache for x's inverse
  inv<-NULL
  ## set(y) changes x to y and clears inv
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  ## get() returns x
  get<-function() x 
  ## setinv(i) caches i to inv
  setinv<-function(i){
    inv<<-i
  }
  ## getinv() returns inv
  getinv<-function() inv
  ## makeCacheMatrix() returns a list of functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## cacheSolve(y, ...) returns the inverse of makeCacheMatrix(x)$get() (x)
cacheSolve <- function(y, ...) {
  ## i is y's (makeCacheMatrix(x)'s) cached inverse
  i<-y$getinv()
  ## if i is not null
    ## cacheSolve(y, ...) returns i (a cached, current inverse matrix)
  if(!is.null(i)){
    message("getting chached inverse")
    return(i)
  }
  ## if i is null
    ## m is y's matrix parameter (x)
  m<-y$get()
    ## i is m's inverse
  i<-solve(m)
    ## y's matrix parameter is i
  y$setinv(i)
    ## cacheSolve(y, ...) returns i
  i
}
