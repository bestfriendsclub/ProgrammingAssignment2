## makeCacheMatrix() creates an object with information about a matrix and fucntions used to edit and inspect that matrix
## cacheSolve() gets or sets then prints the inverse of makeCacheMatrix()'s matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x 
  setinv<-function(i){
    inv<<-i
  }
  getinv<-function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinv()
  if(!is.null(i)){
    message("getting chached inverse")
    return(i)
  }
  m<-x$get()
  i<-solve(m)
  x$setinv(i)
  i
}
