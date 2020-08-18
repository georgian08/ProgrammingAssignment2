## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## to define the matrix, get the matrix, calculate the inverse of the matrix and 
## get the inverse of the matrix.

## solve() is the function used to find the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() {x}
  setInverse<-function(inverse) {inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
## It first checks to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the inverse to the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
