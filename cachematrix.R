## makeCacheMatrix function giving list of: 
## function that sets a matrix
## function that gets the matris
## set the inverse of a matrix
## get the inverse of a matrix

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL 
  set <- function(y) {   
    a <<- y
    inv <<- NULL
  }
  get <- function() a
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##check for solve -gives inverse of square matrix
#m<- matrix(1:4,2,2)
#solve(m)
#library(matrixcalc)
#matrix.inverse(m)
#identical(solve(m),matrix.inverse(m))

# the below function will only work for square matrix
# function cacheSolve checks if the inverse of matix is already calcute and is there in cache,
# if so it will get the inverse from cache or else calsu late the inverse and 
# set that into cache  for nex time.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv) # Returns inverse if matrix is already cached
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv #Retuns inverse of a matrix if not cached
}

#Test result
#x=matrix(1:4,2,2)
#y=matrix(5:8,2,2)
#m<-makeCacheMatrix(x)
#n<-makeCacheMatrix(y)
#Run cacheSolve more than once interchangibly with m and n 
#after 1st run(once cache is prepared) it will fetch inverse from cache.
#cacheSolve(m)
#cacheSolve(n)
