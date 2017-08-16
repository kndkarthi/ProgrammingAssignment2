# Programming Assignment 2 - R Programming

#Matrix inversion is usually a costly computation and there may
#be some benefit to caching the inverse of a matrix rather than 
#compute it repeatedly.The following pair 
#of functions will cache the inverse of a matrix.

#function description:
# makeCacheMatrix: This function creates a special "matrix" object
#that can cache its inverse.
# it creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inversed <<- inv
  getInverse <- function() inversed
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


#cacheSolve: This function computes the inverse of the special "matrix" returned
#by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

# sample run:
#x<-matrix(1:4,nrow = 2,ncol = 2)
#print(x)

#[,1] [,2]
#[1,]    1    3
#[2,]    2    4

#> m<- makeCacheMatrix(x)
#> s<- cacheSolve(m)
#>print(s)

#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#second run will give the cache message
#> s2<-cacheSolve(m)
#getting cached data.
#> s2
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
 