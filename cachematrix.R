## Computes the inverse of a square matrix and caches the result.
## If the computation is performed on the same matrix, the cached result is returned.
## As is, the result of makeCacheMatrix must be assignet to a variable. (y<- makeCacheMatrix(...))
## in order for cacheSolve(y) to function properly.
## Step 1. Create square matrix x
## Step 2. set y<-makeCacheMatrix(x)
## step 3. set z<-cachesolve(y) 
## step 4. Repeat step 3 to see that the return is from cache
## step 5. Repeat from Step 1 to see that the return is from calculation

## This has been tested on the following square matrix, x
## X has been made large to clearly demonstrate the time required to calculate.

x<-matrix(data=rnorm(n=1000000,mean=70,sd=35),nrow=1000,ncol=1000)


## makeCacheMatrix creates an empty matrix that will cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
               x <<- y
               m <<- NULL
          }
          get <- function() x
          setinverse <- function(solve) m <<- solve
          getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
     }
}


## cacheSolve will compute the inverse of the matrix if no inverse is cached  
## or get the cached inverse if it exists.  tm1 is added to show the difference in 
## computation time required.

cacheSolve <- function(x, ...) {
     tm1<-system.time({
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data,...)
     x$setinverse(m)
     m})
     message('time elapsed was:',tm1)
}

