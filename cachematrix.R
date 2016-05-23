makeCacheMatrix <- function(x=matrix())
  ## This function, makeCacheMatrix, creates a special "matrix", which is really a list containing:
  ##1. set the value of the matrix
  ##2. get the value of the matrix
  ##3. set the value of the inverse
  ##4. get the value of the inverse
  {
  m<- NULL
  set <- function(y) {
    x<<-y
    m<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x,...)
  ##This function, cacheSolve, calculates the inverse of the special
  ##matrix created by the makeCacheMatrix function. 
  ##If the inverse of the matrix was already solved, it does not calculate 
  ##again and returns the cached value.
  ##If the this is the first calculation, it calculates the inverse and
  ##caches it for later use.
{
  m<-x$getInverse()
  if(!is.null(m)){
          message("getting cached data")
          return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  print(m)
}