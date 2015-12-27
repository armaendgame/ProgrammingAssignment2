
## Making a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mCache <- NULL
  ##To create a martix and storing into working environment
  set <- function(y){
    x <<- y ##set value
    mCache <<- NULL
  }
  ##Get values from matrix
  get <- function () x
  ##Invert matrix and store into cache
  setMatxInv <- function (inverse) mCache <<- inverse
  ##Get inverted matrix saved in cache
  getMatxInv <- function() mCache
  ##Returns list with above functions
  list (set = set, get = get, 
        setMatxInv = setMatxInv, 
        getMatxInv = getMatxInv)

}

## Function that retrieves the inverse or calculates it using cache functions created.

cacheSolve <- function(x, ...) {
  ##Get inverse of matrix in cache
  mCache <- x$getMatxInv()
  ##If cache has values it is returned
  if(!is.null(mCache)){
    message ("getting cached data")
    return(mCache)
  }
  ## If cached was empty it is calculated
  matrx <- x$get()  ##Get values from matrix
  mCache <- solve(matrx, ...) ##Calculate inverse
  x$setMatxInv(mCache) ##Set resutls
  mCache ## Return a matrix that is the inverse of 'x'
}
