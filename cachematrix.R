

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##          1. set the matrix
  ##          2. get the matrix
  ##          3. set the inverse
  ##          4. get the inverse
  ##        this list is used as the input to cacheSolve()
  inversion <- NULL
  set <- function(y){
    # use '<<-' to assign a value to an object in an environment
    # different from the current environment
    x <<- y
    inversion <<- NULL
  }
  get <- function() x
  setinversion <- function(matrixinversion) inversion <<- matrixinversion
  getinversion <- function() inversion
  list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}




cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  inversion <- x$getinversion()
  # if the inverse has already been calculated
  if(!is.null(inversion)){
    # get it from the cache and skip the computation
    message("getting cached data")
    return(inversion)
  }
  # otherwise, calculate the inverse
  data <- x$get()
  inversion <- solve(data, ...)
  # set the value of the inverse in the cache via the setinversion function
  x$setinversion(inversion)
  inversion
}

test <- function(x){
  ## @x: an invertible matrix
  a <- makeCacheMatrix(x)
  start_time <- Sys.time()
  cacheSolve(a)
  period = Sys.time() - start_time
  print(period)
  start_time <- Sys.time()
  cacheSolve(a)
  period = Sys.time() - start_time
  print(period)
}

set.seed(1234)
b <- rnorm(1000000)
matr <- matrix(b, nrow = 1000, ncol = 1000)
test(matr)
