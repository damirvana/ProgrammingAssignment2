## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix creates a square matrix that can cash (store) its computed invers. 
##
## cacheSolve either calculates the invers of the input Matrix created by the makeCacheMatrix or, if already calculated, returns the invers from the cache 
## provided by the makeCacheMatrix function .
## 

## Write a short comment describing this function
##
## 1.takes an vector with even number of numeric elements and creates a square Matrix and assigns it to the local variable x
## 2.creates the empty local variable m
## 3.nested setMatrix function uses the superassignment operator to override the value of local variables x and  m in the containing environment (here makeCacheFunction) 
## 4.nested setMatrixInvers function uses the superassignment operator to override the value of local variable m and assigns it with the invers of the input Matrix that has
## been computed by the cacheSolve function
##
## 5.getMatrixInvers function returns the value of m (either NULL or computed invers of Matrix), which is tested by the cacheSolve function. If NULL the invers is computed by
## the cacheSolve function. If not NULL (computed invers of matrix) than  calculation of invers of Matrix is omitted and returned from the "cache" (return value of function
## 6.getMatrixInvers in makeCacheMatrix)
## 7.a list is returned containing the aforementioned nested functions and the input matrix 

makeCacheMatrix <- function(x = numeric()) {
  x <- matrix(x, nrow=length(x)/2, ncol=length(x)/2)
  m <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    m <<-NULL
  }
  
  
  setMatrixInvers <- function(MatrixInvers) m <<- matrix(MatrixInvers, nrow=length(x)/2, ncol=length(x)/2)
  getMatrixInvers <-function()m
  list(setMatrix = setMatrix, setMatrixInvers = setMatrixInvers, getMatrix = x, 
       getMatrixInvers = getMatrixInvers)
  
}


## Write a short comment describing this function
## 1.takes the list from makeCacheMatrix function as input (via a global variable to which the makeCashMatrix function has been assigned)
## 2.cacheSolve function adresses the return value of the getMatrixInvers() function (variable as element of the  makeCacheMatrix function list), which is the value 
## of the invers of the input matrix (NULL or computed value). 
## 3. if return value of the getMatrixInvers() function has already been calculated (> NULL), invers calculation is skipped and instead the cashed invers value is returned
## (return value in the getMatrixInvers() function provided in the list from makeCacheMatrix function)
## 4.if return value of the getMatrixInvers() function is NULL (not computed yet), cacheSolve function calculates the invers of the input matrix. 
## For that the input matrix is retrieved from the list from the makeCacheMatrix function 
## 
cacheSolve <- function(x, ...) {
       
  m <- x$getMatrixInvers()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix
  m <- solve(data)
  x$setMatrixInvers(m)
  m
}
