## There are two functions makeCacheMatrix and cacheSolve which will help us
## compute the inverse of a matrix if it's not present in the cache, however 
## if the inverse is present in the chache then it used rather than expensive
## computation for inverse of matrix

## The makeCacheMatrix function returns a list of four function that help you 
## to set / get a matrix arguemnt "mat" or setInverst / getInverse of the matrix

makeCacheMatrix <- function(mat){
  inverse <- NULL
  
  #function to set a new matrix
  set <- function(newmat){
    mat <<- newmat
    inverse <<- NULL
  }
  
  #function to get the matrix
  get <- function() {
    mat
  }
  
  #function to set the inverse of this matrix
  setInverse <- function(invmat){
    inverse <<- invmat
  }
  
  #function to get the inverse this matrix
  getInverse <- function(){
    inverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The cacheSolve matrix will take the function list create by makeMatrixCache
## function and check if the inverse of the matrix already exists in the 
## environment if it does then that inverse matrix is returned else it will
## compute the inverse of the matrix

cacheSolve <- function(funcList,...){
  inverse <- funcList$getInverse()
  
  if(!is.null(inverse)){
    message("Cached Inverse of this matrix returned")
    return(inverse)
  }
  
  mat <- funcList$get()
  inverse <- solve(mat)
  
  funcList$setInverse(inverse)
  
  inverse
}

