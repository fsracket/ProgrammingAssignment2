#This file defines 2 functions that are intended to be used together.

#The first function 'makeCacheMatrix' takes a normal R matrix and produces
#a 'special' matrix that is able to cache the value of its inverse.  To
#get the caching behaviour, values of the 'special' matrix must be used
#with the 'cacheSolve' function, which is the second one defined in this file.


#This function produces a 'special' matrix that is able to cache
#its inverse.  The special matrix produced by this function
#allows the user to change the value of the original matrix that
#was passed to makeCacheMatrix.
makeCacheMatrix <- function(x = matrix()) {
  cachedMatrix <- NULL
   
  #gets the value of the cache
  getCachedMatrix <- function() cachedMatrix
  
  #sets the value of the internal cache
  #the fn does not call solve; it will simple store whatever value is 
  #passed to it
  setCachedMatrix <- function(aMatrix) { cachedMatrix <<- aMatrix}
  
  #gets the current matrix  
  get <- function() x
  
  #a function to set the value of the matrix
  #when the value of x changes, the existing cached inverse is invalid
  #to account for this, when 'x' changes, reset 'cachedMatrix' to null
  set <- function(aMatrix) { 
    x <<- aMatrix
    cachedMatrix <<- NULL
  }
  
  
  
  list(get = get, set = set, getCachedMatrix = getCachedMatrix,
       setCachedMatrix = setCachedMatrix)
  
}

#This function takes in a 'special' matrix created by 'makeCacheMatrix'
#and some set of arbitrary optional parameters (identical to the optional
#parmas passed to 'solve'), and returns the inverse of the matrix.  Repeated
#calls to 'cacheSolve' for the *same* matrix value will be very fast as
#the inverse is cached after the first call.
cacheSolve <- function(x, ...) {
  
  #parameter x is a 'special' matrix created by 'makeCacheMatrix'
  cachedInverse = x$getCachedMatrix()
  if (is.null(x$getCachedMatrix())) {
    x$setCachedMatrix(solve(x$get(), ...))  #if the cache is NULL
                                       #call solve and cache the value.
    
    #Note: because of the way 'set' is implemented in 'makeCacheMatrix'
    #everytime the main matrix value changes, the cache is reset to NULL
  }

  x$getCachedMatrix()
}
