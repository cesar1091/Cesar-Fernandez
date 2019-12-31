# Cesar-Fernandez
ProgrammingAssignment2 _ Cesar Fernandez
# Install and load packages

#install.packages("matlib")
library(matlib)

## Put comments here that give an overall description of what your functions do
# This file contains two main functions: mackeCacheMatrix and cacheSolve. The first function
# creates an object in which a matrix and its inverse is stored. The second function creates the inverse
# of the input matrix when the inverse matrix is not cached in the makeCacheMatrix object.

## Write a short comment describing this function
# The object function in general stores a matrix and its inverse and has four different functionalities. 
# 1. (re)setting of the input matrix (set)
# 2. retrieving of the stored input matrix (get)
# 3. (re)setting of the inverse of the input matrix
# 4. retrieving of the stored inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function (y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x   # get the matrix
  set_inverse <- function(inverse) inv_matrix <<- inverse
  get_inverse <- function () inv_matrix
  list(set = set,   ## gives the name 'set' to the set() function defined above -> allows to extract with $ operator
       get = get,   
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}  

## Write a short comment describing this function
# the function checks whether a cached inverse exists within the makeCacheMatrix object.
# if this is true then the function returns the cached inverse matrix together with a message that the return is cached data.
# if not true the function generates an inverse matrix from the input matrix stored in the makeCacheMatrix function, stores this matrix in the inverse object 
# of the makeCacheMatrix and returns (displays) the inverse matrix in the R console

cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inverse()
  if (!is.null(inv_matrix)) {
    message ("getting cached data")
    return (inv_matrix)
  }
  matrixdata <- x$get()
  inv_matrix <- inv(matrixdata, ...)
  x$set_inverse(inv_matrix)
  inv_matrix
}

#checking code 
set.seed(5)
imat <- makeCacheMatrix(matrix(rnorm(25),5,5))
imat$get()
imat$get_inverse()

cacheSolve(imat)
