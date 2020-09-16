# week-3-assign
mat_a <- matrix(c(1,2,3,4),2,2)
> #solve(mat_a)
> 
> ## Write a short comment describing this function
> ## The following functions cache the inverse of a matrix so that it
> ## can be calculated and stored, rather than having to be calculated
> ## each time it is needed.

> makeCacheMatrix <- function(x = matrix()) {
+   inv <- NULL
+   set <- function(y) {
+     x <<- y
+     inv <<- NULL
+   }
+   get <- function() x
+   setinv <- function(solve) inv <<- solve
+   getinv <- function() inv
+   list(set = set, get = get,
+        setinv = setinv,
+        getinv = getinv)
+ }
> 
> 
> ## Write a short comment describing this function
> ## The first function below: makeCacheMatrix, creates a special "matrix"
> ## object that can cache its inverse.

> cacheSolve <- function(x, ...) {
+         ## Return a matrix that is the inverse of 'x'
+   inv <- x$getinv()
+   if(!is.null(inv)) {
+     message("getting cached data")
+     return(inv)
+   }
+   data <- x$get()
+   inv <- solve(data)
+   x$setinv(inv)
+   inv
+ }
> 
> sol_mat_a <- makeCacheMatrix(mat_a)
> cacheSolve(sol_mat_a)
