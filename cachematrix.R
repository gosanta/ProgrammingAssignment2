## There are 2 functions 'makeCacheMatrix' and 'cacheSolve' to determine the inverse of
## matrix. 

## The 'makeCacheMatrix' sets inital square matrix (2*2), (3*3) and so on. It then gets the 
## value of matrix by accessing the variable passed as an argument into function. It is 
## followed by setting solve function and getting the same. Solve is used to calculate the
## inverse of matrix

makeCacheMatrix <- function(x = matrix()) {  ## Passed argument is a blank matrix. When we call the
                                             ## program, we pass actual non-singular square matrix
  m <- NULL         ## Part of Global Environment
  set <- function(y) {   ## Set the square matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x    ## Get the matrix assigned above
  setsolve <- function(solve) m <<- solve    ## Use the solve funtion and change the scope from
                                             ## global to local environment
  getsolve <- function() m      ## Get the Solve function value
  list(set = set, get = get,    ## The list will output 4 different functions to get, set matrix
       setsolve = setsolve,     ## and then set and get function to compute inverse. Each element of
       getsolve = getsolve)     ## list is a function (think it as 4 different methods returned by 
                                ## the function 'makeCacheMatrix')
}


## The 'cacheSolve' function will check whether a cached value of expensive inverse function
## exists. If we are using for the same matrix values, we check for cached value and print the
## same. Else we compute (also valid for first time). The input arguement will be the called function
## 'makeCacheMatrix' with input square matrix

cacheSolve <- function(x, ...) {
        
    m <- x$getsolve()   ## The above function 'makeCacheMatrix' is passed on to x argument here
                        ## i.e. in 'cacheSolve' function. We refer to 'getSolve' element in list which
                        ## is a function to get output of solve function in previous 'setSolve' step
    if(!is.null(m)) {       ## Check whether a cached version of solve exists and if yes, return the same
      message("getting cached data")
      return(m)
    }
    data <- x$get()     ## If no cached value exists, get the matrix passed on earlier by calling the 
                        ## get() function by subsetting appropriate element of list
    m <- solve(data, ...)   ## A single parameter for solve function => inverse function for data above
    x$setsolve(m)       ## Set the Inverse matrix output to global environment variable 'm'
    m                   ## and return 'm'
}