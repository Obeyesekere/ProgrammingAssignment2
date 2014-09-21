
#Together, makeCacheMatrix and cacheSolve calculate the inverse of a matrix and store it in a variable. 
#So long as the input matrix remains unchanged, the inverse stored in this 'cache' variable will be used 
#for all subsequent requests for the inverse, rather than undertaking costliy re-computation each time. 


#This function takes a matrix as its input and returns a list of 4 functions. It also defines a cache 
#variable. The 4 functions in the list allow the retrieval and change of the input matrix and 
#the cache variable. Whenever the input matrix is changed, the cache variable is reset to NULL. 

makeCacheMatrix <- function(x = matrix()) {
      cache <- NULL
      set <- function(y) {                      #function for changing the input matrix
            x <<- y
            cache <<- NULL                      #resets the cache variable whenever the input matrix is changed
      }
      get <- function() x                             #function for retrieving the input matrix
      setinverse <- function(inv) cache <<- inv       #function for changing the content of the cache variable
      getinverse <- function() cache                  #function for retrieving the cache variable
      
      list(set = set, get = get,                  #generate a list of the 4 functions defined above 
           setinverse = setinverse,
           getinverse = getinverse)
}


#This function takes, as an input, a list of 4 functions constructed by the makeCacheMatrix function.
#It uses these to get the inverse of the matrix that we are interested in: First it checks if the 
#relevant inverse has already been cached (and returns it, if so). If it hasn't been cached, this function 
#computes the inverse, stores it in the cache, and then returns it. 
cacheSolve <- function(x, ...) {
      cache <- x$getinv()
      
      #This 'if' statement checks whether there is a matrix inverse already stored in the cache, and if so, 
      #displays it. We do not have to check whether this is the correct inverse (before displaying it), 
      #because whenever the input matrix is changed via the $set function, the cache is reset to NULL. 
      if(!is.null(cache)) {
            message("getting cached data")
            return(cache)
      }
      
      data <- x$get()         #retrieve the input matrix
      cache <- solve(data)    #calculate the inverse
      x$setinverse(cache)     #store the inverse in the cache
      cache
}
