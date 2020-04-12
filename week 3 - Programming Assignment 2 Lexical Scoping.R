#the following function supposed to cache the inverse of matrix in memory. 
#That must help in few cases: 
#when we need to avoid recalculations when matrix changes 
#or when we make the same request for inverse for the same matrix. 
#function is supposed to return list that contains 4 functions:
#set and get the matrix and then set and get the inverse, so it could be used in the next function.

makeCacheMatrix <- function(x = matrix()) {
        
        inverss <- NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inverss <<- inverse
        getInverse <- function() inverss
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


#next function is supposed to check cache to see if the matrix include inverse
#if in matrix there is no inverse, function is supposed to calculate it.

cacheSolve <- function(x, ...) {
        inverss <- x$getInverse()
       
         if (!is.null(inverss)) {
                
                message("in process of getting cached data")
                
                return(inverss)
         }
        
        m <- a$get()
        
        inverss <- solve(m, ...)
        
        x$setInverse(inverss)
        
        inverss
}



#checking if functions work
B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)