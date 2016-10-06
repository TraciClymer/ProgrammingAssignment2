## The functions makeCacheMatrix and cacheSolve take a square matrix as input, calculates its inverse and caches the value

## makeCacheMatrix creates a list containing functions to set the value of the vector, get the value of a vector, set the value of the mean, and get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL   #initialize i for inverse
        set <- function(y) {
                x <<- y    #assign input to object x in parent environment
                i <<-NULL  #assign NULL to object i in parent environment
        }
        get <- function() x   #retrieves the value of x from the parent environment
        setInverse <- function(inverse) i <<- inverse   #assign input to i from parent environment
        getInverse <- function() i   #retrieves the value of x 
        list(set = set,   
             get = get,
             setInverse = setInverse,
             getInverse = getInverse) #assigns each function as an element in a list
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix, if the inverse has already been calculated it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getInverse() #retrieve values from makeCacheMatrix()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mx <- x$get()
        i <- solve(mx, ...) #calculate inverse
        x$setInverse(i) #Return inverse matrix of x
        i
}