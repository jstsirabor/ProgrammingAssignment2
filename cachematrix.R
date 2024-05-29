# Define a class for our custom matrix to store the inverse
makeCacheMatrix <- function(x = matrix()) {
  # Check if x is a square matrix
  if (!is.square(x)) stop("Input matrix must be square.")
  
  # Create a new object of class "cacheMatrix"
  obj <- list(data = x, inv = NULL)
  
  # Assign class to the object
  class(obj) <- "cacheMatrix"
  
  return(obj)
}

# Method to check if a matrix is square
is.square <- function(mat) {
  nrow(mat) == ncol(mat)
}

# Method to compute the inverse of a cacheMatrix object
cacheSolve <- function(x,...) {
  # Check if x is a cacheMatrix object
  if (!inherits(x, "cacheMatrix")) stop("Input must be a cacheMatrix object.")
  
  # Check if the inverse has already been computed and cached
  if (is.null(x$inv)) {
    # Compute the inverse if not cached
    x$inv <- solve(x$data)
  }
  
  # Return the cached inverse
  return(x$inv)
}

# Example usage
# Create a 3x3 identity matrix
X <- diag(3)

# Wrap the matrix in a cacheMatrix object
myMatrix <- makeCacheMatrix(X)

# Now, calling cacheSolve on myMatrix will compute the inverse once and cache it,
# subsequent calls will retrieve the cached inverse without recomputation
inverse <- cacheSolve(myMatrix)
print(inverse)

