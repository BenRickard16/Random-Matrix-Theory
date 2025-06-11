# Function to generate Gaussian Wigner matrices of size N
Wigner_Matrix <- function(N) {
  # Diagonal elements
  diagonal <- rnorm(n = N, mean = 0, sd = 1) / sqrt(N)
  
  # Off-diagonal elements (symmetric)
  off_diagonal <- rnorm(n = N * (N - 1) / 2, mean = 0, sd = 1) / sqrt(N)
  
  # Create an empty N x N matrix
  wigner_matrix <- matrix(0, nrow = N, ncol = N)
  
  # Fill the diagonal
  diag(wigner_matrix) <- diagonal
  
  # Fill the off-diagonal elements
  index <- 1
  for (i in 1:(N - 1)) {
    for (j in (i + 1):N) {
      wigner_matrix[i, j] <- off_diagonal[index]
      wigner_matrix[j, i] <- off_diagonal[index]  # Symmetric entry
      index <- index + 1
    }
  }
  
  return(wigner_matrix)
}

# Plotting the histogram of eigenvalues and semicircle

# Semicircle distribution
y = function(x){1/(2*pi)*sqrt(4-x^2)}

# Plotting eigenvalues histogram
hist(eigen(Wigner_Matrix(5000))$values, breaks=100, xlim=c(-2,2), ylim=c(0,0.4), prob=TRUE, col='cornflowerblue',
     xlab='Eigenvalues', main='Eigenvalue Distribution vs. Semicircle Distribution', cex.lab=1.4, cex.main=1.7)

# Adding the semicircle distribution
curve(y, from=-2, to=2, type = "l", col='red', axes=FALSE, lwd=5, add=TRUE)

# Adding legend
legend("topright", 
       legend = c("Eigenvalue Distribution", "Semicircular Disitribution"), 
       col = c("cornflowerblue", "red"), 
       pch = c(NA, NA),        # `NA` means no symbol for the curve
       lwd = c(5, 5),         # Line width for the curve
       bty = "n",
       cex=1.3) 
