# Function to generate Gaussian Wigner matrices of size N
Wigner_Matrix <- function(N, sigma) {
  # Diagonal elements
  diagonal <- rnorm(n = N, mean = 0, sd = sigma) / sqrt(N)
  
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

# Function which returns the count of eigenvalues (1dp) of a matrix
eigenvalue_distn <- function(matrix) {
  # Calculate eigenvalues
  eigen_result <- eigen(matrix)
  
  # Extract and round eigenvalues
  eigenvalues <- round(eigen_result$values, digits = 1)
  
  # Create a frequency table of eigenvalues and their counts
  return(table(eigenvalues) / sqrt(nrow(matrix)))  # Normalize by the number of eigenvalues
}

plot_eigen <- function(N, sigma){
  eigen_distn <- eigenvalue_distn(Wigner_Matrix(N, sigma))
  plot(eigen_distn, main = "Eigenvalue Distribution", xlab = "Eigenvalues", ylab = "Frequency", 
       ylim = c(0, max(eigen_distn) * 1.1))
}

par(mfrow = c(1, 3))

plot_eigen(20, 1)
plot_eigen(200, 1)
plot_eigen(3000, 1)
