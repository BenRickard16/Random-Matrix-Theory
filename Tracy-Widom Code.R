
# Install and load the necessary package
if (!require("pracma")) install.packages("pracma", dependencies = TRUE)
library(pracma)
library(RMTstat)
# Function to generate Gaussian GOE matrices of size N
GOE_Matrix <- function(N) {
  # Diagonal elements
  diagonal <- rnorm(n = N, mean = 0, sd = 1) / sqrt(N)
  
  # Off-diagonal elements (symmetric)
  off_diagonal <- rnorm(n = N * (N - 1) / 2, mean = 0, sd = 1) / sqrt(N)
  
  # Create an empty N x N matrix
  GOE_matrix <- matrix(0, nrow = N, ncol = N)
  
  # Fill the diagonal
  diag(GOE_matrix) <- diagonal
  
  # Fill the off-diagonal elements
  index <- 1
  for (i in 1:(N - 1)) {
    for (j in (i + 1):N) {
      GOE_matrix[i, j] <- off_diagonal[index]
      GOE_matrix[j, i] <- off_diagonal[index]  # Symmetric entry
      index <- index + 1
    }
  }
  
  return(GOE_matrix)
}

# Parameters
n <- 500  # Number of GOE matrices
N <- 250    # Size of each GOE matrix

# Generate GOE matrices and extract the largest eigenvalue
largest_eigenvalues <- numeric(n)

for (i in 1:n) {
  GOE <- GOE_Matrix(N)
  eigenvalues <- eigen(GOE, only.values = TRUE)$values
  largest_eigenvalues[i] <- N^(2/3)*(max(eigenvalues)-2)
}


# Histogram of largest eigenvalues
hist(largest_eigenvalues, breaks = 30, col = 'cornflowerblue', border = 'black', prob=TRUE, 
     xlim = c(-4,2), main = "Largest Eigenvalue Density vs. Tracy-Widom Distribution", xlab = "Scaled Largest Eigenvalue", 
     ylab = "Density", ylim=c(0,0.4), cex.lab=1.4, cex.main=1.7)

# Generate x and y for the Tracy-Widom distribution
y = function(x){dtw(x, beta = 1)}  # Replace this with your Tracy-Widom density function if unavailable

# Plot the line
curve(y, from=-4, to=2, type = "l", col='red', lwd=5, add=TRUE)


# Add a legend
# Adding legend
legend("topright", 
       legend = c("Eigenvalue Distribution", "Tracy-Widom Distribution"), 
       col = c("cornflowerblue", "red"), 
       pch = c(NA, NA),        # `NA` means no symbol for the curve
       lwd = c(5, 5),         # Line width for the curve
       bty = "n",
       cex=1.3) 


