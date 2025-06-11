x <- c(-2, -sqrt(2), 0, sqrt(2), 2)
y <- c(2, 4, 4, 4, 2)/16

# Create a data frame
eigenvalue_df <- data.frame(Eigenvalue = x, Frequency = y)
library(ggplot2)
# Plot as a bar plot (manual frequencies)
ggplot(eigenvalue_df, aes(x = Eigenvalue, y = Frequency)) +
  geom_bar(stat = "identity", fill = "cornflowerblue", color = "black", width=0.1) +
  labs(title = "Histogram of Eigenvalues",
       x = "Eigenvalue",
       y = "Density") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),   
    axis.text.y = element_text(size = 14)    
  )


generate_symmetric_matrices <- function(n) {
  # Number of independent elements in the upper triangle (including diagonal)
  num_entries <- (n * (n + 1)) / 2
  
  # Generate all possible binary (-1,1) configurations for these elements
  configs <- expand.grid(replicate(num_entries, c(-1, 1), simplify = FALSE))
  
  matrices <- list()
  
  for (i in 1:nrow(configs)) {
    upper_triangle <- configs[i, ]
    mat <- matrix(0, n, n)
    
    # Fill the upper triangle and diagonal
    mat[upper.tri(mat, diag = TRUE)] <- as.numeric(upper_triangle)
    
    # Reflect to fill the lower triangle
    mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
    
    matrices[[i]] <- mat
  }
  
  return(matrices)
}

# Generate all symmetric 5x5 matrices with elements -1,1
symmetric_matrices <- generate_symmetric_matrices(5)

# Compute eigenvalues for each matrix
eigenvalues_list <- lapply(symmetric_matrices, eigen, only.values = TRUE)

# Extract and round eigenvalues
eigenvalues_rounded <- lapply(eigenvalues_list, function(e) round(e$values, digits = 1))

# Flatten the list of eigenvalues
all_eigenvalues <- unlist(eigenvalues_rounded)

# Create a frequency table
eigenvalue_freq_table <- table(all_eigenvalues)

# Print the frequency table
print(eigenvalue_freq_table)

# Convert the frequency table to a data frame
eigenvalue_df <- as.data.frame(eigenvalue_freq_table)
colnames(eigenvalue_df) <- c("Eigenvalue", "Frequency")

# Convert Eigenvalue to numeric (ensure it's correctly formatted)
eigenvalue_df$Eigenvalue <- as.numeric(as.character(eigenvalue_df$Eigenvalue))

# Normalize frequencies
eigenvalue_df$Normalized_Frequency <- eigenvalue_df$Frequency / sum(eigenvalue_df$Frequency)

# Check the Eigenvalue data
print(eigenvalue_df)

# Plot the normalized histogram
ggplot(eigenvalue_df, aes(x = Eigenvalue, y = Normalized_Frequency)) +
  geom_bar(stat = "identity", fill = "cornflowerblue", color = "black", width=0.1) +
  labs(title = "Histogram of Eigenvalues",
       x = "Eigenvalue",
       y = "Density") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Rotated x-axis labels
    axis.text.y = element_text(size = 14)    
  )


# Function to generate a symmetric matrix with elements -1, 1
generate_large_symmetric_matrix <- function(n) {
  # Create a random matrix of -1, 1 values for the upper triangle (including diagonal)
  mat <- matrix(sample(c(-1, 1), n * n, replace = TRUE), nrow = n, ncol = n)
  
  # Make the matrix symmetric by reflecting the upper triangle to the lower triangle
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  
  return(mat)
}

# Generate a 5000x5000 symmetric matrix with -1 and 1
large_matrix <- generate_large_symmetric_matrix(5000)

# Scale the matrix by 1/sqrt(5000)
scaled_matrix <- large_matrix / sqrt(5000)

# Compute eigenvalues of the scaled matrix
eigenvalues <- eigen(scaled_matrix)$values

# Plot the histogram with normalized frequencies
ggplot(data.frame(Eigenvalue = eigenvalues), aes(x = Eigenvalue)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "cornflowerblue", color = "black") +
  labs(title = "Histogram of Eigenvalues",
       x = "Eigenvalue",
       y = "Density") +  # Change y-axis label to 'Density'
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 14)
  )




# Define the matrix
matrix <- matrix(c(0.746, 1.259, -0.647,
                   1.259, -1.351, 3.394,
                   -0.647, 3.394, 0.239), 
                 nrow = 3, byrow = TRUE)

# Scale the matrix by 1/sqrt(3)
scaled_matrix <- matrix / sqrt(3)

# Compute the eigenvalues of the scaled matrix
eigenvalues <- eigen(scaled_matrix)$values

# Print the eigenvalues
print(eigenvalues)

# Create a data frame with eigenvalues for plotting
eigenvalue_df <- data.frame(Eigenvalue = eigenvalues)

# Normalize frequencies manually (divide by total count to get relative frequency)
eigenvalue_df$Frequency <- table(eigenvalues)[as.character(eigenvalue_df$Eigenvalue)]

# Calculate normalized frequency
eigenvalue_df$Normalized_Frequency <- eigenvalue_df$Frequency / sum(eigenvalue_df$Frequency)

# Plot the empirical eigenvalue distribution with normalized frequencies
ggplot(eigenvalue_df, aes(x = Eigenvalue, y = Normalized_Frequency)) +
  geom_bar(stat = "identity", fill = "cornflowerblue", color = "black", width=0.1) +
  theme_minimal() +
  labs(title = "Empirical Eigenvalue Distribution",
       x = "Eigenvalue",
       y = "Density") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Title size
    axis.title.x = element_text(size = 16),  # X-axis title size
    axis.title.y = element_text(size = 16),  # Y-axis title size
    axis.text.x = element_text(size = 14),   # X-axis tick label size
    axis.text.y = element_text(size = 14)    # Y-axis tick label size
  )


