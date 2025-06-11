# Wigner Surmise
WignerSurm <- function(s){
  (pi/2)*s*exp(-(pi/4)*s^2)
}

# Generating points to plot
s <- seq(0, 5, length.out = 1000)  
y <- WignerSurm(s)  

# Plotting the curve
plot(s, y, type = "l", col = "red", lwd = 3,
     xlab = "s", ylab = 'P(s)',
     main = "Wigner Surmise")


# Load necessary libraries
library(Matrix)
library(ggplot2)

# Parameters
N <- 50000  # Size of the GOE matrix

# Function to generate a GOE matrix
generate_GOE <- function(N) {
  # Generate a symmetric matrix with Gaussian entries
  H <- matrix(rnorm(N * N, mean = 0, sd = 1), nrow = N, ncol = N)
  H <- (H + t(H)) / sqrt(2)  # Symmetrize and normalize
  return(H)
}

# Generate a GOE matrix
H <- generate_GOE(N)

# Compute eigenvalues
eigenvalues <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
eigenvalues <- sort(eigenvalues)

# Compute spacings between consecutive eigenvalues
spacings <- diff(eigenvalues)
mean_spacing <- mean(spacings)
normalized_spacings <- spacings / mean_spacing

# Wigner surmise function
wigner_surmise <- function(s) {
  (pi * s / 2) * exp(-pi * s^2 / 4)
}

# Create a data frame for the normalized spacings
spacing_df <- data.frame(Normalized_Spacing = normalized_spacings)

# Plot the eigenvalue spacing histogram with Wigner surmise overlaid
ggplot(spacing_df, aes(x = Normalized_Spacing)) +
  geom_histogram(aes(y = ..density..), bins = 100, fill = "blue", color = "black", alpha = 0.6) +
  stat_function(fun = wigner_surmise, color = "red", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Eigenvalue Spacing Distribution for GOE Matrix",
    x = "Normalized Spacing (s)",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Title size
    axis.title.x = element_text(size = 16),  # X-axis title size
    axis.title.y = element_text(size = 16),  # Y-axis title size
    axis.text.x = element_text(size = 14),   # X-axis tick label size
    axis.text.y = element_text(size = 14)    # Y-axis tick label size
  ) +
  coord_cartesian(xlim = c(0, 5))





# Load necessary libraries
library(Matrix)
library(ggplot2)

# Parameters
N <- 10000  # Size of the GOE matrix (reduced for computational efficiency)

# Function to generate a GOE matrix with different standard deviations for each element
generate_GOE <- function(N) {
  # Generate a matrix of standard deviations (random or predefined)
  sd_matrix <- matrix(runif(N * N, 0.5, 5), nrow = N, ncol = N)  # Example: random SDs between 0.5 and 1.5
  
  # Generate a symmetric matrix with Gaussian entries, each with its own standard deviation
  H <- matrix(rnorm(N * N, mean = 0, sd = sd_matrix), nrow = N, ncol = N)
  H <- (H + t(H)) / sqrt(2)  # Symmetrize and normalize
  return(H)
}

# Generate a GOE matrix
H <- generate_GOE(N)

# Compute eigenvalues
eigenvalues <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
eigenvalues <- sort(eigenvalues)

# Compute spacings between consecutive eigenvalues
spacings <- diff(eigenvalues)
mean_spacing <- mean(spacings)
normalized_spacings <- spacings / mean_spacing

# Wigner surmise function
wigner_surmise <- function(s) {
  (pi * s / 2) * exp(-pi * s^2 / 4)
}

# Create a data frame for the normalized spacings
spacing_df <- data.frame(Normalized_Spacing = normalized_spacings)

# Plot the eigenvalue spacing histogram with Wigner surmise overlaid
ggplot(spacing_df, aes(x = Normalized_Spacing)) +
  geom_histogram(aes(y = ..density..), bins = 300, fill = "blue", color = "black", alpha = 0.6) +
  stat_function(fun = wigner_surmise, color = "red", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Eigenvalue Spacing Distribution for Symmetric Matrix",
    x = "Normalized Spacing (s)",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Title size
    axis.title.x = element_text(size = 16),  # X-axis title size
    axis.title.y = element_text(size = 16),  # Y-axis title size
    axis.text.x = element_text(size = 14),   # X-axis tick label size
    axis.text.y = element_text(size = 14)    # Y-axis tick label size
  ) +
  coord_cartesian(xlim = c(0, 4))
