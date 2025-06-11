# Define the function
semicircle <- function(x) {
  1/(2*pi) * sqrt(4 - x^2)
}

# Generate x values from -2 to 2
x_vals <- seq(-2, 2, length.out = 1000)

# Compute y values
y_vals <- semicircle(x_vals)

# Plot the semicircle
plot(x_vals, y_vals, type = "l", lwd = 5, col = "red",
     xlab = "x", ylab = "Density", main = "Semicircle Distribution",
     ylim = c(0, 0.5), bty='n', cex.lab=1.4, cex.main=1.7)

# Add axis lines
abline(h = 0, v = 0, col = "gray", lty = 2)

