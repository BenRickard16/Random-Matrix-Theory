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
