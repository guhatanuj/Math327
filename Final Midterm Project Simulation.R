# Final Midterm Project Simulation.R

# Purpose: Simulate data for the in-class question where
# the fitted regression model is:
#   Final.hat = 11 + 0.53*Midterm + 1.2*Project
# where the Midterm exam is scored from 0 to 100
# and the project is scored from 0 to 30.

# Set up 3D surface plotting

lm_surface <- function(lmfit, n=10, alpha=0.2, ...) { 
  coeffs = lmfit$coefficients
  f = function (x1, x2) coeffs[1] + coeffs[2]*x1 + coeffs[3]*x2
  ranges <- rgl:::.getRanges()
  x <- seq(ranges$xlim[1], ranges$xlim[2], length=n)
  y <- seq(ranges$ylim[1], ranges$ylim[2], length=n)
  z <- outer(x,y,f)
  surface3d(x, y, z, alpha=alpha, ...)
}

library(rgl)

# Different ways to vary this simulation (the "levers") are:
#  1. Set the range of values for the Midterm
#  2. Set the range of values for the Project
#  3. Set the residual standard error
#  4. Set the sample size

# First data using would might be considered a typical range of
# values for the Midterm (60-100) and the Project (18-30).

n = 100
sig.e = 5
Midterm = runif (n, 60, 100)
Project = runif (n, 18, 30)
Final = 11 + 0.53 * Midterm + 1.2 * Project + rnorm (n, 0, sig.e)

sim.df = data.frame (Midterm, Project, Final)
plot (sim.df)

# 3D scatterplot

plot3d (Midterm, Project, Final, type="p", col="red", 
        xlab="Midterm", ylab="Project", zlab="Final")
lm_surface(m1)

m1 = lm (Final ~ Midterm + Project)
summary (m1)
predict (m1, list (Midterm=100, Project=30), interval = 'confidence')

lm_surface (m1)

# Let's rescale the Project score to be 0 to 100 (from 0 to 30)

Project100 = Project * 10/3

m1a = lm (Final ~ Midterm + Project100)
summary (m1a)
predict (m1a, list (Midterm=100, Project100=100), interval = 'confidence')

# What if we standardize both predictor variables to have mean=0 and sd=1?

sim.df$Midterm.std = scale (sim.df$Midterm)[,1]
sim.df$Project.std = scale (sim.df$Project)[,1]

plot (sim.df)

m2 = lm (Final ~ Midterm.std + Project.std, data=sim.df)
summary (m2)
predict (m2, list (Midterm.std=(100 - mean (Midterm))/sd (Midterm), 
                   Project.std=(30 - mean(Project))/sd (Project)), interval = 'confidence')

# 3D Plot with the standardized data

plot3d (Midterm.std, Project.std, Final, type="p", col="red", 
        xlab="Midterm std", ylab="Project std", zlab="Final")
lm_surface(m1)

# Above:  Midterm has more association with Final than Project

#############################################################
# Now try using a narrower range for Midterm

Midterm2 = runif (n, 80, 100)
Project2 = runif (n, 10, 30)
Final2 = 11 + 0.53 * Midterm2 + 1.2 * Project2 + rnorm (n, 0, sig.e)

sim.df2 = data.frame (Midterm2, Project2, Final2)
plot (sim.df2)

# 3D scatterplot

plot3d (Midterm2, Project2, Final2, type="p", col="red", 
        xlab="Midterm", ylab="Project", zlab="Final")

m3 = lm (Final2 ~ Midterm2 + Project2, data=sim.df2)
summary (m3)
predict (m3, list (Midterm2=100, Project2=30), interval = 'confidence')

# What if we standardize the predictor variables?

sim.df2$Midterm.std = scale (sim.df2$Midterm2)[,1]
sim.df2$Project.std = scale (sim.df2$Project2)[,1]

plot (sim.df)

m4 = lm (Final2 ~ Midterm.std + Project.std, data=sim.df2)
summary (m4)
predict (m4, list (Midterm.std=(100 - mean (Midterm2)) / sd (Midterm2), 
                   Project.std=(30 - mean (Project2)) / sd (Project2)), interval = 'confidence')

# Note: Standardizing does not affect the following:
#   Predicted response and its confidence (or prediction) interval
#   t-values and p-values on the coefficients
#   R^2, R_adj^2, residual standard error, F-test and its p-value.

