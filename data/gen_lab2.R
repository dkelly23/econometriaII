set.seed(123)  # For reproducibility

# Set the number of observations
n <- 100000

# AR(1) ------------------------------------------------------------------------

# Generate independent variables
time <- 1:n
x1 <- rnorm(n, mean = 50, sd = 10)  # Random normal variable
x2 <- rnorm(n, mean = 30, sd = 5)   # Another random normal variable
x3 <- rnorm(n, mean = 20, sd = 7)   # New independent variable

# Introduce autocorrelation in errors using an AR(1) process
rho1 <- 0.7  # Serial correlation for y1
rho2 <- 0.5  # Serial correlation for y2

errors1 <- numeric(n)
errors1[1] <- rnorm(1)


for (t in 2:n) {
  errors1[t] <- rho1 * errors1[t - 1] + rnorm(1)  # AR(1) for y1
}

# Define the dependent variables
y <- 10 + 2*x1 - 3*x2 + 1.5*x3 + errors1  # Linear model with serially correlated errors

# Create a dataframe
df1 <- data.frame(y, x1, x2, x3)

# View first few rows
head(df1)

# AR(3) ------------------------------------------------------------------------
# Generate independent variables
x1 <- rnorm(n, mean = 50, sd = 10)  # First independent variable
x2 <- rnorm(n, mean = 30, sd = 5)   # Second independent variable
x3 <- rnorm(n, mean = 20, sd = 7)   # Third independent variable

# AR(3) coefficients
phi1 <- 0.6   # First lag coefficient
phi2 <- -0.3  # Second lag coefficient
phi3 <- 0.2   # Third lag coefficient

# Generate AR(3) errors
errors <- numeric(n)
errors[1:3] <- rnorm(3)  # Initialize first 3 values
for (t in 4:n) {
  errors[t] <- phi1 * errors[t-1] + phi2 * errors[t-2] + phi3 * errors[t-3] + rnorm(1)
}

# Define the dependent variable with AR(3) errors
y <- 10 + 2*x1 - 3*x2 + 1.5*x3 + errors  

# Create a dataframe
df2 <- data.frame(y, x1, x2, x3)

# View first few rows
head(df2)



# Guardar ----------------------------------------------------------------------
save(df1, df2, file="/Users/danielkelly/Library/Mobile Documents/com~apple~CloudDocs/El Colegio de México/Lab Ectria 2/ps2.RData")
