# Simulated Dataset Generation
# Model: y = b0 + b1*x1 + b2*x2 + b3*x3 + b4*x2^2 + error

# Set seed for reproducibility
set.seed(123)

# Sample size
n <- 200

# Generate random coefficients
b0 <- runif(1, min = -5, max = 5)  # intercept
b1 <- runif(1, min = -3, max = 3)  # coefficient for x1
b2 <- runif(1, min = -3, max = 3)  # coefficient for x2
b3 <- runif(1, min = -3, max = 3)  # coefficient for x3
b4 <- runif(1, min = -.8, max = -.4)  # coefficient for x2^2

# Display the true coefficients
cat("True Model Coefficients:\n")
cat("b0 (intercept):", round(b0, 3), "\n")
cat("b1 (x1):", round(b1, 3), "\n")
cat("b2 (x2):", round(b2, 3), "\n")
cat("b3 (x3):", round(b3, 3), "\n")
cat("b4 (x2^2):", round(b4, 3), "\n\n")

# Generate independent variables
x1 <- rnorm(n, mean = 0, sd = 1)      # x1 ~ N(0, 1)
x2 <- rnorm(n, mean = 0, sd = 3)      # x2 ~ N(0, 3)
x3 <- rbinom(n, size = 1, prob = 0.3) # x3 is binary with p = 0.3

# Generate noise variables (not in the true model)
z1 <- rnorm(n, mean = 0, sd = 1)      # noise variable 1
z2 <- rnorm(n, mean = 5, sd = 2)      # noise variable 2
z3 <- runif(n, min = -10, max = 10)   # noise variable 3

# Generate error term (assuming standard normal errors)
error <- rnorm(n, mean = 0, sd = 2)

# Generate dependent variable y based on the model
y <- b0 + b1*x1 + b2*x2 + b3*x3 + b4*x2^2 + error

# Create data frame
data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, z1 = z1, z2 = z2, z3 = z3)

# Display first few rows
cat("First few rows of the generated dataset:\n")
print(head(data))

# Summary statistics
cat("\nSummary statistics:\n")
print(summary(data))

# Optional: Fit a model to verify (note: need to include x2^2 term)
cat("\n--- Model Verification ---\n")
cat("Fitting the true model with x2^2 term:\n")
model_true <- lm(y ~ x1 + x2 + x3 + I(x2^2), data = data)
print(summary(model_true))

cat("\n--- Model with Noise Variables ---\n")
cat("Fitting a model that incorrectly includes noise variables:\n")
model_with_noise <- lm(y ~ x1 + x2 + x3 + I(x2^2) + z1 + z2 + z3, data = data)
print(summary(model_with_noise))

cat("\nNote: z1, z2, and z3 are noise variables and should not be significant.\n")

model_test <- lm(y ~ x1 , data = data)
print(summary(model_test))

model_test <- lm(y ~ x1 + z1 + z2 + z3, data = data)
print(summary(model_test))

# Optional: Save the dataset
write.csv(data, "data/simulated_data.csv", row.names = FALSE)
