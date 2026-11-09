install.packages("microbenchmark")
install.packages("dplyr")
#loading the libraries
library(microbenchmark); library(dplyr)

#Exercise 1: Writing Functions
#writing reusable functions to simulation

# Function to compute mean and standard error
sim_stats <- function(n, mean = 0, sd = 1,
                      dist = "normal", ...) {
  
  if (dist == "normal") {
    x <- rnorm(n, mean = mean, sd = sd)
    
  } else if (dist == "poisson") {
    x <- rpois(n, ...)
    
  } else if (dist == "exponential") {
    x <- rexp(n, ...)
    
  } else {
    stop("Distribution not supported")
  }
  
  return(list(
    mean = mean(x),
    se = sd(x) / sqrt(n),
    sd = sd(x),
    n = n
  ))
}

# Test the function
sim_stats(100, mean = 5, sd = 2)
sim_stats(100, dist = "poisson", lambda = 3)
sim_stats(100, dist = "exponential", rate = 2)


# Function with default arguments
run_simulation <- function(n = 100, n_sims = 1000, dist = "normal", ...) {
  results <- replicate(n_sims, {
    
  x <- switch(dist,
                normal = rnorm(n, ...),
                poisson = rpois(n, ...),
                exponential = rexp(n, ...))
    c(mean = mean(x), sd = sd(x))
    })

  return(list(
    means = results[1, ],
    sds = results[2, ],
    mean_of_means = mean(results[1, ]),
    sd_of_means = sd(results[1, ])
     ))
   }

# Test
run_simulation(n = 30, n_sims = 500, dist = "normal", mean = 0, sd = 1)

#Exercise: Build a function to perform the estimation of the mean, se, sd for multiple values
# of n. Investigate the behavior of these summary quantities in a histogram.
run_simulation(n = 30, n_sims = 500, dist = "poisson", lambda = 3)

run_simulation(n = 30, n_sims = 500, dist = "exponential", rate = 2)


# Exercise: Investigate and include the production of samples of one additional distribution
# Function for multiple sample sizes

compare_sample_sizes <- function(n_values,
                                 n_sims = 1000,
                                 dist = "normal",
                                 ...) {
  
  results <- lapply(n_values, function(n) {
    
    sim <- replicate(n_sims, {
      
      x <- switch(dist,
                  normal = rnorm(n, ...),
                  poisson = rpois(n, ...),
                  exponential = rexp(n, ...))
      
      c(mean = mean(x),
        se = sd(x) / sqrt(n),
        sd = sd(x))
    })
    
    data.frame(
      n = n,
      mean = sim[1, ],
      se = sim[2, ],
      sd = sim[3, ]
    )
  })
  
  do.call(rbind, results)
}

#running the function
n_values <- c(10, 30, 50, 100, 500)

results <- compare_sample_sizes(
  n_values = n_values,
  n_sims = 1000,
  dist = "normal",
  mean = 0,
  sd = 1
)

#ploting histograms
par(mfrow = c(3, 1))

hist(results$mean,
     main = "Distribution of Sample Means",
     xlab = "Mean",
     col = "lightblue")

hist(results$se,
     main = "Distribution of Standard Errors",
     xlab = "SE",
     col = "lightgreen")

hist(results$sd,
     main = "Distribution of Standard Deviations",
     xlab = "SD",
     col = "lightpink")

par(mfrow = c(1, 1))

#Exercise 2: Control Structures
#if / else
x <- 5

if (x > 0) {
  print("Positive")
} else if (x == 0) {
  print("Zero")
} else {
  print("Negative")
}

sum <- 0

#for loop
for (i in 1:10) {sum <- sum + i}
print(sum)

# While loop
i <- 1

while (i <= 10) {
  print(i)
  i <- i + 1}

# Nested loop for matrix
mat <- matrix(0, nrow = 5, ncol = 5)

for (i in 1:5) {
  for (j in 1:5) {
    mat[i, j] <- i * j
  }
}

mat

#Exercise 3: Vectorization vs. Loops
library(microbenchmark)

# Vectorized vs loop
n <- 1000000
x <- runif(n)

# Loop-based
loop_square <- function(x) {
  result <- numeric(length(x))
  
  for (i in 1:length(x)) {
    result[i] <- x[i]^2
  }
  
  return(result)
}

# Vectorized
vectorized_square <- function(x) {
  x^2
}

# Compare performance
microbenchmark(
  loop = loop_square(x),
  vectorized = vectorized_square(x),
  times = 10
)

# Apply family
mat <- matrix(rnorm(10000), nrow = 100)

# Row means with apply
row_means_apply <- apply(mat, 1, mean)

# Row means with loop
row_means_loop <- numeric(nrow(mat))

for (i in 1:nrow(mat)) {
  row_means_loop[i] <- mean(mat[i, ])
}

# Compare results
all.equal(row_means_apply, row_means_loop)

#Exercise: Perform the comparison between the loop and apply for the matrix operations
set.seed(123)
mat <- matrix(rnorm(10000), nrow = 100)

# Method 1: Using apply()
row_means_apply <- function(mat) {
  apply(mat, 1, mean)
}

# Method 2: Using for loop
row_means_loop <- function(mat) {
  result <- numeric(nrow(mat))
  
  for (i in 1:nrow(mat)) {
    result[i] <- mean(mat[i, ])
  }
  
  return(result)
}

# Compare the results
all.equal(
  row_means_apply(mat),
  row_means_loop(mat)
)

# Compare execution time
benchmark_results <- microbenchmark(
  apply = row_means_apply(mat),
  loop = row_means_loop(mat),
  times = 100
)

# Display results
print(benchmark_results)

# Plot performance comparison
boxplot(
  benchmark_results,
  main = "Performance: Loop vs Apply",
  ylab = "Execution time (microseconds)",
  col = c("lightblue", "lightgreen")
)

# NOTE
# Both methods produce equivalent results, as confirmed by the all.equal() function.
# 
# The execution times are compared using the microbenchmark() package. 
# The benchmark allows us to evaluate the computational efficiency of each approach.
# 
# The apply() function provides more concise and readable code, while the for loop offers explicit control over the iterations.
# 
# The results show that both approaches can perform the same matrix operation, 
# although their execution times may differ depending on the matrix dimensions and computational environment.
# 
# Conclusion: The apply() function is a convenient alternative to explicit loops when performing operations 
# across matrix rows or columns. However, benchmarking is important to determine which approach is more efficient for a particular problem.

#Exercise 4: Data Manipulation with dplyr
data (iris)

# Filter observations from setosa species
iris_setosa <- filter(iris, Species == "setosa")

# Filter flowers with larger sepals and petals
iris_large <- filter( iris, Sepal.Length > 5 & Petal.Length > 3)

#select
iris_small <- select(
  iris,
  Sepal.Length,
  Petal.Length,
  Species)

#mutate
iris_new <- mutate(
  iris,
  Sepal_Petal_Ratio = Sepal.Length / Petal.Length,
  Sepal_Area = Sepal.Length * Sepal.Width)

#group and summarise
iris_summary <- iris %>%
  group_by(Species) %>%
  summarise(
    mean_sepal = mean(Sepal.Length),
    sd_sepal = sd(Sepal.Length),
    mean_petal = mean(Petal.Length),
    n = n()
  )

print(iris_summary)

#arrange
iris_arranged <- iris %>%
  arrange(desc(Sepal.Length))

# Pipeline example
iris_pipeline <- iris %>%
  filter(Sepal.Length > 5) %>%
  group_by(Species) %>%
  summarise(mean_petal = mean(Petal.Length)) %>%
  arrange(desc(mean_petal))

 print(iris_pipeline)

 
 #Exercise: Describe the commands used above.
 # The dplyr package provides useful functions for data manipulation in R.
 # The filter() function is used to select specific observations based on
 # given conditions. In this exercise, it selects flowers belonging to the
 # setosa species and flowers with sepal lengths greater than 5 and petal
 # lengths greater than 3.
 #
 # The select() function is used to choose specific columns from the dataset.
 # Here, it creates a new dataset containing only Sepal.Length, Petal.Length,
 # and Species.
 #
 # The mutate() function allows us to create new variables or modify existing
 # ones. In this example, it calculates the ratio between sepal and petal
 # length and the sepal area by multiplying sepal length by sepal width.
 #
 # The group_by() function groups the observations according to species,
 # while summarise() calculates summary statistics for each group. In this
 # exercise, it calculates the mean and standard deviation of sepal length,
 # the mean petal length, and the number of observations for each species.
 #
 # The arrange() function is used to organize observations according to a
 # specific variable. The desc() function sorts the data in descending order,
 # from the largest to the smallest sepal length.
 #
 # Finally, the pipe operator (%>%) connects multiple functions, allowing
 # different data manipulation operations to be performed in sequence.
 # In the pipeline example, the data are first filtered to include flowers
 # with sepal lengths greater than 5, then grouped by species, summarized
 # by mean petal length, and finally arranged in descending order.
 #
 # Overall, these functions make data manipulation easier and more efficient,
 # allowing us to filter, select, create, summarize, and organize data.
 
#Exercise: Change Sepal.Length parameter value in the scripts to verify the changes
iris_large <- filter(iris, Sepal.Length > 6 & Petal.Length > 3)

print(iris_large)

#Exercise: Perform a descriptive analysis of iris database
str(iris)

head(iris)

summary(iris)

# Calculate the mean of numerical variables
mean(iris$Sepal.Length)
mean(iris$Sepal.Width)
mean(iris$Petal.Length)
mean(iris$Petal.Width)

# Calculate the standard deviation
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

# Calculate the variance
var(iris$Sepal.Length)
var(iris$Sepal.Width)
var(iris$Petal.Length)
var(iris$Petal.Width)

# Frequency distribution of species
table(iris$Species)

# Descriptive statistics grouped by species
iris %>%
  group_by(Species) %>%
  summarise(
    mean_sepal_length = mean(Sepal.Length),
    mean_sepal_width = mean(Sepal.Width),
    mean_petal_length = mean(Petal.Length),
    mean_petal_width = mean(Petal.Width),
    sd_sepal_length = sd(Sepal.Length),
    sd_petal_length = sd(Petal.Length),
    n = n()
  )

#Exercise 5: Complete Simulation Study
#library(dplyr)


study_coverage <- function(n, n_sims = 1000, mu = 0, sigma = 1) {
  
  results <- replicate(n_sims, {
    x <- rnorm(n, mean = mu, sd = sigma)
    ci <- mean(x) + c(-1.96, 1.96) * sd(x) / sqrt(n)
    covered <- (ci[1] <= mu) & (ci[2] >= mu)
    return(covered)
  })
  return(mean(results))
}

# Test different sample sizes
sample_sizes <- c(10, 30, 50, 100, 500)

coverage_results <- data.frame(
  n = sample_sizes,
  coverage = sapply(sample_sizes, study_coverage)
)
print(coverage_results)

# Plot coverage results
plot(
  coverage_results$n,
  coverage_results$coverage,
  type = "b",
  xlab = "Sample Size",
  ylab = "Coverage Probability",
  main = "Coverage of 95% Confidence Intervals",
  ylim = c(0.9, 1)
)
abline(h = 0.95, col = "red", lty = 2)


# Study the effect of sample size on statistical power
study_power <- function(n, delta, n_sims = 1000) {
   results <- replicate(n_sims, {
    x <- rnorm(n, mean = delta, sd = 1)
    t_test <- t.test(x, mu = 0)
    return(t_test$p.value < 0.05)
  })
  return(mean(results))
}

# Test different effect sizes
effect_sizes <- seq(0, 1, by = 0.1)

power_results <- data.frame(
  delta = effect_sizes,
  power = sapply(effect_sizes, function(d) study_power(30, d))
)

print(power_results)

# Plot power results
plot(
  power_results$delta,
  power_results$power,
  type = "b",
  xlab = "Effect Size",
  ylab = "Statistical Power",
  main = "Power vs Effect Size (n = 30)")
abline(h = 0.8, col = "red", lty = 2)

#Exercise: Investigate how the T-test is performed
set.seed(123)

sample_data <- rnorm(30, mean = 5, sd = 1)

# Perform a one-sample T-test
t_test_result <- t.test(sample_data, mu = 5)

print(t_test_result)

# Extract the p-value
t_test_result$p.value

# Extract the confidence interval
t_test_result$conf.int

# Extract the sample mean
mean(sample_data)

# Extract the sample standard deviation
sd(sample_data)


# Two-sample T-test

# Generate two independent samples
group1 <- rnorm(30, mean = 5, sd = 1)
group2 <- rnorm(30, mean = 6, sd = 1)

# Compare the means of the two groups
two_sample_test <- t.test(group1, group2)

print(two_sample_test)

#Exercise: Investigate how larger values of n change the power of the test
set.seed(123)

# Function to calculate statistical power
study_power <- function(n, delta, n_sims = 1000) {
  results <- replicate(n_sims, {
    x <- rnorm(n, mean = delta, sd = 1)
    t_test <- t.test(x, mu = 0)
    return(t_test$p.value < 0.05)
  })
  return(mean(results))
}

# Test different sample sizes
sample_sizes <- c(10, 20, 30, 50, 100, 200, 500)

# Calculate power for each sample size
power_results <- data.frame(
  n = sample_sizes,
  power = sapply(
    sample_sizes,
    function(n) study_power(n, delta = 0.5)
  )
)

# Display results
print(power_results)

# Plot statistical power
par(mar = c(4, 4, 2, 1)) #this line is used to reduce the graph

plot(
  power_results$n,
  power_results$power,
  type = "b",
  xlab = "Sample Size (n)",
  ylab = "Statistical Power",
  main = "Effect of Sample Size on Statistical Power",
  ylim = c(0, 1)
)
# Add reference line at 80% power
abline(h = 0.8, col = "red", lty = 2)

 #CHALLENGES
# Write a function that simulates the coverage probability of a confidence interval
#for the mean of a Poisson distribution
study_poisson_coverage <- function(n, lambda = 5, n_sims = 1000) {
  
  results <- replicate(n_sims, {
    
    # Generate a random sample from a Poisson distribution
    x <- rpois(n, lambda = lambda)
    
    # Calculate the sample mean and standard error
    sample_mean <- mean(x)
    standard_error <- sd(x) / sqrt(n)
    
    # Calculate the 95% confidence interval
    ci <- sample_mean + c(-1.96, 1.96) * standard_error
    
    # Check if the true mean is inside the confidence interval
    covered <- (ci[1] <= lambda) & (ci[2] >= lambda)
    return(covered)
  })
  
  # Calculate the coverage probability
  return(mean(results))
}

# Test different sample sizes
sample_sizes <- c(10, 30, 50, 100, 500)

coverage_results <- data.frame(
  n = sample_sizes,
  coverage = sapply(sample_sizes, study_poisson_coverage)
)

# Display results
print(coverage_results)

# Plot coverage probability
par(mar = c(4, 4, 2, 1)) #this line is used to reduce the graph

plot(
  coverage_results$n,
  coverage_results$coverage,
  type = "b",
  xlab = "Sample Size",
  ylab = "Coverage Probability",
  main = "Poisson Confidence Interval Coverage",
  ylim = c(0, 1)
)

abline(h = 0.95, col = "red", lty = 2)


#challenge
#Design a simulation study to compare two different estimators for the same parameter
set.seed(123)

# Define simulation parameters
n <- 30
n_sims <- 1000
mu <- 5

# Create simulation function
compare_estimators <- function(n, n_sims, mu) {
  
  results <- replicate(n_sims, {
    
    # Generate random samples
    x <- rnorm(n, mean = mu, sd = 1)
    
    # Calculate two estimators
    mean_estimator <- mean(x)
    median_estimator <- median(x)
    
    return(c(mean_estimator, median_estimator))
  })
  return(results)
}

# Run simulation
results <- compare_estimators(n, n_sims, mu)

# Calculate bias
bias_mean <- mean(results[1, ]) - mu
bias_median <- mean(results[2, ]) - mu

# Calculate variance
var_mean <- var(results[1, ])
var_median <- var(results[2, ])

# Calculate Mean Squared Error (MSE)
mse_mean <- mean((results[1, ] - mu)^2)
mse_median <- mean((results[2, ] - mu)^2)

# Create comparison table
comparison <- data.frame(
  Estimator = c("Sample Mean", "Sample Median"),
  Bias = c(bias_mean, bias_median),
  Variance = c(var_mean, var_median),
  MSE = c(mse_mean, mse_median)
)

print(comparison)

# Compare estimator distributions
par(mar = c(4, 4, 3, 2)) # Adjust plot margins

boxplot(
  results[1, ],
  results[2, ],
  names = c("Mean", "Median"),
  main = "Comparison of Two Estimators",
  ylab = "Estimated Values",
  col = c("lightblue", "lightgreen"),
  cex.main = 1.2
)
abline(h = mu, col = "red", lty = 2, lwd = 2)