install.packages("ggplot2")
install.packages("dplyr")
#loading the libraries
library(ggplot2); library(dplyr)

#Exercise 1: Generating Random Samples
# Set seed for reproducibility
set.seed (123)

# Generate samples
n <- 10000

normal_sample <- rnorm (n, mean = 0, sd = 1)
poisson_sample <- rpois (n, lambda = 3)
binom_sample <- rbinom (n, size = 10, prob = 0.5)
exp_sample <- rexp (n, rate = 2)
unif_sample <- runif (n, min = 0, max = 1)

# Check samples
head(normal_sample)
summary(normal_sample)
summary(poisson_sample)

#Exercise 2: Compute density and cumulative probability functions
# PDF values (density)
x <- seq(-4, 4, length.out = 100)
dnorm(x, mean = 0, sd = 1)

# CDF values (cumulative probability)
pnorm(1.96) # P(Z <= 1.96)
pnorm(0) # P(Z <= 0)
qnorm(0.975) # 97.5th percentile

# Binomial
dbinom(5, size = 10, prob = 0.5) # P(X = 5)
pbinom(7, size = 10, prob = 0.5) # P(X <= 7)

# Poisson
dpois(3, lambda = 3) # P(X = 3)
ppois(5, lambda = 3) # P(X <= 5)

#Exercise 3: Histograms & Density Plots
#Goal: Create histograms with overlaid theoretical densities
# Basic histogram
hist(normal_sample, main = "Normal (0 ,1) Histogram",
     xlab = "x", col = "lightblue", probability = TRUE)

# Add theoretical density
curve(dnorm(x, mean = 0, sd = 1), add = TRUE,
     col = "red", lwd = 2)

# Histogram with custom breaks
hist(normal_sample, breaks = 30, probability = TRUE,
     main = "Normal (0 ,1) with 30 bins")

# Multiple plots in one window
par(mfrow = c(2, 2))

hist(normal_sample, probability = TRUE, main = "Normal")
curve(dnorm(x), add = TRUE, col = "red", lwd = 2)

hist(poisson_sample, probability = TRUE, main = "Poisson (3)")
curve(dpois(x, lambda = 3), add = TRUE, col = "red", lwd = 2, type =
             "h")

hist(exp_sample, probability = TRUE, main = "Exp (2)")
curve(dexp(x, rate = 2), add = TRUE, col = "red", lwd = 2)

hist(unif_sample, probability = TRUE, main = "Uniform (0 ,1)")
abline(h = 1, col = "red", lwd = 2)

# Reset plot layout
par( mfrow = c(1, 1))

#Exercise 4: Boxplots to compare distributions
# Boxplot for one variable
boxplot(normal_sample, main = "Boxplot of Normal Sample",
         ylab = "x", col = "lightblue")

# Boxplots for multiple samples
boxplot(normal_sample, poisson_sample, exp_sample,
       names = c("Normal", "Poisson", "Exponential"),
       main = "Comparison of Distributions",
       col = c("lightblue", "lightgreen", "pink"))

# Boxplot by group using iris
boxplot(Sepal.Length ~ Species , data = iris,
        main = "Sepal Length by Species",
        xlab = "Species", ylab = "Sepal Length",
        col = c("lightblue", "lightgreen", "pink"))

# Exercise 5: Introduction to ggplot2
# Histogram with ggplot2
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Sepal Length",
       x = "Sepal Length", y = "Count") +
  theme_minimal()

# Density plot
ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Sepal Length Density by Species",
       x = "Sepal Length", y = "Density") +
  theme_minimal()

# Scatter plot
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length , color = Species))+
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Sepal Length vs Petal Length",
       x = "Sepal Length", y = "Petal Length") +
  theme_minimal()

# Boxplot with ggplot2
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Sepal Length by Species",
         x = "Species", y = "Sepal Length") +
  theme_minimal() +
  theme(legend.position = "none")

# Faceting
ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(bins = 20, alpha = 0.7) +
  facet_wrap(~ Species) +
  labs(title = "Sepal Length by Species") +
  theme_minimal()

# Saving plots
ggsave("sepal_length_ histogram.png", width = 8, height = 6)

# Exercise 6: Comparing Empirical and Theoretical Distributions
#Goal: Compare generated samples to theoretical distributions
# Q-Q plot ( Quantile - Quantile )
qqnorm(normal_sample)
qqline(normal_sample, col = "red", lwd = 2)

# Q-Q plot for Poisson
qqplot(rpois (1000, lambda = 3), poisson_sample)
abline(0, 1, col = "red")

# Kolmogorov - Smirnov test
ks.test(normal_sample, "pnorm", mean = 0, sd = 1)
ks.test(poisson_sample, "ppois", lambda = 3)

# Compare empirical CDF to theoretical
plot(ecdf(normal_sample), main = "Empirical vs Theoretical CDF")
curve(pnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2)
legend("topleft", legend = c("Empirical", "Theoretical"),
       col = c("black", "red"), lwd = c(1, 2))

 
#Exercise 7: challenge problems
#7.1. Create a 2x2 grid of histograms for different Normal distributions (varying mean and sd).
set.seed(123)

normal1 <- rnorm(1000, mean = 0, sd = 1)
normal2 <- rnorm(1000, mean = 5, sd = 1)
normal3 <- rnorm(1000, mean = 0, sd = 2)
normal4 <- rnorm(1000, mean = 5, sd = 2)

# Create a 2x2 grid
par(mfrow = c(2, 2))

# Histogram 1
hist(normal1,
     main = "Normal: mean = 0, sd = 1",
     xlab = "Value",
     col = "lightblue")

# Histogram 2
hist(normal2,
     main = "Normal: mean = 5, sd = 1",
     xlab = "Value",
     col = "lightgreen")

# Histogram 3
hist(normal3,
     main = "Normal: mean = 0, sd = 2",
     xlab = "Value",
     col = "lightpink")

# Histogram 4
hist(normal4,
     main = "Normal: mean = 5, sd = 2",
     xlab = "Value",
     col = "lightyellow")

#7.2. Generate 10,000 samples from a Beta(2,5) distribution and compare to the theoretical density.
set.seed(123)

beta_sample <- rbeta(10000, shape1 = 2, shape2 = 5)

# Plot the histogram as a density
hist(beta_sample,
     probability = TRUE,
     breaks = 30,
     main = "Beta(2,5): Sample vs Theoretical Density",
     xlab = "Value",
     col = "lightblue",
     border = "white")

# Add the theoretical Beta(2,5) density
curve(dbeta(x, shape1 = 2, shape2 = 5),
      from = 0,
      to = 1,
      add = TRUE,
      col = "red",
      lwd = 2)

# Add a legend
legend("topright",
       legend = c("Sample", "Theoretical density"),
       fill = c("lightblue", NA),
       border = c("white", NA),
       col = c(NA, "red"),
       lwd = c(NA, 2))


#7.3. Create a publication-ready plot of iris data and export it as a PDF.
p <- ggplot(iris, aes(x = Sepal.Length,
                      y = Sepal.Width,
                      color = Species)) +
  geom_point(size = 2.5, alpha = 0.8) +
  labs(
    title = "Sepal Dimensions of Iris Species",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    color = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Display the plot
print(p)

# Export as PDF
ggsave(
  filename = "iris_publication_plot.pdf",
  plot = p,
  width = 7,
  height = 5,
  units = "in"
)
