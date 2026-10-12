# Basic arithmetic
2 + 3
10 - 5
4 * 6
15 / 3
2^4 # Exponentiation
sqrt (16) # Square root
log (100) # Natural logarithm
exp (1) # Exponential function (e ^1)

# Assigning variables
x <- 5
y <- 10
x + y
z <- x * y
z

# Removing variables
rm( z )

################################################
# Creating vectors
vec1 <- c(1, 2, 3, 4, 5)
vec2 <- 1:10 # Sequence from 1 to 10
vec3 <- seq (0 , 1,by = 0.1) # Sequence with step
vec4 <- rep (5, times = 10) # Repeat value

# Vector arithmetic
vec1 + 10
vec1 * 2
vec1^2

# Indexing
vec2[3] # Third element
vec2[3:6] # Elements 3 through 6
vec2[c(2 , 5 , 8) ] # Specific elements

# Vectorized operations
sum(vec2)
mean(vec2)
sd(vec2)
max(vec2)
min(vec2)
length(vec2)


#########################################
# Creating a data frame
df <- data.frame (
  name = c("Alice", "Bob", "Charlie", "Diana"),
  age = c(25,30,35,28),
  score = c(90,85,95,88)
  )

# Inspecting
df
str(df) # Structure
summary(df) # Summary statistics
head(df) # First few rows
tail(df) # Last few rows

# Accessing columns
df$name
df$age
df[,"score"] # Column by name
df[2, ] # Row 2

# Adding a new column
df$grade <- c("A", "B", "A", "B")
df

# Subsetting
df[df$age > 28 , ] # Rows where age > 28
df[order(df$age) , ] # Sort by age

######################
# Using built -in datasets
data(mtcars) # Load the mtcars dataset
data(iris) # Load the iris dataset

# Inspect the data
head(mtcars)
summary(mtcars)
str(mtcars)

# Importing CSV files
# ( Replace with your actual file path )
# my_data <- read.csv ("path/to/your/file.csv")

# Importing Excel files ( requires readxl package )
# install.packages ("readxl")
# library (readxl)
# my_data <- read_excel ("path/to/your/file.xlsx")

# Exporting data
write.csv(mtcars,"mtcars _ exported.csv", row.names = FALSE)


#################
# Load iris data
data(iris)

# Explore
head(iris)
str(iris)
summary(iris)

# Compute statistics by species
mean(iris$Sepal.Length)
sd(iris$Sepal.Length)

# Subset by species
iris_setosa <- iris [iris$Species == " setosa ", ]
iris_versicolor <- iris [iris$Species == " versicolor ", ]
iris_virginica <- iris [iris$Species == " virginica ", ]

# Compare means
mean(iris_setosa$Sepal.Length)
mean(iris_versicolor$Sepal.Length)
mean(iris_virginica$Sepal.Length)

# Basic plot
plot (iris$Sepal.Length , iris$Petal.Length ,
          col = as.numeric (iris$Species) ,
          main = " Sepal Length vs Petal Length ",
          xlab = " Sepal Length ", ylab = " Petal Length ")

legend (" topleft ", legend = levels (iris$Species) ,
           29 col = 1:3 , pch = 1)

######################
#Discussion Questions:
#1. What is the difference between a vector and a data frame?
A vector is a one-dimensional collection of values, usually all the same type.

A data frame is a two-dimensional table with rows and columns, and different columns can have different types.

#2. Why is it important to use set.seed() before generating random numbers? 
It makes random-number generation reproducible, so you get the same random results each time you run the code.

#3. What happens if you import a CSV file with missing values? (do test it)
R usually imports missing entries as NA

#Challenge Problems
# 1. Create a data frame with 10 rows and 5 columns of your own data.
df <- data.frame(
  name = c("Ana", "Bruno", "Carlos", "Diana", "Eduardo", "Fernanda", "Gabriel", "Helena", "Isabela", "João"),
  age = c(10, 11, 12, 13, 10, 11, 12, 13, 10, 11),
  class = c("5º ano", "6º ano", "7º ano", "8º ano", "5º ano", "6º ano", "7º ano", "8º ano", "5º ano", "6º ano"),
  grade = c(8.5, 9.0, 5.8, 9.2, 7.8, 5.5, 10.0, 6.1, 8.9, 4.5),
  approval = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
)
df

# 2. Import a dataset from https://archive.ics.uci.edu/datasets and explore it.
download.file(
  "https://archive.ics.uci.edu/static/public/222/bank+marketing.zip", "bank_mkt.zip")
# Extract the 2 zip folders
unzip("bank_mkt.zip", exdir = "temp")

# Extract only "bank.zip" and read the csv file
unzip("temp/bank.zip", exdir = "bank_data")
bank <- read.csv("bank_data/bank-full.csv", sep = ";")

# Explore
head(bank)
str(bank)
summary(bank)


# 3. Write a function that computes the mean, median, and standard deviation of a numeric vector
stats <- function(x) {
  c(
    mean = mean(x),
    median = median(x),
    standard_deviation = sd(x)
  )
}

stats(c(2, 4, 6, 8, 10))