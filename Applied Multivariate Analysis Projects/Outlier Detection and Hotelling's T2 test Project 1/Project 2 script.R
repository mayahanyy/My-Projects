# Load the required libraries
library(MASS)
library(ggplot2)

# Read the dataset
data <- read.csv("winequality-red.csv")

# a) Plot the QQ plot
qqnorm(data$quality)
qqline(data$quality)

# b) Check if the data is normal and normalize if not
if (!shapiro.test(data$quality)$p.value > 0.05) {
  data$quality <- scale(data$quality)
  # Normalized "quality" variable
}

# c) Split the data into two groups based on quality variable
group1 <- data[data$quality > 6.5, ]
group2 <- data[data$quality <= 6.5, ]

# d) Apply Fisher Linear Discriminant Analysis (FLDA)
flda_model <- lda(factor(quality) ~ ., data)
print(flda_model)

# e) Apply Fisher Linear Discriminant Analysis 2 (FLDA2)
flda2_model <- lda(factor(quality) ~ ., data, prior = c(0.5, 0.5))
print(flda2_model)

# f) Internal validation
flda_internal_val <- CV.lda(flda_model)
print(flda_internal_val)

# g) External validation (assumes you have a separate test dataset)
test_data <- read.csv("test_dataset.csv")
flda_external_val <- predict(flda_model, test_data)
print(flda_external_val)