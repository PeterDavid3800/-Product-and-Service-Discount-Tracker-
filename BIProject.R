if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", dependencies = TRUE)
}
library(renv)

if (!requireNamespace("languageserver", quietly = TRUE)) {
  install.packages("languageserver", dependencies = TRUE)
}
library(languageserver)

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr", dependencies = TRUE)
}
library(readr)

if (!requireNamespace("Amelia", quietly = TRUE)) {
  install.packages("Amelia", dependencies = TRUE)
}
library(Amelia)

if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
  install.packages("ggcorrplot", dependencies = TRUE)
}
library(ggcorrplot)
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071", dependencies = TRUE)
}
library(e1071)
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
library(corrplot)
library(readr)
library(boot)

Argos_UK <- read_csv("data/Argos_UK.csv")

# View the data
View(Argos_UK)

# Check dimensions and data types
dim(Argos_UK)
sapply(Argos_UK, class)

# Replace missing values with column means
Argos_UK <- as.data.frame(lapply(Argos_UK, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

# Frequency and percentage for selected columns
columns_to_analyze <- c(2, 4, 7)
for (col in columns_to_analyze) {
  Argos_UK_freq <- Argos_UK[[col]]
  freq_table <- table(Argos_UK_freq)
  result_table <- cbind(frequency = freq_table, percentage = prop.table(freq_table) * 100)
  print(result_table)
}

# Mode calculation for selected columns
calculate_mode <- function(column) {
  mode_names <- names(table(column))[which(table(column) == max(table(column)))]
  return(mode_names)
}

Argos_UK_ProductPrice_mode <- calculate_mode(Argos_UK$Product.Price)
print(Argos_UK_ProductPrice_mode)

Argos_UK_Category <- calculate_mode(Argos_UK$Category)
print(Argos_UK_Category)

Argos_UK_ProductType_mode <- calculate_mode(Argos_UK$Product.Type)
print(Argos_UK_ProductType_mode)

# Summary statistics
summary(Argos_UK)
any(is.na(Argos_UK))
Argos_UK <- as.data.frame(lapply(Argos_UK, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

# Descriptive statistics for "Product Price"
sapply(Argos_UK[c(7)], sd)
sapply(Argos_UK[c(7)], var)
sapply(Argos_UK[c(7)], kurtosis, type = 2)
sapply(Argos_UK[c(7)], skewness, type = 2)

# Covariance and correlation matrices
Argos_UK_cov <- cov(Argos_UK[c(7)])
View(Argos_UK_cov)

Argos_UK_cor <- cor(Argos_UK[c(7)])
View(Argos_UK_cor)

# One-way ANOVA
Argos_UK_one_way_anova <- aov(Product.Price ~ Product.Type, data = Argos_UK)
summary(Argos_UK_one_way_anova)

# Bar plots for selected columns
par(mfrow = c(1, length(7)))
for (col in 7) {
  barplot(table(Argos_UK[[col]]), main = col)
}

# Missing values visualization
missmap(Argos_UK, col = c("red", "grey"), legend = TRUE)

# Plot correlation matrix
corrplot(cor(Argos_UK[c(7)]), method = "circle")

# Confirmation of Missing Values
missing_values <- colSums(is.na(Argos_UK))
print(missing_values)

# Data Imputation
imputed_values <- as.data.frame(lapply(Argos_UK[, numeric_columns], function(x) {
  ifelse(is.na(x), { 
    imputed_value <- mean(x, na.rm = TRUE)
    print(paste("Imputed value", names(x), ":", imputed_value))
    imputed_value
  }, x)
}))

# Data Transformation (Log Transformation for Product Price)
Argos_UK$Product.Price <- log(Argos_UK$Product.Price + 1)

# Data Transformation (Z-Score Normalization for Numeric Columns)
numeric_columns <- sapply(Argos_UK, is.numeric)
Argos_UK[, numeric_columns] <- scale(Argos_UK[, numeric_columns])

# Data Transformation (Box-Cox Transformation for Numeric Columns)
library(MASS)  # Ensure the MASS package is installed
numeric_columns <- sapply(Argos_UK, is.numeric)
Argos_UK[, numeric_columns] <- lapply(Argos_UK[, numeric_columns], function(x) {
  if (all(x > 0)) {
    boxcox(x)$x
  } else {
    x
  }
})

# Data Transformation (One-Hot Encoding for Categorical Columns)
categorical_columns <- sapply(Argos_UK, function(x) is.factor(x) | is.character(x))
Argos_UK <- model.matrix(~., Argos_UK[, c(7, categorical_columns)], drop = TRUE)

# Load libraries
library(boot)
library(caret)

# Load data
Argos_UK <- read_csv("data/Argos_UK.csv")

# Display the first few rows of the data
head(Argos_UK)

# Replace missing values with column means
Argos_UK <- as.data.frame(lapply(Argos_UK, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

# Check for missing values
if (any(is.na(Argos_UK))) {
  # Handle missing values (e.g., imputation)
  Argos_UK <- na.omit(Argos_UK)
}

# Check for multicollinearity
cor_matrix <- cor(Argos_UK[, sapply(Argos_UK, is.numeric)])
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8)
if (length(highly_correlated) > 0) {
  # Address multicollinearity (e.g., remove highly correlated variables)
  Argos_UK <- Argos_UK[, -highly_correlated]
}

# Data Splitting
set.seed(123)  # Set seed for reproducibility
train_indices <- createDataPartition(Argos_UK$Product.Price, p = 0.7, list = FALSE)
train_data <- Argos_UK[train_indices, ]
test_data <- Argos_UK[-train_indices, ]

# Retrain the model
boot_model <- lm(Product.Price ~ ., data = train_data)

# Bootstrapping with check for model convergence
boot_results <- boot(data = train_data, statistic = function(data, indices) {
  fit <- try(lm(Product.Price ~ ., data = data[indices, ]), silent = TRUE)
  
  if (inherits(fit, "try-error") || length(coef(fit)) != length(coef(boot_model))) {
    return(rep(NA, length(coef(boot_model))))
  }
  
  return(coef(fit))
}, R = 1000)

# Cross-validation (example using linear regression)/error starts here
cv_model <- try(
  train(Product.Price ~ ., data = train_data, method = "lm", trControl = trainControl(method = "cv")),
  silent = TRUE
)


# Model training (linear regression)
final_model <- lm(Product.Price ~ ., data = train_data)

# Model performance comparison using resamples
resamples_list <- resamples(list(Linear_Regression = final_model, CV_Model = cv_model))
summary(resamples_list)
