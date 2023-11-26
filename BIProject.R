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
# Install and load the 'ipred' package
if (!requireNamespace("ipred", quietly = TRUE)) {
  install.packages("ipred", dependencies = TRUE)
}
library(ipred)#
# Install and load the 'earth' package
if (!requireNamespace("earth", quietly = TRUE)) {
  install.packages("earth", dependencies = TRUE)
}
library(earth)
# Install and load the 'randomForest' package
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest", dependencies = TRUE)
}
library(randomForest)
install.packages("SuperLearner")
library(SuperLearner)
install.packages("caretEnsemble")
library(caretEnsemble)
library(boot)

library(readr)
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

# Load required libraries
library(readr)
library(caret)
library(rpart)
library(kknn)
library(ROCR)
library(pROC)
library(MASS)
library(readr)
Argos_UK <- read_csv("data/Argos_UK.csv")

# Replace missing values with column means
Argos_UK <- as.data.frame(lapply(Argos_UK, function(x) {
  if (is.numeric(x) || is.logical(x)) {
    ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  } else {
    x
  }
}))


# Check for missing values
if (any(is.na(Argos_UK))) {
  # Handle missing values (e.g., imputation)
  Argos_UK <- na.omit(Argos_UK)
}

# 1. Data Splitting ----
set.seed(123)  # Set a seed for reproducibility
train_index <- createDataPartition(Argos_UK$Product.Price, p = 0.75, list = FALSE)
Argos_UK_train <- Argos_UK[train_index, ]
Argos_UK_test <- Argos_UK[-train_index, ]

your_statistic_function <- function(data, indices) {
  sample_data <- data[indices]
  return(mean(sample_data))
}

# 2. Bootstrapping ----
bootstrap_results <- boot(data = Argos_UK$Product.Price, statistic = your_statistic_function, R = 1000)

# 3. Cross Validation ----
train_control <- trainControl(method = "cv", number = 5)
set.seed(456)  # Set a seed for reproducibility
ProductPrice_model <- train(Product.Price ~ ., data = Argos_UK_train, method = "rf", trControl = train_control)

# 4. Model Training ----
set.seed(789)  # Set a seed for reproducibility
ProductPrice2_model <- train(Product.Price ~ ., data = Argos_UK_train, method = "rf")

# 5. Model Performance Comparison ----
set.seed(101)  # Set a seed for reproducibility
ProductPrice_model <- train(Product.Price ~ ., data = Argos_UK_train, method = "rf", trControl = train_control)
ProductPrice2_model <- train(Product.Price ~ ., data = Argos_UK_train, method = "rf", trControl = train_control)

# Compare models
compare_models <- resamples(list(Model1 = ProductPrice_model, Model2 = ProductPrice2_model))
summary(compare_models)

# Manual Search
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "random")

tunegrid <- expand.grid(.mtry = c(1:5))

modellist <- list()
for (ntree in c(500, 800, 1000)) {
  set.seed(123)  # Set a seed for reproducibility
  rf_model <- train(Product.Price ~ ., data = Argos_UK_train, method = "rf", metric = "RMSE",
                    tuneGrid = tunegrid,
                    trControl = train_control,
                    ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- rf_model
}

# Lastly, we compare results to find which parameters gave the lowest RMSE
print(modellist)

results <- resamples(modellist)
summary(results)
dotplot(results)


# Bagging
# Train the bagged model using randomForest
set.seed(123)
bag_model <- randomForest(Product.Price ~ ., data = Argos_UK_train, ntree = 500, importance = TRUE)
# Make sure 'Product.Price' is a numeric column in Argos_UK_test
Argos_UK_test$Product.Price <- as.numeric(Argos_UK_test$Product.Price)

# Make predictions on the test set
bag_predictions <- predict(bag_model, newdata = Argos_UK_test)

# Evaluate performance
bag_rmse <- sqrt(mean((bag_predictions - Argos_UK_test$Product.Price)^2))
cat("Bagging RMSE:", bag_rmse, "\n")

# Create a caret model from the bagged model
bag_caret_model <- list(model = bag_model, method = "rf", trControl = train_control)

# Boosting
set.seed(456)
boost_model <- train(Product.Price ~ ., data = Argos_UK_train, method = "gbm", trControl = train_control)

# Create a caret model from the boosting model
boost_caret_model <- list(model = boost_model, method = "gbm", trControl = train_control)

#Stacking
# Create a list of caret models for stacking
models_for_stacking <- list(bag = bag_caret_model, boost = boost_caret_model)

# Use caretStack to stack the models
stack_model <- caretStack(models_for_stacking)

# Make predictions on the test set using the stacked model
stack_predictions <- predict(stack_model, newdata = Argos_UK_test)

# Evaluate performance of the stacked model
stack_rmse <- sqrt(mean((stack_predictions - Argos_UK_test$Product.Price)^2))
cat("Stacked Model RMSE:", stack_rmse, "\n")