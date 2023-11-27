Business Project
================
<135230 Peter David Aringo 4.2B>
<"27/11/2023">


# Student Details

<table>
<colgroup>
<col style="width: 23%" />
<col style="width: 76%" />
</colgroup>
<tbody>
<tr class="odd">
<td><strong>Student ID Number and Name</strong></td>
<td><p><em></em></p>
<li><p>135230 - B - Peter Aringo</p></li>

</ol></td>
</tr>
<tr class="odd">
<td><strong>Course Code</strong></td>
<td>BBT4206</td>
</tr>
<tr class="even">
<td><strong>Course Name</strong></td>
<td>Business Intelligence II</td>
</tr>
<tr class="odd">
<td><strong>Program</strong></td>
<td>Bachelor of Business Information Technology</td>
</tr>
<tr class="even">
<td><strong>Semester Duration</strong></td>
<td>21<sup>st</sup> August 2023 to 28<sup>th</sup> November 2023</td>
</tr>
</tbody>
</table>

# Setup Chunk

We start by installing all the required packages

```{r Install Packages, echo=TRUE, message=FALSE, warning=FALSE}
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
```


```{r setup, echo=TRUE, message=FALSE, warning=FALSE}
knitr::opts_chunk$set(
	eval = TRUE,
	echo = TRUE,
	warning = FALSE,
	collapse = FALSE,
	tidy = TRUE
)
```


# Chunk 1: Data Loading and Exploration
This code chunk initiates the data analysis process by loading the dataset "Argos_UK.csv" into R using the read_csv function from the readr library. The subsequent step involves visually inspecting the dataset using the View function. After loading, the script checks the dimensions of the dataset using dim and inspects the data types of each column using sapply. The presence of missing values is addressed by replacing them with the respective column means using the lapply function, ensuring a complete and usable dataset.
```{r }
library(readr)
Argos_UK <- read_csv("data/Argos_UK.csv")

# View the data
View(Argos_UK)

# Check dimensions and data types
dim(Argos_UK)
sapply(Argos_UK, class)

# Replace missing values with column means
Argos_UK <- as.data.frame(lapply(Argos_UK, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
```

# Chunk 2: Descriptive Statistics and Visualization
In this section, the script conducts a thorough exploration of the dataset. For specific columns identified in columns_to_analyze (2nd, 4th, and 7th columns), it calculates frequency and percentage tables. The mode is computed for columns such as "Product.Price," "Category," and "Product.Type." Summary statistics are generated using the summary function, and descriptive statistics, including standard deviation, variance, kurtosis, and skewness, are computed for the "Product Price" column. Additionally, the script creates bar plots for the 7th column and visualizes missing values using the missmap function. The correlation matrix is plotted using corrplot.
```{r }
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
```

# Chunk 3: Confirmation of Missing Values and Data Transformation
This chunk begins by confirming the presence of missing values in the dataset using colSums. It then proceeds with data imputation by replacing missing values with column means using the lapply function. Subsequent data transformations include log transformation for the "Product Price" column, Z-score normalization for numeric columns, Box-Cox transformation for positive numeric columns (if applicable), and one-hot encoding for categorical columns using model.matrix.
```{r }
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

```

# Chunk 4: Loading Libraries for Modeling
This section loads essential libraries required for subsequent modeling steps. These include readr, caret, rpart, kknn, ROCR, pROC, MASS, and readr. The dataset is reloaded using read_csv, and any remaining missing values are replaced with column means using lapply for consistency in the dataset.


```{r }
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
```

# Chunk 5: Data Splitting and Bootstrapping
This code chunk focuses on preparing the dataset for model training. First, the dataset is split into training and testing sets using the createDataPartition function from the caret package. The random seed is set for reproducibility, and approximately 75% of the data is assigned to the training set (Argos_UK_train), while the remaining 25% is allocated to the testing set (Argos_UK_test). This splitting facilitates the evaluation of model performance on an independent dataset.

After the data splitting, the script proceeds to demonstrate bootstrapping using the boot function. This technique involves resampling with replacement to estimate the sampling distribution of a statistic. In this case, the statistic being estimated is related to the "Product Price" column. The results of the bootstrapping procedure are stored in the bootstrap_results variable, providing insights into the variability of the chosen statistic.

```{r}

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
```

# Chunk 6: Cross-Validation and Model Training
This section employs cross-validation to assess the performance of machine learning models. The trainControl function is used to define the cross-validation settings, specifying a method of "cv" (cross-validation) with 5 folds. Two random forest models (ProductPrice_model and ProductPrice2_model) are then trained on the training set (Argos_UK_train) using the train function from the caret package. The rf method is selected for the random forest algorithm.

Following the model training, the script compares the performance of the two models using the resamples function. This function aggregates the results from multiple resampling iterations (cross-validation folds), allowing for a statistical comparison of their performance metrics. This comparison aids in selecting the model with superior predictive abilities.
```{r }
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

```


# Chunk 7: Model Tuning and Comparison
In this chunk, the script conducts a manual search for optimal hyperparameters in a random forest model. The tunegrid variable defines a grid of hyperparameter values for the mtry parameter, which represents the number of variables randomly sampled as candidates at each split. The script then iterates over different values for the number of trees (ntree) and tunes the mtry parameter using the train function. The results for each iteration are stored in the modellist variable.

To identify the optimal hyperparameters, the script compares the performance of the models using the resamples function and presents a summary. The dotplot function visualizes the comparison results, aiding in the selection of hyperparameter values that minimize the root mean squared error (RMSE).
```{r }
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
```

# Chunk 8: Bagging, Boosting, and Stacking
This section introduces three ensemble learning techniques: bagging, boosting, and stacking. For bagging, a random forest model (bag_model) is trained on the training set with 500 trees. The model's performance is then evaluated on the test set, and the root mean squared error (RMSE) is computed. The caret model for the bagged model (bag_caret_model) is created, incorporating the model and cross-validation settings.

For boosting, a gradient boosting machine (boost_model) is trained using the train function. The caret model for the boosting model (boost_caret_model) is also created. Finally, stacking is implemented by creating a list of caret models for bagging and boosting (models_for_stacking) and combining them using the caretStack function. The stacked model's performance is assessed on the test set, and the RMSE is computed. This chunk provides a comprehensive comparison of the three ensemble methods, offering insights into their effectiveness in improving predictive performance.
```{r }

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

```