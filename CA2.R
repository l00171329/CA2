# Step 1: Data Exploration and Research Question
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
data <- read.csv("diabetes.csv")

# Explore dataset structure
str(data)

# Define a research question
# For example, "Can we predict the risk of diabetes based on various health and lifestyle factors in this dataset?"

# Step 2: Data Preprocessing
# Handle missing data (if any)
# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values

# Encode categorical variables if necessary (if any)
# Example: Encoding 'Sex' variable
data$Sex <- as.factor(data$Sex)

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_ratio <- 0.7  # 70% for training, 30% for testing
sample_index <- sample(1:nrow(data), size = floor(train_ratio * nrow(data)))
train_data <- data[sample_index, ]
test_data <- data[-sample_index, ]

# Step 3: Build a Predictive Model (Logistic Regression Example)
library(glmnet)

# Define predictors and target variable
predictors <- c("HighBP", "HighChol", "BMI", "Smoker", "HeartDiseaseorAttack")
target <- "Diabetes_binary"

# Train a logistic regression model
model <- glm(formula = as.formula(paste(target, "~", paste(predictors, collapse = "+"))),
             family = binomial(link = "logit"), data = train_data)

# Step 4: Model Evaluation
# Evaluate the model using appropriate metrics
# Example: Accuracy
predicted_probs <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
accuracy <- sum(predicted_classes == test_data$Diabetes_binary) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")

# You can also calculate p-values for coefficients if using logistic regression
summary(model)

# Step 5: Model Forecasting
# Devise test cases with variations in independent variables
# Example test case
new_data <- data.frame(
  HighBP = 1,
  HighChol = 1,
  BMI = 30,
  Smoker = 1,
  HeartDiseaseorAttack = 0
)

# Use the trained model to make predictions for test cases
predicted_probability <- predict(model, newdata = new_data, type = "response")
predicted_class <- ifelse(predicted_probability > 0.5, 1, 0)
cat("Predicted Probability:", predicted_probability, "\n")
cat("Predicted Class:", predicted_class, "\n")
