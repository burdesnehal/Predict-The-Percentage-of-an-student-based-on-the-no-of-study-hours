# Step 1: Read the dataset from the provided link
url <- "http://bit.ly/w-data"
s_data <- read.csv(url)
summary(s_data)

# Step 2: Plot the distribution of scores
ggplot(data = s_data, aes(x = Hours, y = Scores)) +
  geom_point() +
  labs(title = "Hours vs Percentage",
       x = "Hours Studied",
       y = "Percentage Score")

# The next step is to divide the data into "attributes" (inputs) and "labels" (outputs).
X <- s_data$Hours
y <- s_data$Scores

# Split the data into training and testing sets
set.seed(123) # Set a seed for reproducibility
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
train_data <- s_data[train_index, ]
test_data <- s_data[-train_index, ]

# Divide the data into attributes (inputs) and labels (outputs)
X_train <- train_data$Hours
y_train <- train_data$Scores
X_test <- test_data$Hours
y_test <- test_data$Scores

# Fit a linear regression model
model <- lm(Scores ~ Hours, data = train_data)

# Summarize the model
summary(model)

# Make predictions on the test set
y_pred <- predict(model, newdata = test_data)

ggplot(data = s_data, aes(x = Hours, y = Scores)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(data = test_data, aes(x = Hours, y = Scores), color = "blue", size = 4) +
  labs(title = "Regression Line and Test Data",
       x = "Hours Studied",
       y = "Percentage Score")

# Create a data frame with actual and predicted scores
comparison <- data.frame(Actual = y_test, Predicted = y_pred)

# Print the comparison
print(comparison)

# Define the hours for prediction
hours <- 9.25

# Predict the score using the trained linear regression model
own_pred <- predict(model, newdata = data.frame(Hours = hours))

# Print the predicted score
print(own_pred)