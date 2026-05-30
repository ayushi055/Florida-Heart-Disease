library(xgboost)
library(caret)
library(randomForest)
library(data.table)

# Load the data
hd_cl = data.table::fread("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data")
hd_hu = data.table::fread("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.hungarian.data")
hd_ch = data.table::fread("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.switzerland.data")
hd_va = data.table::fread("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.va.data")

# Add location variable for each dataset
hd_ch$location = "ch"
hd_cl$location = "cl"
hd_hu$location = "hu"
hd_va$location = "va"

# Combine the four locations into one dataset
hd = rbind(hd_cl, hd_ch, hd_hu, hd_va)

# Add column names
colnames(hd) = c(
  "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", 
  "exang", "oldpeak", "slope", "ca", "thal", "num", "location"
)

# Switch "?" to NA
hd[hd == "?"] = NA

# Clean up
rm(hd_cl, hd_hu, hd_ch, hd_va)

# Convert 'num' to binary (0 = no disease, 1 = heart disease present)
hd$num = ifelse(hd$num == 0, 0, 1)

# Select the specified columns: age, sex, trestbps, chol, fbs, exang, and num
hd_selected = hd[, .(age, sex, trestbps, chol, fbs, exang, num)]

# Remove rows with missing values
hd_selected_cleaned = na.omit(hd_selected)

# Convert categorical variables into numeric for column name matching 
hd_selected_cleaned$sex = as.numeric(hd_selected_cleaned$sex)
hd_selected_cleaned$exang = as.numeric(hd_selected_cleaned$exang)

# Convert continuous variables to numeric
hd_selected_cleaned$chol = as.numeric(hd_selected_cleaned$chol)
hd_selected_cleaned$age = as.numeric(hd_selected_cleaned$age)
hd_selected_cleaned$trestbps = as.numeric(hd_selected_cleaned$trestbps)

# Split the data into training and testing sets
set.seed(123)
trainIndex = createDataPartition(hd_selected_cleaned$num, p = 0.8, list = FALSE)
train_data = hd_selected_cleaned[trainIndex, ]
test_data = hd_selected_cleaned[-trainIndex, ]

# Create the model matrix for train and test sets
train_matrix = model.matrix(num ~ sex + age + trestbps + chol + exang - 1, data = train_data)
test_matrix = model.matrix(num ~ sex + age + trestbps + chol + exang - 1, data = test_data)

# Ensure both matrices have the same column names
train_cols = colnames(train_matrix)
test_cols = colnames(test_matrix)

# Check if columns match (this is just a sanity check)
if (!all(train_cols == test_cols)) {
  stop("Column names of training and testing matrices do not match!")
}

# Extract labels (num)
train_labels = train_data$num
test_labels = test_data$num

# Convert data to xgboost DMatrix format
dtrain = xgb.DMatrix(data = train_matrix, label = train_labels)
dtest = xgb.DMatrix(data = test_matrix, label = test_labels)

# Train the XGBoost model
params = list(
  objective = "binary:logistic",   # Binary classification
  eval_metric = "logloss",          # Log loss evaluation
  max_depth = 6,                   # Depth of the tree
  eta = 0.1,                       # Learning rate
  nrounds = 100                    # Number of boosting rounds
)

model = xgb.train(params, dtrain, nrounds = params$nrounds)

# Make predictions
predictions = predict(model, dtest)
predicted_classes = ifelse(predictions > 0.5, 1, 0)

# Evaluate model performance
conf_matrix = confusionMatrix(as.factor(predicted_classes), as.factor(test_labels))
print(conf_matrix)
