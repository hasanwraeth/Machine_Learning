# Load necessary libraries
library(ggplot2) # equivalent to matplotlib.pyplot in Python

# Read the CSV files
df <- read.csv("./Hands_On/Hands_on_Preprocessing/dengue_features_train.csv")
labels <- read.csv("./Hands_On/Hands_on_Preprocessing/dengue_labels_train.csv")

# Merge the dataframes
data <- merge(df, labels, by=c('city', 'year', 'weekofyear'))

# Filter rows where city is 'sj' and drop the 'city' column
sj <- subset(data, city == 'sj', select = -city)

# Convert the 'week_start_date' column to a Date type
sj$week_start_date <- as.Date(sj$week_start_date)

# Set the 'week_start_date' column as the index (row names)
rownames(sj) <- sj$week_start_date

# Splitting the data into training and testing subsets
train_sj <- sj[1:800, ]
test_sj <- sj[793:nrow(sj), ]

# List of columns to drop
columns_to_drop <- c('precipitation_amt_mm', 'reanalysis_air_temp_k', 'reanalysis_avg_temp_k', 'reanalysis_max_air_temp_k', 
                     'reanalysis_min_air_temp_k', 'reanalysis_precip_amt_kg_per_m2', 'reanalysis_sat_precip_amt_mm', 
                     'ndvi_ne', 'ndvi_nw', 'ndvi_se', 'ndvi_sw', 'reanalysis_tdtr_k', 'week_start_date')

# Dropping the columns from train_sj and test_sj datasets
train_sj <- train_sj[, !(names(train_sj) %in% columns_to_drop)]
test_sj <- test_sj[, !(names(test_sj) %in% columns_to_drop)]

# Loading necessary library
library(scales)

# Filling NA values
train_sj[is.na(train_sj)] <- 0
test_sj[is.na(test_sj)] <- 0

# Scaling the columns
for(col in names(train_sj)) {
  train_sj[[col]] <- scale(train_sj[[col]])
  test_sj[[col]] <- scale(test_sj[[col]])
}

# Splitting the train dataset into X and y
X_train_sj <- as.matrix(train_sj[, !names(train_sj) %in% "total_cases"])
y_train_sj <- train_sj$total_cases

# Creating sequences of 7 for the train data
sj_train_x <- list()
sj_train_y <- vector()

for(i in 8:length(y_train_sj)) {
  sj_train_x[[i-7]] <- X_train_sj[(i-7):(i-1), ]
  sj_train_y[i-7] <- y_train_sj[i]
}

# Convert the lists to matrices for consistency
sj_train_x <- do.call(rbind, sj_train_x)
sj_train_y <- as.numeric(sj_train_y)

#install.packages("keras")
library(reticulate)
library(remotes)
#install.packages("remotes")
#remotes::install_github("rstudio/tensorflow")

reticulate::install_python()

library(tensorflow)
#install_tensorflow(envname = "r-tensorflow")
install_tensorflow(version = '2.13')

#remotes::install_github("rstudio/reticulate")


library(keras)
install_keras()

#use_condaenv("r4.3", required = TRUE)  # replace "r4.3" with the actual name of your conda environment if different

library(keras)
library(remotes)
library(reticulate)
#tf$constant("Hello TensorFlow!")


#tensorflow::tf_config()
#reticulate::py_config()

remotes::install_github("rstudio/reticulate")
remotes::install_github("rstudio/tensorflow")
remotes::install_github("rstudio/keras")
reticulate::miniconda_uninstall()
reticulate::install_miniconda()
keras::install_keras()


dim(sj_train_x)[3]

model_sj <- keras_model_sequential()

input_shape = c(dim(sj_train_x)[1], dim(sj_train_x)[2])

input_shape

model_sj %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(dim(sj_train_x)[1], dim(sj_train_x)[2])) %>%
  layer_lstm(units = 50) %>%
  layer_dense(units = 1)

model_sj %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam'
)

# Load the keras library
#library(keras)
#library(reticulate)

# Define the model
model_sj <- keras_model_sequential()

# Add layers to the model
model_sj %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(dim(sj_train_x)[1], dim(sj_train_x)[2])) %>%
  layer_lstm(units = 50) %>%
  layer_dense(units = 1)

# Compile the model
model_sj %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam'
)

#model_sj.fit(sj_train_x, sj_train_y, epochs=50, batch_size=1, verbose=1)
model_sj %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam()
)

# Train the model
history <- model_sj %>% fit(
  x = sj_train_x,
  y = sj_train_y,
  epochs = 50,
  batch_size = 1,
  verbose = 1
)

X_test_sj = test_sj.drop('total_cases', axis=1).values
y_test_sj = test_sj['total_cases'].values

sj_test_x, sj_test_y = [], []
for i in range(7, len(X_test_sj)):
  sj_test_x.append(X_test_sj[i-7:i])
sj_test_y.append(y_test_sj[i])
sj_test_x, sj_test_y = np.array(sj_test_x), np.array(sj_test_y)

model_sj.evaluate(sj_test_x, sj_test_y)

# Extracting the index (dates) from the test sets before transforming them to numpy arrays
sj_test_dates = test_sj.index[7:]  # Assuming the test data transformation starts from the 7th index

# Predicting the test set
y_pred_sj = model_sj.predict(sj_test_x)
y_pred_sj = [int(round(y[0])) for y in y_pred_sj]  # Flatten and round-off to the nearest integer

# Now, you can plot without encountering the NameError
fig = plt.figure(figsize=(16,7))
plt.plot(sj_train_dates, y_train_sj[7:], label="Actual train")  # Adjusted to match the length of sj_train_dates
plt.plot(sj_train_dates, y_train_pred_sj, label="Predicted train")  # Assuming y_train_pred_sj is already defined
plt.plot(sj_test_dates, y_test_sj[7:], label="Actual test")  # Adjusted to match the length of sj_test_dates
plt.plot(sj_test_dates, y_pred_sj, label="Predicted test")  # Assuming y_pred_sj is already defined
plt.legend()

