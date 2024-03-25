# Importing the required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(zoo) # useful for filling missing values with last observed value
library(reshape2)  # for melt()

# Options
options(warn=-1)                # Equivalent to suppressing warnings
options(max.print=100)          # Display up to 100 columns in the console

# read the features and labels dataset
df <- read.csv('./Hands_On/Hands_on_Preprocessing/dengue_features_train.csv')
labels <- read.csv('./Hands_On/Hands_on_Preprocessing/dengue_labels_train.csv')

# merge the label(total_cases) with all the features
data <- merge(df, labels, by = c("city", "year", "weekofyear"))
data

# The Dataset have data for two region San Juan and Iquitos
# In this project will only focus on San Juan
# So, let's take only San Juan rows
sj <- data[data$city == 'sj', ]
sj <- sj[ , !(names(sj) %in% 'city')]
sj

# converting 'week_start_date' object to datetime
sj$week_start_date <- as.Date(sj$week_start_date)

# set 'week_start_date' as index
# a date index help us to work with time-series data a bit easier
rownames(sj) <- sj$week_start_date

# drop 'week_start_date' column as we already have it in index
sj <- sj[, !(names(sj) %in% "week_start_date")]

train_sj <- sj[1:800, ]
test_sj <- sj[801:nrow(sj), ]
summary(train_sj)

# Exploring total_cases for 800 weeks
# for around 650 weeks weekly dengue cases are less than 50
# also for some weeks total_cases was greater than 400
cat('mean: ', mean(train_sj$total_cases), '\n')
cat('st.dev :', sd(train_sj$total_cases), '\n')
hist(train_sj$total_cases)

ggplot(train_sj, aes(x = 1:nrow(train_sj), y = total_cases)) +
  geom_line() +
  theme_bw() +
  labs(x = "Index", y = "Total Cases") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1/5) + # Adjust the ratio for better visualization, similar to figsize in Python
  theme(plot.background = element_rect(fill = "white", colour = "grey50")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_line(colour = "grey80")) +
  theme(panel.grid.minor = element_line(colour = "grey95")) +
  theme(legend.background = element_rect(fill = "white"))

## checking null valus
sort(colSums(is.na(train_sj)), decreasing = TRUE)

# train_sj forward filling - Filled with the last observed value
train_sj <- as.data.frame(lapply(train_sj, zoo::na.locf))

#let's download the training and testing dataset to further use them in modeling later
write.csv(train_sj, "train_sj.csv", row.names = FALSE)
write.csv(test_sj, "test_sj.csv", row.names = FALSE)

sj_correlations <-cor(train_sj)

# Melt the correlation matrix to create a dataframe suitable for ggplot2
melted_correlations <- melt(sj_correlations)

# Plot the heatmap
ggplot(data = melted_correlations, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed()

# Assuming 'sj_correlations' is your correlation matrix
# Extract the correlation vector for 'total_cases' and remove the self-correlation
cor_vector <- sj_correlations['total_cases', -which(colnames(sj_correlations) == 'total_cases')]

# Sort the correlations in descending order
sorted_cor_vector <- sort(cor_vector, decreasing = TRUE)

# Create a horizontal bar plot
barplot(sorted_cor_vector, horiz = TRUE, names.arg = names(sorted_cor_vector),
        main = "Correlations with 'total_cases'", xlab = "Correlation Value", las=2,cex.names=0.8)

# Sort in descending order for correlation and create a dataframe for plotting
plot_data <- data.frame(
  variable = names(total_cases_correlations),
  correlation = as.numeric(total_cases_correlations)
)

# Sort variable factor levels by their correlation values for plotting
plot_data$variable <- factor(plot_data$variable, levels = plot_data$variable[order(-plot_data$correlation)])  # Note the negative sign to sort in descending order

# Plot
ggplot(plot_data, aes(x = correlation, y = variable)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Correlation with 'total_cases'", x = "Correlation", y = "")
