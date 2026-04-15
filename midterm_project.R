# Task 1: Handling Missing Values in the Dataset
# Define strings that represent missing values in the CSV
## Task 1: Handling Missing Values in the Dataset

na_values <- c("", " ", "NA")
# Read the dataset and treat specified strings as NA
dataset <- read.csv("Final_SP_dataSet.csv", na.strings = na_values)

# Calculate and display the count of missing values for each column
missing_counts <- colSums(is.na(dataset))
cat("Missing values per column before processing:\n")
print(missing_counts)

# dataset$Age <- as.character(dataset$Age)
# dataset$Age <- gsub("O", "0", dataset$Age)
# dataset$Age <- gsub("Z", "2", dataset$Age)
# dataset$Age <- as.numeric(dataset$Age)


# Method 1: Remove all rows that contain at least one missing value
dataset_discarded <- na.omit(dataset)
cat("\nMethod 1: Discard Instances\n")
cat("Original rows:", nrow(dataset), "\n")
cat("Rows after discarding instances:", nrow(dataset_discarded), "\n")


# Method 2: Create a copy and fill missing values using Mean (Numeric) or Mode (Categorical)
dataset_replaced <- dataset

# Helper function to find the most frequent value (Mode) in a vector
get_mode <- function(v) {
    v <- v[!is.na(v)]
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Iterate through columns and apply replacement strategy
for (col in names(dataset_replaced)) {
    if (any(is.na(dataset_replaced[[col]]))) {
        if (is.numeric(dataset_replaced[[col]])) {
            # Replace missing numeric values with the mean of the column
            dataset_replaced[[col]][is.na(dataset_replaced[[col]])] <- round(mean(dataset_replaced[[col]], na.rm = TRUE))
        } else {
            # Replace missing categorical values with the mode of the column
            dataset_replaced[[col]][is.na(dataset_replaced[[col]])] <- get_mode(dataset_replaced[[col]])
        }
    }
}

cat("\nMethod 2: Replace by Most Frequent / Average Value\n")
cat("Remaining missing values after replacement:\n")
print(colSums(is.na(dataset_replaced)))


## Task 2: Detect and Handle Outliers
dataset_outlier_handled <- dataset_replaced

# Function to handle outliers using the Interquartile Range (IQR) method
handle_outliers_iqr <- function(data_vec) {
    # Calculate 1st and 3rd quartiles
    Q1 <- quantile(data_vec, 0.25, na.rm = TRUE)
    Q3 <- quantile(data_vec, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1

    # Define bounds for outliers
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val

    # Identify and count outliers
    outliers <- data_vec < lower_bound | data_vec > upper_bound
    cat("Outliers detected:", sum(outliers), "\n")
    cat("Lower Bound:", lower_bound, " | Upper Bound:", upper_bound, "\n\n")

    # Cap outliers at the lower and upper bounds (Winsorization)
    data_vec[data_vec < lower_bound] <- lower_bound
    data_vec[data_vec > upper_bound] <- upper_bound

    return(data_vec)
}

cat("\n--- Task 2: Outlier Detection and Handling for 'Age' ---\n")

dataset_outlier_handled$Age <- handle_outliers_iqr(dataset_outlier_handled$Age)

cat("Summary of Age BEFORE outlier handling:\n")
print(summary(dataset_replaced$Age))

cat("\nSummary of Age AFTER outlier handling:\n")
print(summary(dataset_outlier_handled$Age))


## Task 3: Attribute Conversion (Numeric to Categorical & Categorical to Numeric)

dataset_converted <- dataset_outlier_handled

cat("\n--- Task 3: Attribute Conversion ---\n")

cat("Converting 'Stress Level' to numeric...\n")
# Map categorical Stress Level to numeric values (Ordinal Encoding)
dataset_converted$SL_Numeric <- as.numeric(factor(dataset_converted$Stress.Level,
    levels = c("Low", "Moderate", "High")
))

cat("Converting 'Academic Performance' to numeric...\n")
# Clean strings and map Academic Performance to numeric values
cleaned_academic <- gsub("[^[:alpha:]]", "", dataset_converted$Academic.Performance)
dataset_converted$AP_Numeric <- as.numeric(factor(cleaned_academic,
    levels = c("Poor", "Average", "Good", "Excellent")
))

cat("Converting 'Age' to categorical Age groups...\n")
# Bin Age into categorical groups (Teens, Youth, Adults)
dataset_converted$Age_Group <- cut(dataset_converted$Age,
    breaks = c(0, 19, 29, Inf),
    labels = c("Teen", "Youth", "Adult")
)

cat("\nSample of converted attributes:\n")
print(head(dataset_converted[, c("Age", "Age_Group", "Stress.Level", "SL_Numeric", "Academic.Performance", "AP_Numeric")]))

cat("\nValue counts for new Age_Group column:\n")
print(table(dataset_converted$Age_Group))


## Task 4: Normalization of Continuous Attributes

dataset_normalized <- dataset_converted

cat("\n--- Task 4: Normalization ---\n")

# Function to scale values between 0 and 1
min_max_norm <- function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

cat("Applying Min-Max Normalization to 'Age'...\n")
dataset_normalized$Age_Normalized <- min_max_norm(dataset_normalized$Age)

cat("\nSummary of Normalized Age (Min-Max):\n")
print(summary(dataset_normalized$Age_Normalized))

cat("\nSample of normalized results:\n")
print(head(dataset_normalized[, c("Age", "Age_Normalized")]))


## Task 5: Handle Duplicate Rows

cat("\n--- Task 5: Handle Duplicate Rows ---\n")

# Count duplicate entries in the dataset
duplicate_count <- sum(duplicated(dataset_normalized))
cat("Number of duplicate rows found:", duplicate_count, "\n")

if (duplicate_count > 0) {
    cat("Duplicate rows detected. Removing duplicates...\n")
    dataset_final <- dataset_normalized[!duplicated(dataset_normalized), ]
} else {
    cat("No duplicate rows found. Keeping dataset as is.\n")
    dataset_final <- dataset_normalized
}

cat("Original Row Count:", nrow(dataset_normalized), "\n")
cat("Final Row Count (After Removing Duplicates):", nrow(dataset_final), "\n")


## Task 6: Data Filtering Methods

cat("\n--- Task 6: Data Filtering ---\n")

# Filter for specific conditions: High Stress, Poor Performance, or Age thresholds
cat("Filtering students with High Stress Level...\n")
high_stress_students <- dataset_final[dataset_final$Stress.Level == "High", ]
cat("Count of High Stress students:", nrow(high_stress_students), "\n")

cat("Filtering students with Poor Academic Performance...\n")
poor_performance_students <- dataset_final[trimws(dataset_final$Academic.Performance) == "Poor", ]
cat("Count of Poor Performance students:", nrow(poor_performance_students), "\n")

cat("Filtering at-risk students (High Stress AND Poor Performance)...\n")
at_risk_students <- dataset_final[dataset_final$Stress.Level == "High" &
    trimws(dataset_final$Academic.Performance) == "Poor", ]
cat("Count of At-Risk students:", nrow(at_risk_students), "\n")
cat("Filtering students older than 25...\n")
older_students <- dataset_final[dataset_final$Age > 25, ]
cat("Count of students older than 25:", nrow(older_students), "\n")

cat("\nSample of results for At-Risk Students:\n")
print(head(at_risk_students[, c("Age", "Stress.Level", "Academic.Performance")]))


## Task 7: Handle Invalid Data

cat("\n--- Task 7: Handle Invalid Data ---\n")

valid_genders <- c("Male", "Female")
valid_stress <- c("Low", "Moderate", "High")
valid_academic <- c("Poor", "Average", "Good", "Excellent")

dataset_invalid_fixed <- dataset_final

# Identify invalid data points using regex and predefined criteria
dataset_invalid_fixed$Age <- as.numeric(gsub("[^0-9.]", "", as.character(dataset_invalid_fixed$Age)))
invalid_age_count <- sum(dataset_invalid_fixed$Age < 10 | dataset_invalid_fixed$Age > 60, na.rm = TRUE)

cleaned_stress <- gsub("[^[:alpha:]]", "", dataset_invalid_fixed$Stress.Level)
invalid_stress_count <- sum(!(cleaned_stress %in% valid_stress))

cleaned_acad <- gsub("[^[:alpha:]]", "", dataset_invalid_fixed$Academic.Performance)
invalid_academic_count <- sum(!(cleaned_acad %in% valid_academic))

cat("Invalid/Out-of-bounds ages found:", invalid_age_count, "\n")
cat("Invalid Stress Levels found:", invalid_stress_count, "\n")
cat("Invalid Academic Performance found:", invalid_academic_count, "\n")

# Replace invalid values with statistically representative ones (Mean/Mode)
if (invalid_age_count > 0) {
    dataset_invalid_fixed$Age[dataset_invalid_fixed$Age < 10 | dataset_invalid_fixed$Age > 60] <- round(mean(dataset_invalid_fixed$Age, na.rm = TRUE))
}

if (invalid_stress_count > 0) {
    mode_stress <- get_mode(dataset_invalid_fixed$Stress.Level)
    dataset_invalid_fixed$Stress.Level[!(cleaned_stress %in% valid_stress)] <- mode_stress
}

if (invalid_academic_count > 0) {
    mode_academic <- get_mode(dataset_invalid_fixed$Academic.Performance)
    dataset_invalid_fixed$Academic.Performance[!(cleaned_acad %in% valid_academic)] <- mode_academic
}

cat("\nResults after handling invalid data:\n")
cat("- Remaining invalid ages:", sum(dataset_invalid_fixed$Age < 10 | dataset_invalid_fixed$Age > 60, na.rm = TRUE), "\n")

final_cleaned_stress <- gsub("[^[:alpha:]]", "", dataset_invalid_fixed$Stress.Level)
cat("- Remaining invalid Stress Levels:", sum(!(final_cleaned_stress %in% valid_stress)), "\n")

final_cleaned_acad <- gsub("[^[:alpha:]]", "", dataset_invalid_fixed$Academic.Performance)
cat("- Remaining invalid Academic Performance:", sum(!(final_cleaned_acad %in% valid_academic)), "\n")

cat("\nInvalid data handled successfully by replacing with Mean/Mode values.\n")



## Task 8: Convert Imbalanced to Balanced Dataset

cat("\n--- Task 8: Dataset Balancing ---\n")

cat("Original distribution of 'Suicide Attempt':\n")
class_counts <- table(dataset_invalid_fixed$Suicide.Attempt)
print(class_counts)

# Balancing Method 1: Subsample majority classes to match the size of the smallest class
min_class_size <- min(class_counts[class_counts > 0])
dataset_undersampled <- do.call(rbind, lapply(
    split(dataset_invalid_fixed, dataset_invalid_fixed$Suicide.Attempt),
    function(df) df[sample(nrow(df), min_class_size), ]
))

cat("Distribution after under-sampling:\n")
print(table(dataset_undersampled$Suicide.Attempt))

# Balancing Method 2: Resample minority classes to match the size of the largest class
max_class_size <- max(class_counts)
dataset_oversampled <- do.call(rbind, lapply(
    split(dataset_invalid_fixed, dataset_invalid_fixed$Suicide.Attempt),
    function(df) df[sample(nrow(df), max_class_size, replace = TRUE), ]
))

cat("Distribution after over-sampling:\n")
print(table(dataset_oversampled$Suicide.Attempt))


## Task 9: Split the Dataset (Training and Testing)

cat("\n--- Task 9: Training and Testing Split ---\n")

# Define training ratio and randomly sample indices for the split
set.seed(123)

train_percentage <- 0.7
sample_size <- floor(train_percentage * nrow(dataset_invalid_fixed))

train_indices <- sample(seq_len(nrow(dataset_invalid_fixed)), size = sample_size)

# Allocate data to training and testing sets
train_set <- dataset_invalid_fixed[train_indices, ]
test_set <- dataset_invalid_fixed[-train_indices, ]

cat("Total observations in original dataset:", nrow(dataset_invalid_fixed), "\n")
cat("Observations in Training Set (70%):", nrow(train_set), "\n")
cat("Observations in Testing Set (30%):", nrow(test_set), "\n")
cat("Overlap between sets:", length(intersect(rownames(train_set), rownames(test_set))), "\n")


## Task 10: Descriptive Statistics and Interpretation

cat("\n--- Task 10: Descriptive Statistics ---\n")

# Calculate central tendency and dispersion for different groups
cat("Overall Summary of Age (Full Dataset):\n")
print(summary(dataset_invalid_fixed$Age))
cat("Standard Deviation of Age:", sd(dataset_invalid_fixed$Age, na.rm = TRUE), "\n")

cat("\nMean Age by Suicide Attempt:\n")
mean_by_target <- aggregate(Age ~ Suicide.Attempt, data = dataset_invalid_fixed, FUN = mean)
print(mean_by_target)

cat("\nMedian Age by Suicide Attempt:\n")
median_by_target <- aggregate(Age ~ Suicide.Attempt, data = dataset_invalid_fixed, FUN = median)
print(median_by_target)

cat("\nStandard Deviation by Suicide Attempt:\n")
sd_by_target <- aggregate(Age ~ Suicide.Attempt, data = dataset_invalid_fixed, FUN = sd)
print(sd_by_target)


cat("\n--- Result Interpretation ---\n")

mean_by_target$CleanLabel <- tolower(trimws(gsub("[^[:alpha:]]", "", mean_by_target$Suicide.Attempt)))

val_attempted <- mean_by_target$Age[mean_by_target$CleanLabel == "attempted"]
val_thought <- mean_by_target$Age[mean_by_target$CleanLabel == "thought"]
val_never <- mean_by_target$Age[mean_by_target$CleanLabel == "neverthought"]

cat(
    "Interpretation: The mean age for students who 'Attempted' is ",
    round(as.numeric(val_attempted[1]), 2), " years, while those who 'Never Thought' is ",
    round(as.numeric(val_never[1]), 2), " years.\n"
)


## Task 11: Compare Average Values Across Categories

cat("\n--- Task 11: Comparison Across Categories ---\n")

# Analyze relationships between categories like Gender, Relationships, and Stress
compare_academic <- aggregate(AP_Numeric ~ Gender,
    data = dataset_invalid_fixed,
    FUN = mean
)

mean_male <- compare_academic$AP_Numeric[compare_academic$Gender == "Male"]
mean_female <- compare_academic$AP_Numeric[compare_academic$Gender == "Female"]

cat("Average Academic Performance Score (1-4 Scale):\n")
cat("- Male Students:", round(mean_male, 2), "\n")
cat("- Female Students:", round(mean_female, 2), "\n")

cat("\nRelation 2: Stress Level by Relationship Condition:\n")
stress_by_relation <- aggregate(SL_Numeric ~ Relationship.Condition,
    data = dataset_invalid_fixed, FUN = mean
)
print(stress_by_relation)

cat("\nRelation 3: Academic Performance by Suicide Attempt:\n")
acad_by_suicide <- aggregate(AP_Numeric ~ Suicide.Attempt,
    data = dataset_invalid_fixed, FUN = mean
)
print(acad_by_suicide)

cat("\n--- Result Interpretation (Combined) ---\n")
diff_val <- abs(mean_male - mean_female)
cat("1. Gender Comparison: Male students have a performance gap of ", round(diff_val, 2), " points vs Females.\n")

val_attempted_acad <- acad_by_suicide$AP_Numeric[acad_by_suicide$Suicide.Attempt == "Attempted"]
val_never_acad <- acad_by_suicide$AP_Numeric[acad_by_suicide$Suicide.Attempt == "Never Thought"]
cat(
    "2. Suicide & Performance: Students who 'Attempted' have an average academic score of ",
    round(val_attempted_acad[1], 2), " while 'Never Thought' groups score ", round(val_never_acad[1], 2), ".\n"
)


## Task 12: Compare Variability Across Categories
cat("\n--- Task 12: Variability Across Categories ---\n")

cleaned_acad_performance <- trimws(gsub("[^[:alpha:]]", "", dataset_invalid_fixed$Academic.Performance))

cat("Dispersion Metrics for Age by Academic Performance:\n")

# Calculate multiple dispersion statistics to understand data spread within categories
sd_comp <- aggregate(Age ~ Academic.Performance, data = dataset_invalid_fixed, FUN = sd)
names(sd_comp)[2] <- "SD"

var_comp <- aggregate(Age ~ Academic.Performance, data = dataset_invalid_fixed, FUN = var)
names(var_comp)[2] <- "Variance"

iqr_comp <- aggregate(Age ~ Academic.Performance, data = dataset_invalid_fixed, FUN = IQR)
names(iqr_comp)[2] <- "IQR"

range_comp <- aggregate(Age ~ Academic.Performance,
    data = dataset_invalid_fixed,
    FUN = function(x) diff(range(x))
)
names(range_comp)[2] <- "Range"

# Merge all dispersion metrics into a single summary table
dispersion_summary <- merge(merge(merge(sd_comp, var_comp), iqr_comp), range_comp)
print(dispersion_summary)

cat("\nRelation 2: Age Variability by Gender:\n")
sd_gender <- aggregate(Age ~ Gender, data = dataset_invalid_fixed, FUN = sd)
names(sd_gender)[2] <- "SD"
iqr_gender <- aggregate(Age ~ Gender, data = dataset_invalid_fixed, FUN = IQR)
names(iqr_gender)[2] <- "IQR"
gender_variability <- merge(sd_gender, iqr_gender)
print(gender_variability)

cat("\n--- Result Interpretation ---\n")
cat(
    "1. Academic Link: The group with the most stable ages is the '",
    as.character(dispersion_summary[which.min(dispersion_summary$SD), 1]), "' group.\n"
)

cat(
    "2. Gender Stability: ", as.character(gender_variability[which.min(gender_variability$SD), 1]),
    " students show higher age consistency (Lower SD: ", round(min(gender_variability$SD), 2), ") than the other gender group.\n"
)
