dataset <- read.csv("d:/Spring 25-26/IDS/Projects/Dataset/Final_SP_dataSet.csv", stringsAsFactors = FALSE)
colnames(dataset) <- trimws(colnames(dataset))

cat("Q1: Missing Value Handling\n")
data_q1 <- dataset
data_q1[data_q1 == ""] <- NA
colSums(is.na(data_q1))
data_q1 <- na.omit(data_q1)
cat("Missing values removed.\n\n")

cat("Q2: Outlier Detection\n")
data_q2 <- dataset
q1 <- quantile(data_q2$Age, 0.25)
q3 <- quantile(data_q2$Age, 0.75)
iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr
outliers <- data_q2$Age[data_q2$Age < lower | data_q2$Age > upper]
print(outliers)
data_q2 <- data_q2[data_q2$Age >= lower & data_q2$Age <= upper, ]
cat("Outliers handled by removal.\n\n")

cat("Q3: Attribute Conversion\n")
data_q3 <- dataset
data_q3$Stress_Numeric <- as.numeric(factor(data_q3$Stress.Level, levels=c("Low", "Moderate", "High")))
head(data_q3[, c("Stress.Level", "Stress_Numeric")])
cat("Stress level converted from categorical to numeric.\n\n")

cat("Q4: Normalization\n")
data_q4 <- dataset
min_age <- min(data_q4$Age)
max_age <- max(data_q4$Age)
data_q4$Age_Normalized <- (data_q4$Age - min_age) / (max_age - min_age)
head(data_q4[, c("Age", "Age_Normalized")])
cat("Min-Max Normalization applied to Age.\n\n")

cat("Q5: Duplicate Removal\n")
data_q5 <- dataset
sum(duplicated(data_q5))
data_q5 <- unique(data_q5)
cat("Duplicate rows removed.\n\n")

cat("Q6: Data Filtering\n")
data_q6 <- dataset
female_high_performance <- subset(data_q6, Gender == "Female" & Academic.Performance == "Excellent")
nrow(female_high_performance)
head(female_high_performance)
cat("Filtered for Excellent performing Female students.\n\n")

cat("Q7: Invalid Data Detection\n")
data_q7 <- dataset
invalid_age <- subset(data_q7, Age > 100)
print(invalid_age)
data_q7 <- subset(data_q7, Age <= 100)
cat("Removed records with biologically impossible age.\n\n")

cat("Q8: Data Balancing\n")
data_q8 <- dataset
table(data_q8$Suicide.Attempt)
classes <- split(data_q8, data_q8$Suicide.Attempt)
min_size <- min(sapply(classes, nrow))
balanced_data <- do.call(rbind, lapply(classes, function(x) x[sample(nrow(x), min_size), ]))
table(balanced_data$Suicide.Attempt)
cat("Dataset balanced by random undersampling.\n\n")

cat("Q9: Train/Test Split\n")
data_q9 <- dataset
set.seed(42)
idx <- sample(1:nrow(data_q9), 0.7 * nrow(data_q9))
train_data <- data_q9[idx, ]
test_data <- data_q9[-idx, ]
cat("Split ratio 70:30.\n\n")

cat("Q10: Descriptive Statistics of Numerical Variables by Target Class\n")
data_q10 <- subset(dataset, Age <= 100)
aggregate(Age ~ Suicide.Attempt, data = data_q10, summary)
cat("Interpretation: Descriptive summary of Age for each risk level.\n\n")

cat("Q11: Comparing Averages across Categories\n")
data_q11 <- subset(dataset, Age <= 100)
aggregate(Age ~ Gender, data = data_q11, mean)
cat("Insight: Comparing average age of Male and Female students.\n\n")

cat("Q12: Comparing Variability across Categories\n")
data_q12 <- subset(dataset, Age <= 100)
aggregate(Age ~ Suicide.Attempt, data = data_q12, function(x) c(IQR = IQR(x), SD = sd(x), Range = diff(range(x))))
cat("Insight: Comparing age dispersion among suicide risk categories.\n")
