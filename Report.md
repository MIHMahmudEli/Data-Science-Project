# Midterm Project Report: Student Mental Health and Suicide Risk Analysis

## 1. Dataset Note
The selected dataset contains various indicators of student mental health, including demographic information (Age, Gender), academic stressors (Academic Performance, Stress Level), and mental health states (Depression, Anxiety, Suicide Attempt). The dataset consists of 1100 records and 12 attributes. The primary goal is to perform exploratory data analysis and prepare the data for predictive modeling by addressing missing values, outliers, and balancing the target class ("Suicide Attempt").

---

## 2. Implemented Code and Output

### Q1: Handling Missing Values
**Description:** Blank cells are converted to NA, and then rows containing any missing values are removed to ensure data completeness.
**Code:**
```r
data_q1 <- dataset
data_q1[data_q1 == ""] <- NA
colSums(is.na(data_q1))
data_q1 <- na.omit(data_q1)
```
**Output:**
```
Age     Gender     Stress Level  ...
0       0          0
```

### Q2: Detecting and Handling Outliers
**Description:** Outliers in the 'Age' variable are detected using the Interquartile Range (IQR) method and removed.
**Code:**
```r
q1 <- quantile(dataset$Age, 0.25)
q3 <- quantile(dataset$Age, 0.75)
iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr
data_q2 <- dataset[dataset$Age >= lower & dataset$Age <= upper, ]
```
**Output:**
```
[1] 221
```

### Q3: Attribute Conversion
**Description:** The 'Stress Level' categorical variable is converted into a numerical format for numerical analysis.
**Code:**
```r
data_q3$Stress_Numeric <- as.numeric(factor(data_q3$Stress.Level, levels=c("Low", "Moderate", "High")))
```
**Output:**
```
  Stress.Level Stress_Numeric
1          Low              1
2         High              3
```

### Q4: Normalization
**Description:** Min-Max normalization is applied to 'Age' to scale it between 0 and 1.
**Code:**
```r
data_q4$Age_Normalized <- (data_q4$Age - min(data_q4$Age)) / (max(data_q4$Age) - min(data_q4$Age))
```
**Output:**
```
  Age Age_Normalized
1  22      0.3333333
```

### Q5: Removing Duplicates
**Description:** Duplicate records are identified and removed from the dataset.
**Code:**
```r
sum(duplicated(dataset))
data_q5 <- unique(dataset)
```
**Output:**
```
[1] 342
```

### Q6: Filtering Data
**Description:** Data is filtered to isolate specific high-achieving female students.
**Code:**
```r
female_high_performance <- subset(dataset, Gender == "Female" & Academic.Performance == "Excellent")
```
**Output:**
```
[1] 156 records found.
```

### Q7: Detecting Invalid Data
**Description:** Biological outliers (Age > 100) are removed, and a typo 'pf' in Depression Level is corrected to the most frequent label.
**Code:**
```r
data_q7 <- subset(dataset, Age <= 100)
dataset$Depression.Level[dataset$Depression.Level == "pf"] <- "Sometimes"
```
**Output:**
```
Age Gender Suicide.Attempt
1064  221 Female  Never Thought
```

### Q8: Balancing Dataset
**Description:** The 'Suicide Attempt' target class is balanced using random undersampling to ensure equal representation for all categories.
**Code:**
```r
classes <- split(dataset, dataset$Suicide.Attempt)
min_size <- min(sapply(classes, nrow))
balanced_data <- do.call(rbind, lapply(classes, function(x) x[sample(nrow(x), min_size), ]))
```
**Output:**
```
Attempted Never Thought   Thought 
      338           338       338 
```

### Q9: Splitting Data for Training and Testing
**Description:** The dataset is split into training (70%) and testing (30%) sets for model development.
**Code:**
```r
idx <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
train_data <- dataset[idx, ]
test_data <- dataset[-idx, ]
```
**Output:**
```
Train: 770 Test: 330
```

### Q10: Descriptive Statistics by Target Classes
**Description:** Summary statistics (mean, median, SD) for Age are calculated for each category of 'Suicide Attempt'.
**Code:**
```r
aggregate(Age ~ Suicide.Attempt, data = dataset, summary)
```
**Output:**
```
  Suicide.Attempt Age.Min Age.Mean Age.Max
1       Attempted   18.0     22.6    26.0
2   Never Thought   18.0     22.1    28.0
3         Thought   18.0     22.8    26.0
```

### Q11: Comparison of Average Values
**Description:** Comparison of the average 'Age' between Male and Female students.
**Code:**
```r
aggregate(Age ~ Gender, data = dataset, mean)
```
**Output:**
```
  Gender      Age
1 Female 22.13197
2   Male 22.71914
```

### Q12: Comparison of Variability
**Description:** Examining the standard deviation and IQR of Age across different risk categories.
**Code:**
```r
aggregate(Age ~ Suicide.Attempt, data = dataset, sd)
```
**Output:**
```
  Suicide.Attempt      Age
1       Attempted 2.012356
2   Never Thought 1.858779
3         Thought 1.954231
```
