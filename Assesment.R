student_data <- data.frame(
  student_id = 1:17,
  no_visual_aids = c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61),
  with_visual_aids = c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)
)

# structure of the data frame
str(student_data)

# Creating a boxplot comparing scores with and without visual aids
windows(20,12)
boxplot(
  student_data$no_visual_aids, student_data$with_visual_aids,
  names = c("No Visual Aids", "With Visual Aids"),
  main = "Comparison of Scores",
  ylab = "Scores",
  col = c("blue", "red")
)

# Installing and loading the e1071 Library 
install.packages("e1071")
library(e1071)

# Calculating the score differences between scores with and without visual aids
score_diff <- student_data$with_visual_aids - student_data$no_visual_aids

# Plotting QQ plot for score differences
windows(20,12)
qqnorm(score_diff, main = "QQ Plot for Score Differences")
qqline(score_diff, col = "blue")

# By using  histogram we can visualize the distribution of score differences
windows(20,12)
hist(score_diff, breaks = 15, col = "green", border = "black", 
     main = "Histogram of Score Differences",
     xlab = "Score Differences")

# Perform Shapiro-Wilk test for normality on score differences
shapiro_test_diff <- shapiro.test(score_diff)
shapiro_test_diff  # Output of the Shapiro-Wilk test

# Summary statistics for score differences
summary(score_diff)

# Combining scores from both groups into one vector
all_scores <- c(student_data$no_visual_aids, student_data$with_visual_aids)
all_scores

# Conducting a normality test on the combined scores
combined_normality <- shapiro.test(all_scores)
combined_normality
#Mean and Median of combined scores
mean_all_scores <- mean(all_scores)
mean_all_scores  # The mean of all scores
median_all_scores <- median(all_scores)
median_all_scores  # The median of all scores

# Calculating the skewness of the combined scores
skewness_all_scores <- skewness(all_scores)
skewness_all_scores  # Skewness of all scores

# Calculating the mean and standard deviation for both conditions
mean_no_visual_aids <- mean(student_data$no_visual_aids)
mean_no_visual_aids
mean_with_visual_aids <- mean(student_data$with_visual_aids)
mean_with_visual_aids
std_no_visual_aids <- sd(student_data$no_visual_aids)
std_no_visual_aids
std_with_visual_aids <- sd(student_data$with_visual_aids)
std_with_visual_aids

# Performing a paired t-test comparing the two conditions
paired_t_test <- t.test(student_data$no_visual_aids, student_data$with_visual_aids, paired = TRUE)
paired_t_test  # Output of the t-test

