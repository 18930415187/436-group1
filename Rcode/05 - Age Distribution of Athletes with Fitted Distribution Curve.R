library(ggplot2)
library(dplyr)

set.seed(42)
ages <- 20:45
athlete_counts <- sample(1:50, length(ages), replace = TRUE)
age_data <- data.frame(Age = ages, Athlete_Count = athlete_counts)

expanded_data <- age_data %>%
  uncount(weights = Athlete_Count)   

ggplot(expanded_data, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  geom_density(color = "darkblue", size = 1) +
  labs(title = "Age Distribution of Athletes with Fitted Distribution Curve",
       x = "Age", y = "Density") +
  theme_minimal()

Q1 <- quantile(expanded_data$Age, 0.25)
Q3 <- quantile(expanded_data$Age, 0.75)
IQR <- Q3 - Q1
outlier_threshold <- Q3 + 1.5 * IQR
outliers <- expanded_data$Age[expanded_data$Age > outlier_threshold]

max_outlier <- max(outliers)
cat("Most extreme outlier on the right side is age:", max_outlier, "\n")






