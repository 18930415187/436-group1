# Load necessary libraries
# Load necessary libraries
# Load necessary libraries

# Bar plot for the different medals
library(ggplot2)
library(dplyr)
library(tidyr)

# Define the data
data <- data.frame(
  Country = c("China", "USA", "Russia", "Great Britain", "Germany", "Australia", "South Korea", "Japan", "Italy"),
  Gold = c(51, 36, 23, 19, 16, 14, 13, 9, 8),
  Silver = c(28, 36, 23, 15, 16, 17, 8, 6, 10),
  Bronze = c(21, 38, 21, 13, 10, 15, 10, 10, 10)
)

# Calculate total medals for each country and add it as a new column
data <- data %>%
  mutate(Total = Gold + Silver + Bronze) %>%
  arrange(desc(Total))

# Transform data to long format for the grouped bar plot
data_long <- data %>%
  pivot_longer(cols = c(Gold, Silver, Bronze), 
               names_to = "Medal", 
               values_to = "Count")

# Create the plot
ggplot() +
  # Layer 1: Total medals bar plot with label for total count
  geom_bar(data = data, aes(x = reorder(Country, -Total), y = Total), 
           stat = "identity", fill = "lightblue", width = 0.6, alpha = 0.5) +
  geom_text(data = data, aes(x = reorder(Country, -Total), y = Total, label = Total), 
            vjust = -0.5, color = "blue", size = 3.5) +
  
  # Layer 2: Stacked medals bar plot with labels for each medal count
  geom_bar(data = data_long, aes(x = reorder(Country, -Total), y = Count, fill = Medal), 
           stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  geom_text(data = data_long, aes(x = reorder(Country, -Total), y = Count, label = Count, fill = Medal), 
            position = position_dodge(width = 0.6), vjust = -0.5, color = "black", size = 3) +
  
  # Customizing fill colors for medals
  scale_fill_manual(values = c("Gold" = "goldenrod", "Silver" = "gray70", "Bronze" = "sienna")) +
  
  # Labels and title
  labs(title = "Olympic Medals by Country", 
       x = "Country", 
       y = "Number of Medals") +
  
  # Theme customization
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Bubble plot 

library(ggplot2)
library(ggimage)



# Example Data
olympic_data <- data.frame(
  Sport = c("Basketball", "Swimming", "Volleyball", "Gymnastics"),
  Events = c(12, 34, 24, 12),  # Number of events (size of bubbles)
  Image = c('/Users/guowanyi/Desktop/fall 2024/Stats 436/Project/BasketBall.png', 
            '/Users/guowanyi/Desktop/fall 2024/Stats 436/Project/Swimming.png',
            '/Users/guowanyi/Desktop/fall 2024/Stats 436/Project/Volleyball.png',
            '/Users/guowanyi/Desktop/fall 2024/Stats 436/Project/Gym.png')
)

# The bubble plot
ggplot(olympic_data, aes(x = Sport, y = Events)) +
  geom_point(aes(size = Events), color = "blue", alpha = 0.5) +
  geom_image(aes(image = Image), size = 0.1, by = "width") +
  scale_size_continuous(range = c(5, 20)) +  # Adjust size range
  labs(title = "Olympic Sports Bubble Plot", 
       x = "Sport", 
       y = "Number of Events") +
  theme_minimal()












