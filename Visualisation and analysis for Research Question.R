library(ggplot2)
data <- read.csv("C:/Users/DELL/Downloads/DS171-main/top_200_password_2020_by_country updated.csv")
filtered_data <- data[data$country %in% c("Australia", "Canada"), ]
# Categorize password strength based on our ranges
filtered_data$password_strength <- cut(filtered_data$Time_to_crack_in_seconds,
                                       breaks = c(-Inf, 60, 3600, 86400, Inf), # time intervals for Weak, Moderate, Strong, Very Strong
                                       labels = c("Weak", "Moderate", "Strong", "Very Strong"))
# Combine 'Strong' and 'Very Strong' into 'Strongest'
filtered_data$password_strength <- ifelse(filtered_data$password_strength %in% c("Strong", "Very Strong"),
                                          "Strongest", 
                                          as.character(filtered_data$password_strength))
# Set desired order of password strength categories
filtered_data$password_strength <- factor(filtered_data$password_strength, 
                                          levels = c("Weak", "Moderate", "Strongest"))
# Remove rows with missing country or Time_to_crack_in_seconds
filtered_data <- filtered_data %>%
  filter(!is.na(Time_to_crack_in_seconds) & !is.na(country))

# Save the filtered and updated dataset to a new file
 write.csv(filtered_data, "filtered_dataset.csv", row.names = FALSE)

   # Filter the dataset for Australia and Canada
   filtered_data <- data[data$country %in% c("Australia", "Canada"), ]
 filtered_data$password_strength <- cut(
  +     filtered_data$Time_to_crack_in_seconds,
  +     breaks = c(-Inf, 60, 3600, 86400, Inf), # Define intervals
  +     labels = c("Weak", "Moderate", "Strong", "Very Strong") # Assign category labels
  + 
    )

# Calculate proportions
library(dplyr)
proportions <- filtered_data %>%
  group_by(country, password_strength) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(country) %>%
  mutate(proportion = count / sum(count))

library(ggplot2)
library(scales)

# Example data
proportions <- data.frame(
  password_strength = rep(c("Weak", "Medium", "Strong"), 2),
  proportion = c(0.3, 0.5, 0.2, 0.4, 0.4, 0.2),
  country = rep(c("Australia", "Canada"), each = 3)
)

# Stacked bar chart
plot <- ggplot(proportions, aes(x = password_strength, y = proportion, fill = country)) +
  geom_bar(stat = "identity", position = "stack") + # Use "stack" for stacked bars
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
            position = position_stack(vjust = 0.5), # Place labels at the middle of each stack
            size = 3.5) +
  labs(title = "Proportion of Password Strength Categories in Australia and Canada",
       x = "Password Strength Category",
       y = "Proportion",
       fill = "Country") +
  scale_fill_manual(values = c("Australia" = "blue", "Canada" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # Center title


# Display plot
print(plot)
# Create a contingency table for Chi-square test
contingency_table <- table(filtered_data$password_strength, filtered_data$country)
print(contingency_table)
# Perform Chi-square test
chi_test <- chisq.test(contingency_table)
print(chi_test)

