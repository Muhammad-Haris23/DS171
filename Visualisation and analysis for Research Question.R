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

# Enhanced bar plot with labels and custom colors
plot <- ggplot(proportions, aes(x = password_strength, y = proportion, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(proportion)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5) + # Add text labels for proportions above bars
  labs(title = "Proportion of Password Strength Categories in Australia and Canada",
       x = "Password Strength Category",
       y = "Proportion") +
  scale_fill_manual(values = c("Australia" = "blue", "Canada" = "red")) + # Custom colors
  theme_minimal()

# Display plot
print(plot)
# Create a contingency table for Chi-square test
contingency_table <- table(filtered_data$password_strength, filtered_data$country)
print(contingency_table)
# Perform Chi-square test
chi_test <- chisq.test(contingency_table)
print(chi_test)

