#/// Gideon Simiyu ///

# Full Amazon Prime Dataset Daily-updated of the Amazon Prime Movies & TV Series Dataset #


# Load required libraries
library(dplyr)
library(tidyr) # Load tidyr for separate_rows function
library(ggplot2)
library(maps)
library(countrycode)

# Load the data (adjust the path to the CSV file as needed)

data <- read.csv("C:/Users/Admin/Desktop/New folder/data.csv")

# Extract and process the available countries
# Extract and process the available countries
country_data <- data %>%
  filter(!is.na(availableCountries)) %>%
  separate_rows(availableCountries, sep = ",\\s*") %>%
  distinct(availableCountries)

# Convert ISO country codes to full country names
country_data$country <- countrycode(country_data$availableCountries, "iso2c", "country.name")

# Merge country data with world map data to ensure alignment
world_map <- map_data("world")
world_map$region <- tolower(world_map$region)
country_data$country <- tolower(country_data$country)
mapped_data <- world_map %>%
  left_join(country_data, by = c("region" = "country"))



# Plot the map with enhanced map elements
ggplot() +
  # World map outline
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray50", size = 0.3) +
  # Highlighted countries
  geom_polygon(data = mapped_data %>% filter(!is.na(availableCountries)), aes(x = long, y = lat, group = group, fill = "Associated Countries"), color = "white", size = 0.3) +
  # Customize the fill for associated countries
  scale_fill_manual(values = c("Associated Countries" = "yellow"), guide = "legend") +
  theme_minimal() +
  # Add labels and titles
  labs(
    title = "Global Map of Movie Availability",
    subtitle = "Countries where movies are available based on provided data",
    caption = "Data Source: Your Movie Dataset",
    x = "Longitude",
    y = "Latitude",
    fill = "Legend"
  ) +
  # Theme customizations
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +

  # Plot the map with enhanced map elements
  ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
  # World map outline
  geom_polygon(fill = "gray90", color = "gray50", size = 0.3) +
  # Highlighted countries
  geom_polygon(data = mapped_data %>% filter(!is.na(availableCountries)),
               aes(fill = "Associated Countries"), color = "white", size = 0.3) +
  # Customize the fill for associated countries
  scale_fill_manual(values = c("Associated Countries" = "dodgerblue"), guide = "legend") +
  theme_minimal() +
  # Add labels and titles
  labs(
    title = "Global Map of Movie Availability for (Movies and TV Series) available on Amazon Prime.",
    subtitle = "Countries where movies are available based on provided data",
    caption = "Data Source: Your Movie Dataset",
    x = "Longitude",
    y = "Latitude",
    fill = "Legend"
  ) +
  # Theme customizations
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  # Adjust map limits (optional)
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-55, 85))






















  
  