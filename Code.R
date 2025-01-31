# Load required libraries
library(ggplot2)
library(gganimate)
library(dplyr)

# Read the synthetic dataset
cotton_data <- read.csv("synthetic_cotton_data.csv")

# Check the first few rows
head(cotton_data)

# Ensure the Year column is numeric
cotton_data$Year <- as.integer(cotton_data$Year)

# Remove any rows with NA values
cotton_data <- cotton_data %>%
  filter(!is.na(Area) & !is.na(Yield))

# Check for any issues
head(cotton_data)


# Create the animated comet plot
comet_plot <- ggplot(cotton_data, aes(x = Area, y = Yield, color = Country, size = Area)) +
  geom_point(alpha = 0.8, stroke = 0.5) +
  geom_line(aes(group = Country), alpha = 0.3) +  # Adds lines between points to make the comet trail
  scale_size_continuous(range = c(2, 8)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Cotton Seed (Unginned) Production Analysis",
       subtitle = "Year: {as.integer(frame_time)}",
       x = "Area Harvested (Million Hectares)",
       y = "Yield (Kg per Hectare)",
       color = "Country",
       size = "Area Harvested") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 12, margin = margin(b = 7)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    panel.grid.major = element_line(color = "gray90", size = 0.2),
    panel.grid.minor = element_line(color = "gray95", size = 0.1),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12, color = "gray30")
  ) +
  transition_time(Year) +
  ease_aes('linear') +
  shadow_wake(wake_length = 0.15, alpha = 0.25) +
  enter_fade() +
  exit_fade()

# Save the animation as a GIF
anim_save("cotton_comet_animation.gif", comet_plot, fps = 20, duration = 10, width = 2500, height = 1900, res = 300)
