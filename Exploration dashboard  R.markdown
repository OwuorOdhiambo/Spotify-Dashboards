---
title: "Spotify Exploration dashboard"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---

```{r setup, include=FALSE}
library(flexdashboard)
library(knitr)
library(ggplot2)
library(plotly)
library(dplyr)
library(highcharter)
library(readxl)
library(tidyverse)
library(viridis)

Spotify_data <- read_excel("Spotify_data.xlsx") # Assuming your Excel file is named "Spotify_data.xlsx"
sd <- Spotify_data

spotify_data_long <- sd %>%
  separate_rows(spotify_listening_device, sep = ", ")

spotify_data_long <- spotify_data_long %>%
  separate_rows(spotify_listening_device,sep = ", ")
spotify_data_long <- spotify_data_long %>%
  separate_rows(music_Influencial_mood,sep = ", ")
spotify_data_long <- spotify_data_long %>%
  separate_rows(music_lis_frequency,sep = ", ")
spotify_data_long <- spotify_data_long %>%
  separate_rows(music_expl_method,sep = ", ")
```

Column {data-width=650}
-----------------------------------------------------------------------

### Chart A: Exploration chart

```{r}
Music_exploration_chart <- ggplot(spotify_data_long, aes(x = music_expl_method)) +
  geom_bar(fill = "steelblue") + # You can change the fill color if you like
  labs(
    title = "Distribution of Music Exploration Methods",
    x = "Music Exploration Method",
    y = "Number of Users"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(Music_exploration_chart)
```

Column {data-width=350}
-----------------------------------------------------------------------

### Chart B:Exploration effectiveness

```{r}
##likert scale
# Assuming your data frame is 'spotify_data_long' and the rating column is 'music_recc_rating'

# 1. Ensure the rating column is treated as an ordered factor
spotify_data_long$music_recc_rating <- ordered(spotify_data_long$music_recc_rating, levels = 1:5)

# 2. Count the occurrences of each rating
rating_counts <- spotify_data_long %>%
  group_by(music_recc_rating) %>%
  summarise(count = n()) %>%
  ungroup()

# 3. Calculate percentages for each rating
rating_percentages <- rating_counts %>%
  mutate(
    percentage = count / sum(count),
    percentage_label = paste0(round(percentage * 100), "%") # Create percentage labels
  )

# Define Likert scale labels
likert_labels <- c("1\nNot Effective", "2", "3", "4", "5\nMost Effective")

# 4. Create the Likert bar chart
music_exploration_effectiveness <- ggplot(rating_percentages, aes(x = music_recc_rating, y = percentage, fill = music_recc_rating)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = percentage_label),
    position = position_stack(vjust = 0.5), # Adjust label position
    size = 3,
    color = "black"
  ) +
  scale_x_discrete(labels = likert_labels) + # Use the Likert scale labels
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Format y-axis as percentages
  scale_fill_manual(values = c("#D73027", "#F46D43", "#FDAE61", "#ABD9E9", "#2171B5"), guide = "none") + # Color scale
  labs(
    title = "Effectiveness Rating of Music Recommendations",
    x = "Rating",
    y = "Percentage of Users"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),  # Adjust x-axis label size
    axis.title.x = element_text(size = 12), # Adjust x-axis title size
    axis.text.y = element_text(size = 10),  # Adjust y-axis label size
    axis.title.y = element_text(size = 12), # Adjust y-axis title size
    title = element_text(size = 14)       # Adjust title size
  )
ggplotly(music_exploration_effectiveness)

```
