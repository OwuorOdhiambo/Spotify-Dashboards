---
title: "Content Preference"
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
library(patchwork)

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

### Chart A

```{r}
top_music_genres <- spotify_data_long %>%
  group_by(fav_music_genre) %>%
  summarise(music_count = n()) %>%
  arrange(desc(music_count)) %>%
  top_n(5, music_count) # Reduced to top 5 for combined plot

# 2. Get the top podcast genres
top_pod_genres <- spotify_data_long %>%
  group_by(fav_pod_genre) %>%
  summarise(pod_count = n()) %>%
  arrange(desc(pod_count)) %>%
  top_n(5, pod_count) # Reduced to top 5 for combined plot

music_plot_bar001 <- ggplot(top_music_genres, aes(x = reorder(fav_music_genre, music_count), y = music_count)) +
  geom_bar(stat = "identity", fill = "steelblue") + # Added stat = "identity"
  coord_flip() +
  labs(
    title = "Top Music Genres", # Shortened title
    x = "Music Genre",
    y = "Number of Users" #Shortened
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 8), # Adjusted text sizes
    axis.title.y = element_text(size = 8),
    title = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

ggplotly(music_plot_bar001)
```

Column {data-width=350}
-----------------------------------------------------------------------

### Chart B

```{r}
# Podcast Genres
pod_plot_bar <- ggplot(top_pod_genres, aes(x = reorder(fav_pod_genre, pod_count), y = pod_count)) +
  geom_bar(stat = "identity", fill = "darkseagreen") + # Added stat = "identity"
  coord_flip() +
  labs(
    title = "Top Podcast Genres", # Shortened title
    x = "Podcast Genre",
    y = "Number of Users" # Shortened
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 8), # Adjusted text sizes
    axis.title.y = element_text(size = 8),
    title = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )
ggplotly(pod_plot_bar)
```

### Chart C

```{r}
mood_genre_counts <- spotify_data_long %>%
  group_by(fav_music_genre, music_Influencial_mood) %>%
  summarise(count = n()) %>%
  ungroup()

mood_genre_heatmap <- ggplot(mood_genre_counts, aes(x = fav_music_genre, y = music_Influencial_mood, fill = count)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  geom_text(aes(label = count), color = "white", size = 2) + # Add counts to cells
  labs(
    title = "Music Mood by Genre (Heatmap)",
    x = "Music Genre",
    y = "Influential Mood",
    fill = "Number of Users"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8))
ggplotly(mood_genre_heatmap)
```
