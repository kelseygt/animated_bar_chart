# loading packages
library(gifski)
library(ggplot2)
library(gganimate)
library(lubridate)
library(tidyr)
library(dplyr)
library(showtext)
library(viridis)
library(ggtext)
font_add_google(name = "Rajdhani", family = "Rajdhani")
showtext_auto()
windowsFonts("Rajdhani" = windowsFont("Rajdhani"))

# loading in data
majors <- read.csv(file.choose(), header = T)
head(majors)

# formatting data (with college info)
majors_formatted_wc <- majors %>%
  filter(Major.Desc != "Undeclared" & Major.Desc != "Non-Degree Seeking") %>%
  group_by(Year, College.Desc) %>%
  mutate(Rank = rank(-Count, ties.method = "first")) %>%
  filter(Rank <= 10)
head(majors_formatted_wc)

# creating list of colleges for loop
colleges <- as.list(unique(majors_formatted_wc$College.Code))
colleges <- colleges[colleges != "AA" & colleges != "0" & colleges != "SH"]

# creating function for plotting and exporting animation
racing_bar_chart <- function(data_set) {
  p <- ggplot(data_set, aes(Rank, group = Major.Desc, fill = Count, color = Count)) +
    theme_minimal() +
    theme(text = element_text(family = "Rajdhani"), 
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(fill = "seashell", color = "seashell"),
          panel.background = element_rect(fill = "seashell", color = "seashell"),
          axis.text.x = element_text(color = "gray16")) +
    geom_tile(aes(y = Count/2, height = Count, width = 0.9), 
              alpha = 0.8, 
              color = NA) +
    geom_text(aes(y = Count, label = paste("n =", as.character(Count))), 
              family = "Rajdhani", 
              hjust = -0.2, 
              size = 3.5, 
              color = "gray16") +
    geom_text(aes(y = -25, label = Major.Desc), 
              family = "Rajdhani", 
              hjust = 1,
              color = "gray16") +
    coord_flip(clip = "off", expand = FALSE) +
    scale_y_continuous(limits = c(-25, max(data$Count)+100), 
                       breaks = seq(0, max(data$Count)+100, by = 250), 
                       labels = scales::comma) +
    scale_x_reverse() +
    guides(color = "none", fill = "none") +
    labs(title="A Decade of Undergraduate Majors", 
         subtitle = paste(unique(data_set$College.Desc), "\nFall Semesters, 2012 - 2022 | Year: {closest_state}", sep = ""), 
         x = "", 
         y = "Headcount by Major") +
    theme(plot.title = element_text(size = 18, face = "bold", margin = margin(b = 10)),
          plot.subtitle = element_text(size = 10, margin = margin(b = 10), lineheight = 1.5),
          axis.ticks.y = element_blank(),  
          axis.text.y  = element_blank(), 
          plot.margin = margin(1, 1, 1, 7, "cm")) +
    transition_states(Year, transition_length = 4, state_length = 2, wrap = F) +
    scale_fill_viridis(direction = -1) +
    enter_fade() +
    exit_fade() +
    ease_aes('cubic-in-out')
  # animating and saving
  animate(p, fps = 15, duration = 40, width = 600, height = 400, end_pause = 60)
  anim_save(paste("majors_over_time_", college, ".gif", sep = ""))
}

# looping
setwd("C:/Users/Laserbeams/Desktop")
for (college in colleges) {
  data <- majors_formatted_wc %>% filter(College.Code == college)
  racing_bar_chart(data)
}
  