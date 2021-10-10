library(gifski)
library(ggplot2)
library(gganimate)
library(lubridate)
library(tidyr)
library(dplyr)

majors <- read.csv(file.choose(), header = T)
head(majors)

majors_formatted <- majors %>%
  filter(Major.Desc != "Undeclared" & Major.Desc != "Non-Degree Seeking") %>%
  group_by(Year) %>%
  mutate(Rank = rank(-Count, ties.method = "first")) %>%
  filter(Rank <= 20)
head(majors_formatted)

p <- ggplot(majors_formatted, aes(Rank, group = Major.Desc, fill = as.factor(Major.Desc), color = as.factor(Major.Desc))) +
  geom_tile(aes(y = Count/2, height = Count, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = Count, label = as.character(Count)), hjust = -0.2, size = 3, color = "gray45") +
  geom_text(aes(y = 0, label = paste(Major.Desc, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 500), labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title="Top 20 Majors by Headcount in {closest_state}", subtitle = "Fall Semesters 2000 - 2020", x = "", y = "Headcount by Major") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  
        axis.text.y  = element_blank(), 
        plot.margin = margin(1, 1, 1, 6, "cm")) +
  transition_states(Year, transition_length = 4, state_length = 2, wrap = F) +
  enter_fade() +
  exit_fade() +
  ease_aes('cubic-in-out')

animate(p, fps = 15, duration = 40, width = 800, height = 600, end_pause = 60)

anim_save("majors_over_time.gif")
