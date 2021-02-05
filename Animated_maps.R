# Install packages
install.packages(c('tidyr','dplyr','magrittr','tidyverse',
                   'lubridate','gganimate','gifski','png','av','ggrepel'))
# Load packages
library(dplyr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)
library(png)
library(av)
library(ggthemes)
library(ggrepel)
library(ggplot2)
library(scatterpie)


# Read in SAS dataset
data <- haven::read_sas("data.sas7bdat")

# Select First Dose Date for each Subject
clean <- data %>%
  select(SITE, SUBJ, EXDATN) %>%
  mutate(EXDATN = as_date(EXDATN)) %>%
  group_by(SITE, SUBJ) %>%
  summarise(Date = min(EXDATN, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(TimePoint = format(Date, "%Y-%m-%d")) %>%
  select(Site = SITE, TimePoint)

# Create cumulative totals within site
clean2 <- clean %>%
  group_by(Site, TimePoint) %>%
  summarise(Patients = n()) %>%
  ungroup() %>%
  right_join(expand(clean, Site, TimePoint)) %>% # Create record for each TimePoint and Site
  mutate(Patients = replace_na(Patients, 0)) %>%
  group_by(Site) %>%
  mutate(TotalPatients = cumsum(Patients)) %>%
  ungroup()

# Create Ranking
clean3 <- clean2 %>%
  mutate(TotalPatientsAdj = TotalPatients - as.integer(Site)/100000) %>%
  group_by(TimePoint) %>%
  mutate(Rank = min_rank(-(TotalPatientsAdj) * 1),
         RelVal = TotalPatients/max(TotalPatients),
         label = paste0(" ", round(TotalPatients, 0))) %>%
  filter(Rank <= 10)

# Create Plot
anim <- ggplot(clean3, aes(x = Rank, y = RelVal, fill = Site)) +
  geom_col(width = 0.9, position = 'identity') +
  coord_flip() +
  scale_x_reverse() +
  geom_text(aes(x = Rank, y = 0, label = paste0(Site, " ")), hjust = 1) +
  geom_text(aes(x = Rank, y = RelVal, label = label, hjust = 0)) +
  theme_minimal() +
  theme(legend.position = 'none', 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0, size = 22),
        plot.subtitle = element_text(hjust = 0, size = 20)) +
  labs(title = 'Time Lapse of Patient Treatment', 
       subtitle = 'Time Point: {closest_state}',
       x = 'Site',
       y = 'Number of Patients Treated') +
  transition_states(TimePoint, 4, .1) +
  ease_aes('sine-in-out')



# GIF File
animate(anim, nframes = 2500, fps = 50,
        width = 1200, height = 1000, renderer = gifski_renderer("gganim.gif", loop = FALSE))

# Video File
animate(anim, nframes = 500, fps = 20, width = 1200, height = 1000, renderer = av_renderer("gganim.avi"))