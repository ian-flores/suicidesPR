## Differences by Sex
library(tidyverse)
library(ggridges)

suicide_data <- read_csv('data/suicide_data.csv')

female_avg_age <- suicide_data %>%
    filter(sex == 'Female') %>%
    summarize(mean_age = mean(age)) %>%
    pull()

male_avg_age <- suicide_data %>%
    filter(sex == 'Male') %>%
    summarize(mean_age = mean(age)) %>%
    pull()

age_lines <- data.frame(Sex = c('Female', 'Male'),
                        x0 = c(female_avg_age, male_avg_age))

suicide_data %>%
    ggplot() +
    geom_density_ridges(aes(x = age, y = sex, fill = sex), 
                        jittered_points = TRUE,
                        scale = 0.75,
                        point_shape = '?',
                        point_size = 1,
                        alpha = 0.65,
                        rel_min_height = 0.01) +
    geom_segment(data = age_lines, 
                 aes(x = x0, 
                     xend = x0, 
                     y = as.numeric(Sex), 
                     yend = as.numeric(Sex) + 0.65), colour = 'brown', lty = 3, lwd = 1.15) +
    geom_text(data = suicide_data %>%
                  group_by(sex) %>%
                  mutate(avg_age = round(mean(age), 1)) %>%
                  select(sex, avg_age) %>%
                  distinct(),
              aes(x = avg_age,
                  y = sex,
                  label = paste('Average age for', str_to_lower(sex), ':',avg_age)),
              position = position_nudge(y = -0.1),
              size = 4.25) +
    scale_fill_manual(values = c("#fdcdac", "#cbd5e8"), 
                      labels = c("Female", "Male")) +
    labs(x = 'Age', 
         y = 'Sex',
         title = 'How are suicides distributed by age and sex in Puerto Rico?',
         subtitle = 'Suicides from 2017-2019',
         caption = 'Graph prepared by Ian Flores Siaca') +
    guides(fill = FALSE) +
    theme_ridges(grid = FALSE, center = TRUE)

ggsave(filename = 'plots/overall_sex.png', width = 8)
