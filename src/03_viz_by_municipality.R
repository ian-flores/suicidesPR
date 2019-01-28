## Differences by Sex
library(tidyverse)
library(forcats)

suicide_data <- read_csv('data/suicide_data.csv')

clean_data <- suicide_data %>%
    filter(!is.na(residence_muni)) %>%
    group_by(residence_muni, sex) %>%
    tally() %>%
    spread(key = 'sex', value = 'n', fill = 0) %>%
    mutate(total = sum(Female + Male)) %>%
    gather(key = 'sex', value = 'persons', -residence_muni, - total) %>%
    select(residence_muni, sex, persons, total) %>%
    arrange(desc(total))
    

top_munis <- clean_data %>%
    select(residence_muni, total) %>%
    distinct() %>%
    arrange(desc(total)) %>%
    head(30) %>%
    pull(residence_muni)

avg_ppl <- clean_data %>%
    ungroup() %>%
    summarize(avg_ppl = mean(total)) %>% 
    pull() %>%
    round(2)

clean_data %>%
    filter(residence_muni %in% top_munis) %>%
    ggplot(aes(x = fct_reorder(residence_muni, persons), 
               y = persons, 
               fill = sex)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = avg_ppl, 
               colour = 'brown', 
               lty = 3, 
               lwd = 1.15) +
    annotate('text', 
             label = paste('Average number of suicides per municipality:', 
                           avg_ppl), 
             x = 'HATILLO', 
             y = avg_ppl + 9.2,
             colour = 'brown') +
    coord_flip() +
    labs(y = 'Persons', 
         x = 'Municipality', 
         title = 'Which municipalities have more suicides in Puerto Rico?',
         subtitle = 'Suicides from 2017-2019',
         caption = 'Graph prepared by Ian Flores Siaca') +
    scale_fill_manual(values = c("#fdcdac", "#cbd5e8"), 
                      labels = c("Female", "Male")) +
    theme_minimal()

ggsave(filename = 'plots/overall_municipalities.png', width = 10, height = 8)
