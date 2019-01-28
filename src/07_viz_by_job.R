library(tidyverse)
library(forcats)

suicide_data <- read_csv('data/suicide_data.csv')

suicide_data %>%
    group_by(industry, sex) %>%
    tally() %>%
    spread(key = 'sex', value = 'n', fill = 0) %>%
    mutate(total = sum(Female + Male)) %>%
    gather(key = 'sex', value = 'persons', -industry, - total) %>%
    select(industry, sex, persons, total) %>%
    arrange(desc(total)) %>%
    filter(total > 15, !is.na(industry)) %>%
    ungroup() %>%
    ggplot(aes(x = fct_reorder(industry, persons), y = persons, fill = sex)) +
        geom_bar(stat = 'identity') +
        coord_flip() +
    labs(x = 'Industry of Work', 
         y = 'Persons', 
         title = 'In what do people that commit suicide work in Puerto Rico?',
         subtitle = 'Suicides from 2017-2019',
         caption = 'Graph prepared by Ian Flores Siaca') +
    scale_fill_manual(values = c("#fdcdac", "#cbd5e8"), 
                      labels = c("Female", "Male")) +
    theme_minimal()    
