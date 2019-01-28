library(tidyverse)
library(forcats)

suicide_data <- read_csv('data/suicide_data.csv')

suicide_data %>%
    group_by(acme_cause_description, sex) %>%
    tally() %>%
    spread(key = 'sex', value = 'n', fill = 0) %>%
    mutate(total = sum(Female + Male)) %>%
    gather(key = 'sex', value = 'persons', -acme_cause_description, - total) %>%
    select(acme_cause_description, sex, persons, total) %>%
    arrange(desc(total)) %>%
    filter(total > 5) %>%
    ungroup() %>%
    mutate(acme_cause_description = str_split(acme_cause_description, "\\(suicide\\) by "),
           acme_cause_description = map_chr(acme_cause_description, ~str_to_title(.x[2])),
           acme_cause_description = fct_reorder(acme_cause_description, persons)) %>%
    ggplot(aes(x = acme_cause_description, y = persons, fill = sex)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    labs(x = 'Cause of Death', 
         y = 'Persons', 
         title = 'How do people commit suicide in Puerto Rico?',
         subtitle = 'Suicides from 2017-2019',
         caption = 'Graph prepared by Ian Flores Siaca') +
    scale_fill_manual(values = c("#fdcdac", "#cbd5e8"), 
                      labels = c("Female", "Male")) +
    theme_minimal()    

ggsave(filename = 'plots/overall_causeofdeath.png', width = 8)
