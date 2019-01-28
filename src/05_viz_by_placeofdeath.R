library(tidyverse)
library(forcats)

suicide_data <- read_csv('data/suicide_data.csv')

suicide_data %>%
    filter(!is.na(death_place)) %>%
    group_by(death_place, sex) %>%
    tally() %>%
    ungroup() %>%
    mutate(death_place = fct_reorder(death_place, n),
           death_place = fct_recode(death_place, 
                                    'Emergency Room' = 'AMBULATORIO/SALA DE EMERGENCIA',
                                    'Long-term Health Facility' = 'ASILO/CASA DE SALUD/FACILIDAD CIUIDADO PROLONGADO',
                                    'Hospital' = 'HOSPITALIZADO',
                                    'Dead on Arrival' = 'MUERTO AL LLEGAR',
                                    'Other' = 'OTRO',
                                    'Personal Residence' = 'RESIDENCIA DE LA PERSONA FALLECIDA')) %>%
    ggplot(aes(x =death_place, y = n, fill = sex)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    labs(x = 'Place of Death', 
         y = 'Persons', 
         title = 'Where do people die from suicides in Puerto Rico?',
         subtitle = 'Suicides from 2017-2019',
         caption = 'Graph prepared by Ian Flores Siaca') +
    scale_fill_manual(values = c("#fdcdac", "#cbd5e8"), 
                      labels = c("Female", "Male")) +
    theme_minimal()    

ggsave(filename = 'plots/overall_death_place.png', width = 8)
