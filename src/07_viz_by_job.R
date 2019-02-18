library(tidyverse)
library(forcats)

suicide_data <- read_csv('data/suicide_data.csv')

suicide_data %>%
    mutate(industry = fct_recode(industry, 
                                 'Reparations & Maintenance' = '811 - REPARACIÓN Y MANTENIMIENTO',
                                 'Agriculture' = '111 - AGRICULTURA',
                                 'Own home' = '002 - HOGAR PROPIO',
                                 'Justice Services & Public Order' = '922 - SERVICIOS DE JUSTICIA, ORDEN PÚBLICO Y SEGURIDAD',
                                 'Has never Worked' = '004 - NUNCA TRABAJO',
                                 'High School & University Education' = '003 - ESCUELA SUP/UNIVERSIDAD',
                                 'Construcction' = "230 - CONSTRUCCIÓN (23)  CONSTRUCCIÓN",
                                 'Unknown' = '000 - DESCONOCIDO',
                                 'Construcction of Buildings' = '236 - CONSTRUCCIÓN DE EDIFICIOS')) %>%
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

ggsave(filename = 'plots/overall_by_job.png', width = 8)
