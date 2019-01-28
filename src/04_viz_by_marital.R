library(tidyverse)
library(forcats)

suicide_data <- read_csv('data/suicide_data.csv')

suicide_data %>%
    mutate(marital_status = fct_relevel(marital_status, 
                       'NUNCA CASADO(A)', 
                       'CASADO(A)',
                       'CASADO(A), PERO SEPARADO(A)',
                       'DIVORCIADO(A)',
                       'VIUDO(A)',
                       'DESCONOCIDO'),
           marital_status = fct_recode(marital_status, 
                                       'Never Married' = 'NUNCA CASADO(A)',
                                       'Married' = 'CASADO(A)',
                                       'Married, but Separated' = 'CASADO(A), PERO SEPARADO(A)',
                                       'Divorced' = 'DIVORCIADO(A)',
                                       'Widow(er)' = 'VIUDO(A)',
                                       'Unknown' = 'DESCONOCIDO')) %>%
    ggplot(aes(x = marital_status)) +
    geom_bar(fill = 'wheat3') +
    labs(x = 'Marital Status', y = 'Persons',
         title = 'What\'s the marital status of people that commit suicides in Puerto Rico?',
         subtitle = 'Suicides from 2017-2019',
         caption = 'Graph prepared by Ian Flores Siaca') +
    theme_minimal()

ggsave(filename = 'plots/overall_marital.png', width = 8)

    
