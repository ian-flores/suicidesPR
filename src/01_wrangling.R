library(tidyverse)

mortality_data <- read_csv('data/regdem-defunciones-01-enero-2017-hasta-enero-18-2019.csv')

suicide_data <- mortality_data %>%
    filter(TypeOfDeath == 'SUICIDIO') %>%
    select(-contains('DeathCause'), 
           -contains('BirthPlace'), 
           -contains('Facility'),
           -contains('Funeral'),
           -contains('?'),
           -contains('TimeOfResidence'),
           -contains('Injury'),
           -'AgeUnit', 
           -'Citizenship',
           -'TypeOfDeath') %>%
    separate(col = 'ResidencePlace', into = c('Country', 'residence_muni'), sep = ', ', fill = 'warn') %>%
    select(-Country) %>%
    separate(col = 'MunicipalityDeathPlace', into = c('Country', 'death_muni'), sep = ', ', fill = 'warn') %>%
    select(-Country) %>%
    rename(sex = 'Gender',
           age = 'Age',
           birth_year = 'BirthDate_Year',
           marital_status = 'MaritalStatus',
           death_place = 'DeathPlace',
           death_year = 'DeathDate_Year',
           death_month = 'DeathDate_Month',
           death_hour = 'DeathTimeHour',
           death_minute = 'DeathTimeMinutes',
           death_time_ind = 'DeathTimeAMPM',
           education = 'Education',
           occupation = 'Occupation',
           industry = 'Industry',
           years_working = 'OccupationYears',
           acme_cause_code = 'AcmeUnderlyingCauseCode',
           acme_cause_description = 'ACMEUnderlyingCauseDescription') %>%
    mutate(age = as.numeric(age),
           sex = case_when(sex == 'M' ~ 'Male',
                           sex == 'F' ~ 'Female'),
           industry = str_conv(industry, 'ISO-8859-1'),
           education = str_conv(education, 'ISO-8859-1'),
           occupation = str_conv(occupation, 'ISO-8859-1'))


write_csv(suicide_data, 'data/suicide_data.csv')

