library(fs)
library(lubridate)
library(tidyverse)
library(ggfortify)

data_files <- dir_ls('data/mortality_data_2000_2008/', type = 'file')

mortality <- map_df(data_files, read_csv, col_types = cols(.default = "c"))


ts_mortality <- mortality %>%
    filter(typedeath == '2') %>%
    select(yeardeath, monthdeath) %>%
    group_by(yeardeath, monthdeath) %>%
    count() %>%
    ungroup() %>%
    mutate(date = ymd(paste(yeardeath, monthdeath, '01', sep = '-'))) %>%
    filter(!is.na(date), date < ymd('2010-01-01')) %>%
    arrange(date) %>%
    select(date, n)

ts_mortality %>%
    ggplot(aes(x = date, y =n)) +
        geom_line()

ts_mortality <- ts(ts_mortality$n, start = c(2000, 1), frequency = 12)

loess_decomp <- stl(ts_mortality, s.window = 'periodic')

loess_decomp %>%
    autoplot(colour = 'brown') +
    theme_light() +
    labs(y = 'Suicide cases',
         x = 'Date',
         title = 'How do suicide cases vary by time in Puerto Rico?',
         subtitle = 'LOESS Decomposition from 2000 to 2008',
         caption = 'Graph prepared by Ian Flores Siaca')
