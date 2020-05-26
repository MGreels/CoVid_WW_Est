####################################
# This script performs a test of all meters to find the most appropriate
# meter to scale the total town flow off of each month
# 

library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(dplyr)
## Define location of datafile from Crystal Report
file <- 'raw_data/qnet_monthly.xls'

##Pulls in raw data from excel file and puts in tidy format, 
##      splits to month/date for easy sortin
raw_towns <- file %>% 
        read_excel(sheet = "QNet") %>%
        pivot_longer("1/2016":'4/2020', 
                     names_to = "date", 
                     values_to = "TOT_MGD") %>%
        mutate(month = month(parse_date_time(date, orders="my")),
               year = year(parse_date_time(date, orders="my")))

# Drop 'QNet" from Town strings to match QFinal table

raw_towns$town <- sub("([A-Za-z]+).*", "\\1", raw_towns$town)

## Pulls in Raw data from excel for each revenue meter and town association
## splits into month/date for easy sorting

raw_meters <- file %>% 
        read_excel(sheet = "QFinal") %>%
        pivot_longer("1/2016":"4/2020", 
                     names_to = "date", 
                     values_to = "Q_MGD")%>%
        mutate(month = month(parse_date_time(date, orders="my")),
               year = year(parse_date_time(date, orders="my"))) 

townlist <- unique(raw_towns$town)


final_data <- left_join(raw_meters, 
                        raw_towns[, c('town', 'date', 'TOT_MGD')], 
                        by = c('town', 'date'))

final_data <- final_data %>%
                mutate(est_ratio = TOT_MGD/Q_MGD)

### tibble search example
### raw_towns$Q_MGD[raw_towns$Town == "Arlington" & raw_towns$date == "1/2018"]

#raw_meters <- read_excel(file, sheet = "QFinal")
#raw_towns <- raw_towns %>% gather(:,"date")