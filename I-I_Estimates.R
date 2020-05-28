####################################
# This script Examines the past 5 years monthly 
# 

library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(dplyr)


##Pulls in raw data from excel file and puts in tidy format, 
##      splits to month/date for easy sortin
cols <- c('text', 'text', rep('numeric', 13))
for(i in 2019){
looptbl <- paste('raw_data/', as.character(i), '_ii_calcs.xls', sep = '') %>% 
        read_excel(sheet = "Table4",
                   range = "A2:O729",
                   skip = 1,
                   col_types = cols) %>%
        fill("Community", .direction = "down")%>%
        na.omit()

assign(paste('II_', i, sep = ''), looptbl)
rm(looptbl)
}
        # pivot_longer("1/2016":'4/2020', 
        #              names_to = "date", 
        #              values_to = "BILLED_MGD") %>%
        # mutate(month = month(parse_date_time(date, orders="my")),
        #        year = year(parse_date_time(date, orders="my")))