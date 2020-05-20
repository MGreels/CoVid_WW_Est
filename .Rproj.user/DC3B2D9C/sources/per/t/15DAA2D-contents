####################################
#Edits to the 

library(tidyverse)
library(ggplot2)
library(readxl)

file <- 'raw_data/qnet_monthly.xls'

##Pulls in raw data from excel file and puts in tidy format
raw_towns <- file %>% 
        read_excel(sheet = "QNet") %>%
        pivot_longer("1/2016":'4/2020', 
                     names_to = "date", 
                     values_to = "Q_MGD")
raw_meters <- file %>% 
        read_excel(sheet = "QFinal") %>%
        pivot_longer("1/2016":"4/2020", 
                     names_to = "date", 
                     values_to = "Q_MGD")



#raw_meters <- read_excel(file, sheet = "QFinal")
#raw_towns <- raw_towns %>% gather(:,"date")