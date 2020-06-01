####################################
# This script organizes and Cleans data for the I-I analysis from 2016 through 2020
# ggplot graphs created on subsequent script.
# Use files 
# 2016-2019_ii_calcs.xls From Carl Leone, annual II reports
# daily_water_2015_2020.xls From Telog
# water_cust_types
# qnet_monthly
 

library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(dplyr)

##Pulls in raw data from excel file and puts in tidy format, 
##      splits to month/date for easy sortin

cols <- c('guess', 'skip','skip', rep(c('numeric', 'skip', 'skip'), 63))
Water_hist <- read_excel('raw_data/daily_water_2015_2020.xls',
                           col_types = cols) %>%
        pivot_longer(2:64,
                     names_to = "community",
                     values_to = "Q_mgd") %>%
        na.omit()

Water_hist$Date <- mdy_hm(Water_hist$Date)
Water_hist$community <- sub("([A-Za-z]+).*", "\\1", 
                       Water_hist$community)

Water_hist <- Water_hist %>%
        mutate(month = month.abb[month(Date)], 
               year = year(Date))%>%
        group_by(community, month, year) %>%
        summarise(Q_MGD=mean(Q_mgd))%>%
        mutate(meas = "Daily Water Demand") %>%
        ungroup() %>%
        na.omit()

## Bring in Carls II Numbers, iteratively searches by years 2016-2019
cols <- c('text', 'text', rep('numeric', 13))
for(i in 2016:2019){
        
looptbl <- paste('raw_data/', as.character(i), '_ii_calcs.xls', sep = '') %>%
        read_excel(sheet = "Table4",
                   range = "A2:O729",
                   skip = 1,
                   col_types = cols) %>%
        fill("Community", .direction = "down")%>%
        na.omit() %>%
        rename('ANNUAL' = '(MGD)',
               'meas' = 'Flow Characteristic',
               'community' = 'Community') %>%
        pivot_longer('Jan':'ANNUAL',
                     names_to = "month",
                     values_to = "Q_MGD") %>%
        mutate(year = i)

assign(paste('II_', i, sep = ''), looptbl)

}


####Bind all years data

II_2016_2019 <- bind_rows(II_2016, II_2017, II_2018, II_2019, Water_hist)
rm(cols, i, looptbl)
rm(II_2016, II_2017, II_2018, II_2019)

##Normalizing data names "Boston (Total)" to "Boston"
II_2016_2019$meas[II_2016_2019$meas == "Final Estimated Infiltration"] <- 
        "Estimated Infiltration"
II_2016_2019$community[II_2016_2019$community == "Boston (Total)"] <- 
        "Boston"
II_2016_2019$community[II_2016_2019$community == "Brookline (Total)"] <- 
        "Brookline"
II_2016_2019$community[II_2016_2019$community == "Milton (Total)"] <- 
        "Milton"
II_2016_2019$community[II_2016_2019$community == "Newton (Total)"] <- 
        "Newton"

###Creates list of WW Communities

WW_towns <- 'raw_data/qnet_monthly.xls' %>%
        read_excel(sheet = "QNet",
                   range = "A1:A55")
W_Towns <- 'raw_data/water_cust_types.xlsx' %>%
        read_excel()


FS_Sewer_towns <- unique(sub("([A-Za-z]+).*", "\\1", WW_towns$town))
FS_Water_towns <- W_Towns$community[W_Towns$cust_type == "F"]
FS_WandS_towns <- intersect(FS_Sewer_towns,FS_Water_towns)

m_type <- unique(II_2016_2019$meas)
#winter <- month.abb[c(1,2,3,12)]


write_csv(FS_WandS, "BI_data/II_Water.csv")
          
rm(WW_towns, W_Towns)
rm(Water_hist)
rm(winter)





