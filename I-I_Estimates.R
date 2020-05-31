####################################
# This script Examines the past 4 years monthly 
 

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

## Bring in Carls  
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

###Creates list of WW Communities

townlist <- 'raw_data/qnet_monthly.xls' %>%
        read_excel(sheet = "QNet",
                   range = "A1:A55")

townlist$town <- sub("([A-Za-z]+).*", "\\1", townlist$town)


##Normalizing data names (Boston Total)
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

measurements <- unique(II_2016_2019$meas)
townlist <- unique(townlist$town)
winter <- month.abb[c(1,2,3,12)]






#########Graphs!##########################

II_Base <- II_2016_2019[II_2016_2019$meas %in% measurements[c(12, 3:5)] &
                                II_2016_2019$community %in% townlist,] %>%
        group_by(community, month) %>%
        ggplot(aes(fill = meas, x = community, y = Q_MGD)) +
        theme(axis.text.x = element_text(angle = 90, vjust = .25, hjust = 1))

Water_Comp <- II_2016_2019[II_2016_2019$meas %in% measurements[c(4,13)] &
                                   II_2016_2019$community %in% townlist &
                                   II_2016_2019$month %in% winter,] %>%
        group_by(community,year) %>%
        ggplot(aes(x = month, y = Q_MGD, fill = meas))

P_Water <- Water_Comp +
        geom_bar(stat = "summary", fun = mean, position = position_dodge()) +
        facet_wrap(~community)
                                   


# p_total <- II_Base + geom_bar(stat = "summary", fun = mean, position = "stack")
# p_norm <- II_Base + geom_bar(stat = "summary", fun = mean, position = "fill")
# 
# p_facet <- p_norm + facet_wrap(~year)
