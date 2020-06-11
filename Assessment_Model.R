### Building Assessment Model Sheets
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(dplyr)

#####   Sanitary Data -----------------------------------------------------------
# Formats Water scaling factors from COVID data
W_factors <- Water_hist %>%
        filter(year %in% c(2017:2020),
               month %in% "Apr") %>%
        group_by(community, month) %>%
        summarise(factor = Q_MGD[year == 2020]/
                          (mean(Q_MGD[year %in% 2017:2019]))) %>%
        subset(select = c(-month))%>%
        mutate(factor = if_else (is.na(factor),1,factor))

# Pull Sanitary flow averages for towns 
Sani_17_19 <- II_2016_2019 %>%
        filter(meas == m_type[4],
               year >= 2017,
               year <= 2019,
               month %in% month.abb,
               community %in% FS_Sewer_towns )  %>%
        group_by(community, month) %>%
        summarise(Sani_Q = mean(Q_MGD)) %>%
        mutate(year = "17-19",) %>%
        ungroup() 

Sani_16_18 <- II_2016_2019 %>%
        filter(meas == m_type[4],
               year >= 2016,
               year <= 2018,
               month %in% month.abb,
               community %in% FS_Sewer_towns )  %>%
        group_by(community, month) %>%
        summarise(Sani_Q = mean(Q_MGD)) %>%
        mutate(year = "16-18",) %>%
        ungroup()

Sani_19 <- II_2016_2019 %>%
        filter(meas == m_type[4],
               year == 2019,
               month %in% month.abb,
               community %in% FS_Sewer_towns ) %>%
        select(select = -meas) %>%
        rename(Sani_Q = Q_MGD) %>%
        subset(select = -year)

Sani_20W_deltas6 <-II_2016_2019 %>%
        filter(meas == m_type[4],
               year == 2019,
               month %in% month.abb,
               community %in% FS_Sewer_towns ) %>%
        select(select = -meas) %>%
        rename(Sani_Q = Q_MGD) %>%
        subset(select = -year)%>%
        left_join(W_factors, by = c("community"))%>%
        mutate(factor = if_else (is.na(factor),1,factor))%>%
        mutate(factor = if_else(month %in% month.abb[3:8], factor,1))%>%
        mutate(Sani_delta = (factor-1)*Sani_Q) %>%
        subset(select = c(-Sani_Q, -factor))

Sani_20W_deltas2 <-II_2016_2019 %>%
        filter(meas == m_type[4],
               year == 2019,
               month %in% month.abb,
               community %in% FS_Sewer_towns ) %>%
        select(select = -meas) %>%
        rename(Sani_Q = Q_MGD) %>%
        subset(select = -year)%>%
        left_join(W_factors, by = c("community"))%>%
        mutate(factor = if_else (is.na(factor),1,factor))%>%
        mutate(factor = if_else(month %in% month.abb[3:4], factor,1))%>%
        mutate(Sani_delta = (factor-1)*Sani_Q) %>%
        subset(select = c(-Sani_Q, -factor))

#####   FY20 actual -----------------------------------------------------------
mon_16_18 <- II_2016_2019 %>%
        ## Filters raw I/I data for year range, and final flows only
        filter(meas %in% m_type[c(10,1)],
               year >= 2016,
               year <= 2018,
               ## Filters out "ANNUAL" entry
               month %in% month.abb,
               ## Filters out non-community entries
               community %in% FS_Sewer_towns )  %>%
        ## Groups by community and comes up with 3 year monthly means
        group_by(community, month) %>%
        summarise(monthlyav = mean(Q_MGD)) %>%
        mutate(year = "16-18",) %>%
        ungroup() %>%
        ## joins sanitary flows and calculates II only flows
        left_join(Sani_16_18, by = c("community", "month", "year")) %>%
        mutate(av_II_Q = monthlyav - Sani_Q) %>%
        group_by(community) 

ww_FY20_act <- mon_16_18 %>%
        mutate(FY = "FY20",
               desc = "FY20 Actuals",
               actual = T) %>%
        mutate(max_MGD= max(monthlyav),
               max_II_Q = max_MGD - Sani_Q) %>%
        ungroup()

#####   FY21 actual -------------------

mon_17_19 <- II_2016_2019 %>%
        filter(meas %in% m_type[c(10,1)],
               year >= 2017,
               year <= 2019,
               month %in% month.abb,
               community %in% FS_Sewer_towns ) %>%
        group_by(community, month) %>%
        summarise(monthlyav = mean(Q_MGD)) %>%
        mutate(year = "17-19") %>%
        ungroup() %>%
        left_join(Sani_17_19, by = c("community", "month", "year")) %>%
        mutate( av_II_Q = monthlyav - Sani_Q)

ww_FY21_act <- mon_17_19 %>%
        mutate(FY = "FY21",
               desc = "FY21 Actuals",
               actual = T)%>%
        group_by(community) %>%
        mutate(max_MGD= max(monthlyav),
               max_II_Q = max_MGD - Sani_Q) %>%
        ungroup()

#####   FY21 ESTIMATE of 2019 data to compare to actual --------

#FY 21 Estimate uses actual data from 2017 and 2018.  2019 is av of 2016-2018
# Set 2019 Data of Record
rec19 <- mon_16_18 %>%
        mutate(year = 2019) %>%
        subset(select = c(-av_II_Q, -Sani_Q))

ww_FY21_est19 <- Build_Est(2017, 
                           2018, 
                           rec19, 
                           "FY21", 
                           "FY21 w/ 3yr av for CY19")
#####   Baseline Estimate -----------------------------------
##

rec20 <- mon_17_19 %>%
        mutate(year = 2020) %>%
        subset(select = c(-av_II_Q, -Sani_Q))

ww_FY22_est20 <- Build_Est(2018, 
                           2019, 
                           rec20,
                           'FY22',
                           "Baseline FY22 w/ 3yr av for CY20")

#####   Estimate w/ double weighting of CY19 ------------
##      rec20a is a 2020 of record with 2 instances of 2019 included in avg
rec20a <- II_2016_2019 %>%
        filter(meas %in% m_type[c(10,1)],
               year %in% 2017:2019,
               month %in% month.abb,
               community %in% FS_Sewer_towns) %>%
        subset(select = -meas)%>%
        rename(monthlyav = Q_MGD) %>%
        group_by(community, month) %>%
        summarise(monthlyav = (sum(monthlyav)+monthlyav[year == 2019])/4) %>%
        mutate(year = 2020) %>%
        ungroup()

ww_FY22_doub19 <- Build_Est(2018,
                            2019, 
                            rec20a,
                            "FY22",
                            "Option 2 - FY22 w/ dbl 2019")

#####   Estimate 2020 including CoVid Water Rebalancing for 6 months ---------

## Sets 2020 of Record - CoVid Assessment through Various months

rec20W6 <- rec20 %>%
        left_join(Sani_20W_deltas, by = c("community", "month")) %>%
        mutate(monthlyav =  monthlyav + Sani_delta) %>%
        subset(select = c(-Sani_delta))
     
ww_FY22_6mo_water <- Build_Est(2018,
                            2019, 
                            rec20W6,
                            "FY22",
                            "Option 3.2 - FY22 w/ 6 mo.COVID adjustment") %>%
## Estimate Function leaves with Normal Sani_Q, not adjusted based
## on water difference.  This code adds the water scaling factor to the
## tbl, then calculates II and total flow based on reduced sanitary
        left_join(Sani_20W_deltas, by = c("community", "month"))%>%
        mutate(Sani_Q = (3*Sani_Q+Sani_delta)/3) %>%
        mutate(av_II_Q = monthlyav - Sani_Q,
               max_II_Q = monthlyav - Sani_Q) %>%
        subset(select = c(-Sani_delta))

#####   Estimate 2020 including CoVid Water Rebalancing for 2 months ---------

rec20W2 <- rec20 %>%
        left_join(Sani_20W_deltas2, by = c("community", "month")) %>%
        mutate(monthlyav =  monthlyav + Sani_delta) %>%
        subset(select = c(-Sani_delta))

ww_FY22_2mo_water <- Build_Est(2018,
                               2019, 
                               rec20W2,
                               "FY22",
                               "Option 3.1 - FY22 w/ 2 mo.COVID adjustment") %>%
        ## Estimate Function leaves with Normal Sani_Q, not adjusted based
        ## on water difference.  This code adds the water scaling factor to the
        ## tbl, then calculates II and total flow based on reduced sanitary
        left_join(Sani_20W_deltas2, by = c("community", "month"))%>%
        mutate(Sani_Q = (3*Sani_Q+Sani_delta)/3) %>%
        mutate(av_II_Q = monthlyav - Sani_Q,
               max_II_Q = monthlyav - Sani_Q) %>%
        subset(select = c(-Sani_delta))



#####Print Estimates to CSV-----

all_bound <- rbind(ww_FY20_act, 
                ww_FY21_act, 
                ww_FY21_est19,
                ww_FY22_est20, 
                ww_FY22_doub19,
                ww_FY22_6mo_water,
                ww_FY22_2mo_water)

WandS_bound <- filter(all_bound, all_bound$community %in% FS_WandS_towns)

Model_diff <- WandS_bound %>%
        group_by(community, desc) %>%
        summarise(monthlyav = mean(monthlyav),
                  Sani_Q = mean(Sani_Q),
                  av_II_Q = mean(av_II_Q)) %>%
        mutate(diff = (monthlyav - 
                       monthlyav[desc == "Baseline FY22 w/ 3yr av for CY20"])/
                       monthlyav[desc == "Baseline FY22 w/ 3yr av for CY20"])


write_csv(all_bound, "BI_data/Current_Estimates.csv")

write_csv(Model_diff, "BI_data/Model_diffs.csv")
