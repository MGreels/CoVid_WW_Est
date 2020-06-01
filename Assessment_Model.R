### Building Assessment Model Sheets

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



mon_16_18 <- II_2016_2019 %>%
        filter(meas %in% m_type[c(10,1)],
               year >= 2016,
               year <= 2018,
               month %in% month.abb,
               community %in% FS_Sewer_towns )  %>%
        group_by(community, month) %>%
        summarise(monthlyav = mean(Q_MGD)) %>%
        mutate(year = "16-18",) %>%
        ungroup() %>%
        left_join(Sani_16_18, by = c("community", "month", "year")) %>%
        mutate( II_Q = monthlyav - Sani_Q)

ww_FY20_act <- mon_16_18 %>%
        # group_by(community) %>%
        # summarise(Sani_Q = mean(Sani_Q),
        #           II_Q = mean(II_Q),
        #           Assessment = mean(monthlyav),
        #           max_month = month[monthlyav == max(monthlyav)],
        #           max_MGD = max(monthlyav)) %>%
        mutate(FY = "FY20",
               desc = "2019 Only",
               actual = T)

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
        left_join(Sani_16_18, by = c("community", "month", "year")) %>%
        mutate( II_Q = monthlyav - Sani_Q)

ww_FY21_act <- mon_17_19 %>%
        # group_by(community) %>%
        # summarise(Sani_Q = mean(Sani_Q),
        #           II_Q = mean(II_Q),
        #           Assessment = mean(monthlyav),
        #           max_month = month[monthlyav == max(monthlyav)],
        #           max_MGD = max(monthlyav)) %>%
        mutate(FY = "FY21",
               desc = "2019 Only",
               actual = T)



###### ESTIMATES #######

#FY 21 Estimate uses actual data from 2017 and 2018.  2019 is av of 2016-2018

# Set 2019 Data of Record
rec19 <- mon_16_18 %>%
        mutate(year = 2019) %>%
        subset(select = c(-II_Q, -Sani_Q))

ww_FY21_est19 <- Build_Est(2017, 
                           2018, 
                           rec19, 
                           "FY21", 
                           "FY21 w/ 3yr est for CY 2019")

# #Baseline Estimate

rec20 <- mon_17_19 %>%
        mutate(year = 2020) %>%
        subset(select = c(-II_Q, -Sani_Q))

ww_FY22_est20 <- Build_Est(2018, 
                           2019, 
                           rec20,
                           'FY22',
                           "Est of FY22 w/ dbl 2019")

#####   Estimate w/ double weighting of CY19
##      rec20a is a 2020 of record with 2 instances of 2019 included in avg
rec20a <- II_2016_2019 %>%
        filter(meas %in% m_type[c(10,1)],
               year == 2019,
               month %in% month.abb,
               community %in% FS_Sewer_towns) %>%
        subset(select = -meas)%>%
        rename(monthlyav = Q_MGD) %>%
        rbind(rec20) %>%
        rbind(rec20) %>%
        rbind(rec20) %>%
        group_by(community, month) %>%
        summarise(monthlyav = mean(monthlyav)) %>%
        mutate(year = 2020) %>%
        ungroup()

ww_FY22_doub19 <- Build_Est(2018,
                            2019, 
                            rec20a,
                            "FY22",
                            "Est of FY22 w/ dbl 2019")

#######Estimate 2020 including CoVid Water reallocation

W_factors <- Water_hist %>%
        filter(year %in% c(2017:2020),
               month %in% "Apr") %>%
        group_by(community, month) %>%
        summarise(factor = Q_MGD[year == 2020]/
                          (mean(Q_MGD[year %in% 2017:2019]))) %>%
        subset(select = c(-month))%>%
        mutate(factor = if_else (is.na(factor),1,factor))


rec20W <- rec20 %>%
        left_join(Sani_19, by = c("community", "month")) %>%
        left_join(W_factors, by = c("community"))%>%
        mutate(factor = if_else (is.na(factor),1,factor))%>%
        mutate(monthlyav = monthlyav - (Sani_Q*(1-factor))) %>%
        subset(select = c(-factor, -Sani_Q))
     
ww_FY22_water <- Build_Est(2018,
                            2019, 
                            rec20W,
                            "FY22",
                            "Est of FY22 w/ water adjustment")   
        
#####Print Estimates to CSV

write_csv(rbind(ww_FY20_act, 
                ww_FY21_act, 
                ww_FY21_est19,
                ww_FY22_est20, 
                ww_FY22_doub19,
                ww_FY22_water), 
          "BI_data/Current_Estimates.csv")

# write_csv(rbind(ww_FY22_est20, 
#                 ww_FY22_doub19), 
#           "BI_data/FY22_Estimates.csv")