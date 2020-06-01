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
        group_by(community) %>%
        summarise(Sani_Q = mean(Sani_Q),
                  II_Q = mean(II_Q),
                  Assessment = mean(monthlyav),
                  max_month = month[monthlyav == max(monthlyav)],
                  max_MGD = max(monthlyav)) %>%
        mutate(FY = "FY20",
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
        group_by(community) %>%
        summarise(Sani_Q = mean(Sani_Q),
                  II_Q = mean(II_Q),
                  Assessment = mean(monthlyav),
                  max_month = month[monthlyav == max(monthlyav)],
                  max_MGD = max(monthlyav)) %>%
        mutate(FY = "FY21",
               actual = T)



###### ESTIMATES #######

#FY 21 Estimate uses actual data from 2017 and 2018.  2019 is av of 2016-2018

# Set 2019 Data of Record
rec19 <- mon_16_18 %>%
        mutate(year = 2019) %>%
        subset(select = c(-II_Q, -Sani_Q))

ww_FY21_est19 <- II_2016_2019 %>%
        filter(meas %in% m_type[c(10,1)],
               year >= 2017,
               year <= 2018,
               month %in% month.abb,
               community %in% FS_Sewer_towns) %>%
        subset(select = -meas) %>%
        rename(monthlyav = Q_MGD) %>%
        rbind (rec19) %>%
        group_by(community, month) %>%
        summarise(monthlyav = mean(monthlyav)) %>%
        mutate(year = "17-19") %>%
        ungroup() %>%
        left_join(Sani_17_19, by = c("community", "month", "year")) %>%
        mutate( II_Q = monthlyav - Sani_Q)%>%
        group_by(community) %>%
        summarise(Sani_Q = mean(Sani_Q),
                  II_Q = mean(II_Q),
                  Assessment = mean(monthlyav),
                  max_month = month[monthlyav == max(monthlyav)],
                  max_MGD = max(monthlyav)) %>%
        mutate(FY = "FY21",
               actual = F)

# #Baseline Estimate

rec20 <- mon_17_19 %>%
        mutate(year = 2020) %>%
        subset(select = c(-II_Q, -Sani_Q))

ww_FY22_est20 <- II_2016_2019 %>%
        #Filter out only final WW measurements
        filter(meas %in% m_type[c(10,1)],
               year >= 2018,
               year <= 2019,
               month %in% month.abb,
               community %in% FS_Sewer_towns) %>%
        #Drop Measurement Name and rename Q_MGD
        subset(select = -meas) %>%
        rename(monthlyav = Q_MGD) %>%
        #Add the 2020 estimate of record
        rbind(rec20)%>%
        #group tbl by community and month
        group_by(community, month) %>%
        #Summarize the flow values to an average for months, add year range
        summarise(monthlyav = mean(monthlyav)) %>%
        mutate(year = "18-20") %>%
        ungroup() %>%
        #Add Sanitary flows over the same months and calculate teh I-I Flow
        left_join(Sani_19, by = c("community", "month")) %>%
        mutate( II_Q = monthlyav - Sani_Q)%>%
        #Group by community and create a community monthly estimate
        group_by(community) %>%
        summarise(Sani_Q = mean(Sani_Q),
                  II_Q = mean(II_Q),
                  Assessment = mean(monthlyav),
                  max_month = month[monthlyav == max(monthlyav)],
                  max_MGD = max(monthlyav)) %>%
        #Tag this Estimate with FY and Estimate identifiers
        mutate(FY = "FY22",
               CY20_meth = "3yr ave (2017-19)",
               actual = F)%>%
        ungroup()

#Estimate w/ double weighting of CY19

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

ww_FY22_doub19 <- II_2016_2019 %>%
        #Filter out only final WW measurements
        filter(meas %in% m_type[c(10,1)],
               year >= 2018,
               year <= 2019,
               month %in% month.abb,
               community %in% FS_Sewer_towns) %>%
        #Drop Measurement Name and rename Q_MGD
        subset(select = -meas) %>%
        rename(monthlyav = Q_MGD) %>%
        #Add the 2020 estimate of record
        rbind(rec20a)%>%
        #group tbl by community and month
        group_by(community, month) %>%
        #Summarize the flow values to an average for months, add year range
        summarise(monthlyav = mean(monthlyav)) %>%
        mutate(year = "18-20") %>%
        ungroup() %>%
        #Add Sanitary flows over the same months and calculate teh I-I Flow
        left_join(Sani_19, by = c("community", "month")) %>%
        mutate( II_Q = monthlyav - Sani_Q)%>%
        #Group by community and create a community monthly estimate
        group_by(community) %>%
        summarise(Sani_Q = mean(Sani_Q),
                  II_Q = mean(II_Q),
                  Assessment = mean(monthlyav),
                  max_month = month[monthlyav == max(monthlyav)],
                  max_MGD = max(monthlyav)) %>%
        #Tag this Estimate with FY and Estimate identifiers
        mutate(FY = "FY22",
               CY20_meth = "3yr ave (2017-19)",
               actual = F)

#####Print Estimates to CSV

write_csv(rbind(ww_FY20_act, 
                ww_FY21_act, 
                ww_FY21_est19), 
          "BI_data/2019tests.csv")

write_csv(rbind(ww_FY22_est20, 
                ww_FY22_doub19), 
          "BI_data/2019Estimates.csv")