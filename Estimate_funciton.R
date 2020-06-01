###### FUnction that builds estimate values from 2 initial years real data
###     and and estimate of the 3rd.  Ensures 


Build_Est <- function(year_1, 
                      year_2, 
                      year_3est, 
                      FY = "Undefined",
                      desc = "Undefined")
        {
        
        II_2016_2019 %>%
                #Filter out only final WW measurements
                filter(meas %in% m_type[c(10,1)],
                       year >= year_1,
                       year <= year_2,
                       month %in% month.abb,
                       community %in% FS_Sewer_towns) %>%
                #Drop Measurement Name and rename Q_MGD
                subset(select = -meas) %>%
                rename(monthlyav = Q_MGD) %>%
                #Add the 2020 estimate of record
                rbind(year_3est)%>%
                #group tbl by community and month
                group_by(community, month) %>%
                #Summarize the flow values to an average for months, add year range
                summarise(monthlyav = mean(monthlyav)) %>%
                mutate(year = "18-20") %>%
                ungroup() %>%
                #Add Sanitary flows over the same months and calculate teh I-I Flow
                left_join(Sani_19, by = c("community", "month")) %>%
                mutate( II_Q = monthlyav - Sani_Q)%>%
                # #Group by community and create a community monthly estimate
                # group_by(community) %>%
                # summarise(Sani_Q = mean(Sani_Q),
                #           II_Q = mean(II_Q),
                #           Assessment = mean(monthlyav),
                #           max_month = month[monthlyav == max(monthlyav)],
                #           max_MGD = max(monthlyav)) %>%
                # #Tag this Estimate with FY and Estimate identifiers
                mutate(FY = FY,
                       desc = desc,
                       actual = F)
}
        