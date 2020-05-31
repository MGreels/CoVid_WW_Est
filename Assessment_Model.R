### Building Assessment Model Sheets

ww_FY20_monthly <- II_2016_2019 %>%
        filter(meas %in% m_type[c(10,1)],
               year >= 2016,
               year <= 2018,
               month %in% month.abb,
               community %in% FS_Sewer_towns )  %>%
        group_by(community, month) %>%
        summarise(monthlyav = mean(Q_MGD)) %>%
        mutate(year = "17-19") %>%
        ungroup()

ww_FY20 <- ww_FY20_monthly%>%
        group_by(community) %>%
        summarise(Assessment = mean(monthlyav),
                  max_month = month[monthlyav == max(monthlyav)],
                  max_MGD = max(monthlyav))

ww_FY21_monthly <- II_2016_2019 %>%
        filter(meas %in% m_type[c(10,1)],
               year >= 2017,
               year <= 2019,
               month %in% month.abb,
               community %in% FS_Sewer_towns ) %>%
        group_by(community, month) %>%
        summarise(monthlyav = mean(Q_MGD)) %>%
        mutate(year = "17-19") %>%
        ungroup()

ww_FY21 <- ww_FY21_monthly %>%
        group_by(community) %>%
        summarise(Assessment = mean(monthlyav),
                  max_month = month[monthlyav == max(monthlyav)],
                  max_MGD = max(monthlyav))

WW_FY21_est19 <- II_2016_2019 %>%
        filter(meas %in% m_type[c(10,1)],
               year >= 2017,
               year <= 2018,
               month %in% month.abb,
               community %in% FS_Sewer_towns) %>%
        subset(select = -meas) %>%
        rename(monthlyav = Q_MGD) %>%
        rbind(ww_FY20_monthly)%>%
        group_by(community) %>%
        summarise(Assessment = mean(monthlyav),
                  max_month = month[monthlyav == max(monthlyav)],
                  max_MGD = max(monthlyav)) %>%
        mutate(av_share = Assessment/sum(Assessment),
               max_share = max_MGD/sum(max_MGD))




        