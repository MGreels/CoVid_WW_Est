######  Script for graph creation for I-I analysis.  
### Need to run I-I_Estimate.R



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
        geom_bar(stat = "summary", fun = mean, position = position_dodge())

# p_total <- II_Base + geom_bar(stat = "summary", fun = mean, position = "stack")
# p_norm <- II_Base + geom_bar(stat = "summary", fun = mean, position = "fill")
# 
# p_facet <- p_norm + facet_wrap(~year)
