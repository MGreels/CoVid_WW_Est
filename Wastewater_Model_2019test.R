####################################
# This script performs a test of all meters to find the most appropriate
# meter to scale the total town flow off of each month
# 

library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(dplyr)

## Define location of datafile from Crystal Report
file <- 'raw_data/qnet_monthly.xls'

##Pulls in raw data from excel file and puts in tidy format, 
##      splits to month/date for easy sortin
raw_towns <- file %>% 
        read_excel(sheet = "QNet") %>%
        pivot_longer("1/2016":'4/2020', 
                     names_to = "date", 
                     values_to = "BILLED_MGD") %>%
        mutate(month = month(parse_date_time(date, orders="my")),
               year = year(parse_date_time(date, orders="my")))


# Drop 'QNet" from Town strings to match QFinal table

raw_towns$town <- sub("([A-Za-z]+).*", "\\1", raw_towns$town)

## Pulls in Raw data from excel for each revenue meter and town association
## splits into month/date for easy sorting

raw_meters <- file %>% 
        read_excel(sheet = "QFinal") %>%
        pivot_longer("1/2016":"4/2020", 
                     names_to = "date", 
                     values_to = "Q_MGD")%>%
        mutate(month = month(parse_date_time(date, orders="my")),
               year = year(parse_date_time(date, orders="my"))) 


final_data <- left_join(raw_meters, 
                        raw_towns[, c('town', 'date', 'BILLED_MGD')], 
                        by = c('town', 'date'))

#Test Data for 3 year model
act2019_town <- filter(raw_towns, year == 2019)
act2019_meter <- filter(raw_meters, year == 2019)

## Calculates Estimate ratio based on all data available before 2019 (Average flow by month)
est_model_ratios <- final_data %>%
        mutate(est_ratio = BILLED_MGD/Q_MGD) %>%
        filter(year <= 2018) %>%
        group_by(town, meter, month) %>%
        summarise_each(est_ratio, Q_MGD,funs = list(mean = mean)) %>%
        rename(meter_3Yav = Q_MGD_mean) %>%
        ungroup()

#####   creates estimate table based on model ratios and actual 2019 meter data
#       mutate function adds 2019 estimate based on model ratio
est_2019 <- left_join(
        act2019_meter,
        est_model_ratios[,c("town", 
                            "meter", 
                            "month",
                            "meter_3Yav",
                            "est_ratio_mean")],
                            by = c("town", "meter", "month")) %>%
        mutate(est_2019 = Q_MGD*est_ratio_mean) %>%
        rename(meter_cur = Q_MGD)

#####   creates comparison table based on model estimate
#       mutate function adds ratio of difference (% delta from billed 2019 data)
est_comp_table <- left_join(
        est_2019[,c("town", 
                    "meter", 
                    "date",
                    "meter_3Yav",
                    "meter_cur",
                    "est_2019")],
        act2019_town[, c("town", "date", "BILLED_MGD")],
                by = c("town", "date")) %>%
        mutate(diff_ratio = (est_2019-BILLED_MGD)/BILLED_MGD)

####    Selects Best Meter for each town based on difference
#       

est_all_meter <- est_comp_table %>%
        group_by(town, meter) %>%
        summarise_at(
                c("meter_cur",
                  "est_2019",
                  "BILLED_MGD", 
                  "diff_ratio"), 
                list(mean))
        
# Selectes Best Meter based on total average variance from actual 2019 numbers
est_best_meter <- est_comp_table %>%
        group_by(town, meter) %>%
        summarise_at(
                c("est_2019", 
                  "BILLED_MGD", 
                  "diff_ratio"), 
                mean)%>%
        ungroup() %>%
        group_by(town) %>%
        slice(
                which.min(abs(diff_ratio))) %>%
        ungroup() 

####    Selects worst Month from teh best meter (biggest diff from 2019 data)
#
est_worst_month <- est_comp_table %>%
        group_by(town, meter) %>%
        slice(which.max(abs(diff_ratio))) %>%
        ungroup()

####    Adds worst month to the list
est_best_meter <- left_join(
        est_best_meter,
        est_worst_month[, c("town", "meter", "diff_ratio", "date")],
        by = c("town", "meter")
        ) %>%
        rename(Annual_Ave = diff_ratio.x,
               Worst_month = diff_ratio.y)

#Creates a list of unique town names from the town files to be estimated
townlist <- unique(est_comp_table$town)

#for loop creates individual box plots for each town
for(i in townlist) {
        p <- ggplot(est_comp_table[est_comp_table$town == i,], 
                    aes(x = meter, y = diff_ratio)) +
                geom_boxplot() +
                facet_wrap(~town, scales = "free") +
                ylim(-0.15, 0.15) +
                ylab("Monthly ratio of 2019 Estimate to Actual by meter") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
# save plots as .pdf
        ggsave(p,
               file=paste('plots/',
                          i,
                          '_box',
                          ".pdf",
                          sep=''),
               scale=2)
###Print Plots to Console
#        print(p)
}


