install.packages("ggpubr")
library("tidyverse")
library("quantmod")
library("ggplot2")
library("ggpubr")

# San Diego County Data comes from https://sdgis-sandag.opendata.arcgis.com/datasets/covid-19-statistics-san-diego-county
covid_sd <- read.csv("C:/Users/Jason Justo/Downloads/COVID_19_Statistics_San_Diego_County (3).csv")
str(covid_sd)


# date is in year, month, date and hours, minutes, seconds
# so change date into character to run string split
covid_sd$date <- as.character(covid_sd$date)
# apply strsplit to each element in vector date AND keep only first element in element list 
covid_sd$date <- sapply(strsplit(covid_sd$date," "), `[`, 1)

lubridate::ymd(covid_sd$date)
covid_sd$date <- as.Date(covid_sd$date)
# New Cases
        ggplot(data = covid_sd) + geom_line(mapping = aes(x= covid_sd$date,
        y= covid_sd$newcases)) + xlab("Date") + ylab("New Cases") +
        scale_x_date(date_breaks = "months" , date_labels = "%b-%y")


# Growth Rate of New Cases
covid_sd$delt_newcases <- Delt(covid_sd$newcases)*100
        
new_case_grow <- ggplot(data = covid_sd) + geom_line(mapping = aes(x= covid_sd$date,
        y= covid_sd$delt_newcases)) + xlab("Date") + ylab("Growth Rate of New Cases") +
        scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

tail(covid_sd$delt_newcases, n = 14)        

# Growth Rate of New Tests
covid_sd$delt_newtests <- Delt(covid_sd$newtests)*100

new_test_grow <- ggplot(data = covid_sd) + geom_line(mapping = aes(x= covid_sd$date,
        y= covid_sd$delt_newtests)) + xlab("Date") + ylab("Growth Rate of New Tests") +
        scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

# Growth Rate of Deaths
covid_sd$delt_deaths <- Delt(covid_sd$deaths)*100

new_death_grow <- ggplot(data = covid_sd) + geom_line(mapping = aes(x= covid_sd$date,
        y= covid_sd$delt_deaths)) + xlab("Date") + ylab("Growth Rate of Deaths") +
        scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

ggarrange(new_case_grow,new_test_grow, new_death_grow)


# Conditions to begin Phase 3:

        #1.) 25 new cases per 100,000 in past 14 days
        # Take the rate of new cases per 100,000 condition and apply to SD population of 3,338,000
        # according to quick google search:
        .00025*3338000
        # 834.5
        # Currently we are at:
        phase3_cond1 <- sum(tail(covid_sd$newcases, n = 14))   
        phase3_cond1
        # 1926
        
        # OR
        #2.) Less than 8% testing positive in past 7 days
        # San Diego population: 3.1 million
        # Take rate of positives over totals test and check past 7 days
        less_8 <- covid_sd$positives/covid_sd$tests
        phase3_cond2 <- tail(less_8, n = 7)
        phase3_cond2

        # average positives from tests.
        x<-na.omit(less_8)
        sum(x)/length(x)
        max(x)
        
