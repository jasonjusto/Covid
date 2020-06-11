library("tidyverse")
library("quantmod")
covid <- read.csv("C:/Users/Jason Justo/Downloads/owid-covid-data (2).csv")

# Data can be downloaded at the bottom of https://ourworldindata.org/coronavirus

# Observe and clean
str(covid)
covid$date <- as.Date(covid$date)

covid <- covid %>% 
        mutate(ln_new_cases = log(new_cases))

country <- covid$location != "World" & covid$location != "International"

countries <- covid[country,]

unique(countries$location)

# Focus on top countries in news or other reports. China excluded due to
# falsely reported data.
# United States, Spain, Italy, United Kingdom, France 
tops <- countries$location == "United States" | countries$location == "Spain" |
        countries$location == "Italy" |  countries$location =="United Kingdom" |
        countries$location == "France"        
top <- countries[tops,]

top$location


# GRAPHS

        # New Cases
        ggplot(data = top) + geom_point(mapping = aes(x= top$date, y= top$new_cases, color = location)) +
                facet_wrap(~ location) + xlab("Date") + ylab("New Cases")
        
                # Log of New Cases
                top <- top %>% 
                        mutate(ln_new_cases = log(new_cases))
                
                ggplot(data = top) + geom_point(mapping = aes(x= top$date, y= top$delt_new_cases, color = location)) +
                        facet_wrap(~ location) + xlab("Date") + ylab("Growth Rate of New Cases")
                
                # Growth Rate of New Cases
                top$delt_new_cases <- Delt(top$new_cases)*100
                
                ggplot(data = top) + geom_line(mapping = aes(x= top$date, y= top$delt_new_cases, color = location)) +
                        facet_wrap(~ location) + xlab("Date") + ylab("Growth Rate of New Cases")
        
        # New Deaths
        ggplot(data = top) + geom_point(mapping = aes(x= top$date, y= top$new_deaths, color = location)) +
                facet_wrap(~ location) + xlab("Date") + ylab("New Deaths")
        
        
                # Log of New Deaths
                top <- top %>% 
                        mutate(ln_new_deaths = log(new_deaths))
                
                ggplot(data = top) + geom_point(mapping = aes(x= top$date, y= top$ln_new_deaths, color = location)) +
                        facet_wrap(~ location) + xlab("Date") + ylab("Growth Rate of New Deaths")
                
                # Growth Rate of New Deaths
                top$delt_new_deaths <- Delt(top$new_deaths)*100
                
                ggplot(data = top) + geom_line(mapping = aes(x= top$date, y= top$delt_new_deaths, color = location)) +
                        facet_wrap(~ location) + xlab("Date") + ylab("Growth Rate of New Deaths")
        
        
        # Total deaths
        ggplot(data = top) + geom_point(mapping = aes(x= top$date, y= top$total_deaths, color = location)) +
                facet_wrap(~ location) + xlab("Date") + ylab("Total Deaths")
