install.packages("tidyverse")
install.packages("quantmod")
library("tidyverse")
library("quantmod")
covid <- read.csv("C:/Users/Jason Justo/Downloads/owid-covid-data.csv")

# Data can be downloaded at the bottom of https://ourworldindata.org/coronavirus

# Observe and clean
str(covid)
covid$date <- as.Date(covid$date)

covid <- covid %>% 
        mutate(ln_new_cases = log(new_cases))

country <- covid$location != "World" & covid$location != "International"

countries <- covid[country,]

unique(countries$location)

countries %>%
        group_by(location) %>% 
        arrange(desc(total_cases)) %>% 
        distinct(location)
        

# Focus on top "5" countries with highest total_cases.
# United States, India, Brazil, France, Russia 
tops <- countries$location == "United States" | countries$location == "India" |
        countries$location == "Brazil" |  countries$location =="France" |
        countries$location == "Russia"        

# GRAPHS

        # New Cases
        png('new_cases.png')
        
        ggplot(data = top) + geom_point(mapping = aes(x= date, y= new_cases, color = location)) +
                facet_wrap(~ location) + xlab("Date") + ylab("New Cases")

        dev.off()
        
                # Log of New Cases
                top <- top %>% 
                        mutate(ln_new_cases = log(new_cases))
                
                png('ln_new_cases.png')
                
                ggplot(data = top) + geom_point(mapping = aes(x= date, y= ln_new_cases, color = location)) +
                        facet_wrap(~ location) + xlab("Date") + ylab("Ln(New Cases)")
                
                dev.off()
                
        # Growth Rate of Total Cases
                top$delt_total_cases <- Delt(top$total_cases)*100
                
                png('delt_total_cases.png')
                
                ggplot(data = top) + geom_line(mapping = aes(x= date, y= delt_total_cases, color = location)) +
                        facet_wrap(~ location) + xlab("Date") + ylab("Growth Rate of Total Cases")
                
                dev.off()
                
                
        # New Deaths
        png('new_deaths.png')        
        
        ggplot(data = top) + geom_point(mapping = aes(x= date, y= new_deaths, color = location)) +
                facet_wrap(~ location) + xlab("Date") + ylab("New Deaths")
        
        dev.off()
        
                # Log of New Deaths
                top <- top %>% 
                        mutate(ln_new_deaths = log(new_deaths))
                
                png('ln_new_deaths.png')
                
                ggplot(data = top) + geom_point(mapping = aes(x= date, y= ln_new_deaths, color = location)) +
                        facet_wrap(~ location) + xlab("Date") + ylab("Ln(New Deaths)")
                
                dev.off()
                        
        # Total deaths
        png('total_deaths.png')        
                
        ggplot(data = top) + geom_point(mapping = aes(x= date, y= total_deaths, color = location)) +
                facet_wrap(~ location) + xlab("Date") + ylab("Total Deaths")

        dev.off()
        
        # Growth Rate of Total Deaths
        top$delt_total_deaths <- Delt(top$total_deaths)*100
        
        png('delt_total_deaths.png')
        
        ggplot(data = top) + geom_line(mapping = aes(x= date, y= delt_total_deaths, color = location)) +
                facet_wrap(~ location) + xlab("Date") + ylab("Growth Rate of Total Deaths")
        
        dev.off()
        
